#' @title helper: add a new or update existing sample record
#'
#' @description Function to add a new or update existing record of
#' stormwater.samples

insert_new_or_update_sample <- function(
  site,
  date,
  time,
  comments,
  temperature,
  conductance,
  bottle,
  survey_type,
  sample_id = NULL
  ) {


  # adjust seconds based on sample type and carousel
  time <- as.POSIXct(
    x      = time,
    tz     = "America/Phoenix",
    format = "%Y-%m-%d %H:%M:%S"
  )

  if (grepl("blk", bottle, ignore.case = TRUE)) {

    carousel <- as.integer(stringr::str_match(bottle, "(?:_)(\\d{1,2})(?:_)")[2])
    lubridate::second(time) = carousel * 10

  } else {

    lubridate::second(time) = 0

  }

  site        <- glue::glue_sql("{sample_sites[sample_sites$abbreviation == site,]$site_id*}")
  time        <- strftime(time, "%T")
  comments    <- gsub("[\r\n]", "; ", comments)
  comments    <- gsub(",", ";", comments)
  comments    <- ifelse(comments == "", "NA", comments)
  temperature <- as.numeric(temperature)
  conductance <- as.numeric(conductance)
  datetime    <- paste0(date, " ", time)

  if (survey_type == "insert") {

    parameterized_query <- glue::glue_sql("
      INSERT INTO stormwater.samples
      (
        site_id,
        sample_datetime,
        \"comments\",
        lab_temperature,
        lab_conductance,
        bottle
      )
      VALUES(
        { site },
        { datetime },
        NULLIF({ comments }, 'NA')::text,
        { temperature },
        { conductance },
        { bottle }
      )
      ;
      ",
      .con = DBI::ANSI()
    )

    run_interpolated_execution(parameterized_query)
    # print(parameterized_query)

  } else {

    parameterized_query <- glue::glue_sql("
      UPDATE stormwater.samples
      SET
        site_id = { site },
        sample_datetime = { datetime },
        \"comments\" = NULLIF({ comments }, 'NA')::text,
        lab_temperature = { temperature },
        lab_conductance = { conductance },
        bottle = { bottle }
      WHERE
       sample_id = { sample_id }
      ;
      ",
      .con = DBI::ANSI()
    )

    run_interpolated_execution(parameterized_query)
    # print(parameterized_query)

  }

}
