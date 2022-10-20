#' @title helper: add a new or update existing solid record
#'
#' @description Function to add a new or update existing record of
#' stormwater.solids

insert_new_or_update_solid <- function(
  filter_initial,
  filter_dry,
  volume_filtered,
  filter_ashed,
  comments,
  query_type,
  sample_id = NULL,
  solid_id  = NULL
  ) {

  filter_initial  <- as.numeric(filter_initial)
  filter_dry      <- as.numeric(filter_dry)
  volume_filtered <- as.numeric(volume_filtered)
  filter_ashed    <- as.numeric(filter_ashed)
  comments        <- gsub("[\r\n]", "; ", comments)
  comments        <- gsub(",", ";", comments)
  comments        <- ifelse(comments == "", "NA", comments)

  if (query_type == "insert") {

    parameterized_query <- glue::glue_sql("
      INSERT INTO stormwater.solids
      (
        sample_id,
        filter_initial,
        filter_dry,
        volume_filtered,
        filter_ashed,
        replicate,
        \"comments\"
      )
      VALUES(
        { sample_id },
        { filter_initial },
        { filter_dry },
        { volume_filtered },
        { filter_ashed },
        1,
        NULLIF({ comments }, 'NA')::text
      )
      ;
      ",
      .con = DBI::ANSI()
    )

    run_interpolated_execution(parameterized_query)
    # print(parameterized_query)

  } else {

    parameterized_query <- glue::glue_sql("
      UPDATE stormwater.solids
      SET
        filter_initial = { filter_initial },
        filter_dry = { filter_dry },
        volume_filtered = { volume_filtered },
        filter_ashed = { filter_ashed },
        \"comments\" = NULLIF({ comments }, 'NA')::text
      WHERE
      solid_id = { solid_id }
      ;
      ",
      .con = DBI::ANSI()
    )

    run_interpolated_execution(parameterized_query)
    # print(parameterized_query)

  }

}
