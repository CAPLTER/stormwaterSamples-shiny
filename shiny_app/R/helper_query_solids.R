#' @title Query stormwater solids
#'
#' @description The functions included here (query_solids and
#' query_solid_solid_id) facilitate querying stormwater AFDM data from the
#' database. `query_solids` queries all solids and associated data within a
#' period of the current date to three year(s) prior. `query_solid_solid_id`
#' queries a single solid record and associated data given a solid id.
#'
#' @export
#'

query_solids <- function() {

  parameterized_query <- glue::glue_sql("
  SELECT
    solids.solid_id AS id,
    samples.sample_id,
    sites.abbreviation AS site,
    samples.sample_datetime::TEXT AS date_time,
    samples.bottle,
    solids.filter_initial,
    solids.filter_dry,
    solids.volume_filtered,
    solids.filter_ashed,
    solids.comments
  FROM stormwater.samples
  JOIN stormwater.sites ON (samples.site_id = sites.site_id)
  LEFT JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
  WHERE samples.sample_datetime >= { Sys.Date() - lubridate::years(3) }
  ORDER BY samples.sample_id DESC
  ;
  ",
  .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}

#'
#' @note individual solid by solid_id
#'
#' @export
#'
query_solid_solid_id <- function(solid_id) {

  if (grepl("na", solid_id, ignore.case = TRUE)) {

    null_tibble <- tibble::tibble(
      id              = NA_integer_,
      filter_initial  = 999,
      filter_dry      = 999,
      volume_filtered = 999,
      filter_ashed    = 999,
      comments        = "error: must create record before editing"
    )

    return(null_tibble)

  } else {

    parameterized_query <- glue::glue_sql("
      SELECT
        solids.solid_id AS id,
        solids.filter_initial,
        solids.filter_dry,
        solids.volume_filtered,
        solids.filter_ashed,
        solids.comments
      FROM stormwater.solids
      WHERE solids.solid_id = { solid_id }
      ;
      ",
      .con = DBI::ANSI()
    )

    run_interpolated_query(parameterized_query)

  }

}

#'
#' @note individual solid by sample_id
#'
#' @export
#'
query_solid_sample_id <- function(sample_id) {

  parameterized_query <- glue::glue_sql("
    SELECT
      solids.solid_id AS id
    FROM stormwater.solids
    WHERE solids.sample_id = { sample_id }
    ;
    ",
    .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}
