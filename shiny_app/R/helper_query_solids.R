#' @title Query stormwater solids
#'
#' @description The functions included here (query_solids and
#' query_single_solid) facilitate querying stormwater AFDM data from the
#' database. `query_solids` queries all solids and associated data within a
#' period of the current date to three year(s) prior. `query_single_solid`
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
    -- solids.replicate,
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


query_single_solid <- function(solid_id) {

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
    -- solids.replicate,
    solids.comments
  FROM stormwater.samples
  JOIN stormwater.sites ON (samples.site_id = sites.site_id)
  JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
  WHERE samples.solid_id = { solid_id }
  ",
  .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}


# query_solids_default <- function() {
#   
#   baseQuery <- '
#   SELECT
#     solids.solid_id,
#     samples.sample_id,
#     sites.abbreviation AS site,
#     samples.sample_datetime::TEXT,
#     samples.bottle,
#     samples.afdm_bottle_id,
#     solids.filter_initial,
#     solids.filter_dry,
#     solids.volume_filtered,
#     solids.filter_ashed,
#     solids.replicate,
#     solids.comments
#   FROM stormwater.samples
#   JOIN stormwater.sites ON (samples.site_id = sites.site_id)
#   LEFT JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
#   ORDER BY samples.sample_id DESC
#   LIMIT 50;'
#   
#   parameterizedQuery <- sqlInterpolate(ANSI(),
#                                        baseQuery)
#   
#   run_interpolated_query(parameterizedQuery)
#   
# }


# query_solids_site_date <- function(start, end, site) {
#   
#   baseQuery <- '
#   SELECT
#     solids.solid_id,
#     samples.sample_id AS id,
#     sites.abbreviation AS site,
#     samples.sample_datetime::TEXT,
#     samples.bottle,
#     samples.afdm_bottle_id,
#     solids.filter_initial,
#     solids.filter_dry,
#     solids.volume_filtered,
#     solids.filter_ashed,
#     solids.replicate,
#     solids.comments
#   FROM stormwater.samples
#   JOIN stormwater.sites ON (samples.site_id = sites.site_id)
#   LEFT JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
#   WHERE
#     sites.abbreviation = ?studySite AND
#     (samples.sample_datetime BETWEEN ?startDate AND ?endDate)
#   ORDER BY
#     samples.sample_datetime;'
#   
#   parameterizedQuery <- sqlInterpolate(ANSI(),
#                                        baseQuery,
#                                        startDate = start,
#                                        endDate = end,
#                                        studySite = site)
#   
#   run_interpolated_query(parameterizedQuery)
#   
# }
