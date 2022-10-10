#' @title helper: query stormwater sample data
#'
#' @description The functions included here (query_samples and
#' query_single_sample) facilitate querying stormwater sample data from the
#' database. `query_samples` queries all sample and associated data within a
#' period of the current date to three years prior. `query_single_sample`
#' queries a single sample and associated data given a sample id.
#'
#' @export
#'

query_samples <- function() {

  parameterized_query <- glue::glue_sql("
    SELECT
      samples.sample_id AS id,
      sites.abbreviation AS site,
      samples.sample_datetime::TEXT AS date_time,
      samples.comments,
      samples.lab_temperature AS temp,
      samples.\"lab_pH\" AS pH,
      samples.lab_conductance AS cond,
      samples.bottle --,
      -- samples.doc_vial_id,
      -- samples.afdm_bottle_id
    FROM stormwater.samples
    JOIN stormwater.sites ON (samples.site_id = sites.site_id)
    WHERE samples.sample_datetime >= { Sys.Date() - lubridate::years(3) }
    ORDER BY samples.sample_id DESC
    ;
    ",
  .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}


query_single_sample <- function(sample_id) {

  parameterized_query <- glue::glue_sql("
    SELECT
      samples.sample_id AS id,
      sites.abbreviation AS site,
      samples.sample_datetime::TEXT AS date_time,
      samples.comments,
      samples.lab_temperature AS temp,
      samples.\"lab_pH\" AS pH,
      samples.lab_conductance AS cond,
      samples.bottle --,
      -- samples.doc_vial_id,
      -- samples.afdm_bottle_id
    FROM stormwater.samples
    JOIN stormwater.sites ON (samples.site_id = sites.site_id)
    WHERE samples.sample_id = { sample_id }
    ;
    ",
  .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}

# query_samples_default <- function() {
#   
#   baseQuery <- '
#   SELECT
#     samples.sample_id AS id,
#     -- samples.site_id,
#   	sites.abbreviation AS site,
#     samples.sample_datetime::TEXT,
#     samples.comments,
#     samples.lab_temperature AS temp,
#     samples."lab_pH" AS pH,
#     samples.lab_conductance AS cond,
#     samples.bottle,
#     samples.doc_vial_id,
#     samples.afdm_bottle_id
# 	FROM stormwater.samples
# 	JOIN stormwater.sites ON (samples.site_id = sites.site_id)
#   ORDER BY samples.sample_id DESC
#   LIMIT 50;'
#   
#   parameterizedQuery <- sqlInterpolate(ANSI(),
#                                        baseQuery)
#   
#   run_interpolated_query(parameterizedQuery)
#   
# }
# 
# 
# query_samples_site_date <- function(start, end, site) {
#   
#   baseQuery <- '
#   SELECT
#     samples.sample_id AS id,
#     -- samples.site_id,
#   	sites.abbreviation AS site,
#     samples.sample_datetime::TEXT,
#     samples.comments,
#     samples.lab_temperature AS temp,
#     samples."lab_pH" AS pH,
#     samples.lab_conductance AS cond,
#     samples.bottle,
#     samples.doc_vial_id,
#     samples.afdm_bottle_id
# 	FROM stormwater.samples
# 	JOIN stormwater.sites ON (samples.site_id = sites.site_id)
#   WHERE
#     sites.abbreviation = ?studySite AND
#     (sample_datetime BETWEEN ?startDate AND ?endDate)
#   ORDER BY
#     sample_datetime;'
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
