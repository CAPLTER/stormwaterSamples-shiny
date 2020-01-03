#' @title Query stormwater samples
#'
#' @description The functions included here ( query_samples_default and
#'   query_samples_site_date) facilitate querying stormwater data from the
#'   database. The default query (visible on the samples tab upon open) queries
#'   the 50 most recently entered samples while the query_samples_site_date()
#'   function returns samples data filtered by site and a date range.
#'   
query_samples_default <- function() {
  
  baseQuery <- '
  SELECT
    samples.sample_id AS id,
    -- samples.site_id,
  	sites.abbreviation AS site,
    samples.sample_datetime::TEXT,
    samples.comments,
    samples.lab_temperature AS temp,
    samples."lab_pH" AS pH,
    samples.lab_conductance AS cond,
    samples.bottle,
    samples.doc_vial_id,
    samples.afdm_bottle_id
	FROM stormwater.samples
	JOIN stormwater.sites ON (samples.site_id = sites.site_id)
  ORDER BY samples.sample_id DESC
  LIMIT 50;'
  
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery)
  
  run_interpolated_query(parameterizedQuery)
  
}


query_samples_site_date <- function(start, end, site) {
  
  baseQuery <- '
  SELECT
    samples.sample_id AS id,
    -- samples.site_id,
  	sites.abbreviation AS site,
    samples.sample_datetime::TEXT,
    samples.comments,
    samples.lab_temperature AS temp,
    samples."lab_pH" AS pH,
    samples.lab_conductance AS cond,
    samples.bottle,
    samples.doc_vial_id,
    samples.afdm_bottle_id
	FROM stormwater.samples
	JOIN stormwater.sites ON (samples.site_id = sites.site_id)
  WHERE
    sites.abbreviation = ?studySite AND
    (sample_datetime BETWEEN ?startDate AND ?endDate)
  ORDER BY
    sample_datetime;'
  
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery,
                                       startDate = start,
                                       endDate = end,
                                       studySite = site)
  
  run_interpolated_query(parameterizedQuery)
  
}
