#' @title Query stormwater solids
#'
#' @description The functions included here (query_solids_default and
#'   query_solids_site_date) facilitate querying stormwater AFDM data from the
#'   database. The default query (visible on the samples tab upon open) queries
#'   the 50 most recently entered samples while the query_samples_site_date()
#'   function returns samples data filtered by site and a date range.

query_solids_default <- function() {
  
  baseQuery <- '
  SELECT
    solids.solid_id,
    samples.sample_id,
    sites.abbreviation AS site,
    samples.sample_datetime::TEXT,
    samples.bottle,
    samples.afdm_bottle_id,
    solids.filter_initial,
    solids.filter_dry,
    solids.volume_filtered,
    solids.filter_ashed,
    solids.replicate,
    solids.comments
  FROM stormwater.samples
  JOIN stormwater.sites ON (samples.site_id = sites.site_id)
  LEFT JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
  ORDER BY samples.sample_id DESC
  LIMIT 50;'
  
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery)
  
  run_interpolated_query(parameterizedQuery)
  
}


query_solids_site_date <- function(start, end, site) {
  
  baseQuery <- '
  SELECT
    solids.solid_id,
    samples.sample_id AS id,
    sites.abbreviation AS site,
    samples.sample_datetime::TEXT,
    samples.bottle,
    samples.afdm_bottle_id,
    solids.filter_initial,
    solids.filter_dry,
    solids.volume_filtered,
    solids.filter_ashed,
    solids.replicate,
    solids.comments
  FROM stormwater.samples
  JOIN stormwater.sites ON (samples.site_id = sites.site_id)
  LEFT JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
  WHERE
    sites.abbreviation = ?studySite AND
    (samples.sample_datetime BETWEEN ?startDate AND ?endDate)
  ORDER BY
    samples.sample_datetime;'
  
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery,
                                       startDate = start,
                                       endDate = end,
                                       studySite = site)
  
  run_interpolated_query(parameterizedQuery)
  
}
