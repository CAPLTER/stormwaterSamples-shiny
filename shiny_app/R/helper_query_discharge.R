#' @title Query stormwater discharge
#'
#' @description The functions included here (query_discharge_default and
#' query_discharge_site_date) facilitate querying stormwater discharge data
#' from the database. The default query (visible on the samples tab upon open)
#' queries the most recently uploaded sample set while the
#' query_discharge_site_date() function returns samples data filtered by site
#' and a date range.

query_discharge_default <- function() {
  
  baseQuery <- '
  SELECT
    -- discharge.discharge_id,
    -- discharge.site_id,
    sites.abbreviation AS site,
    discharge.event_datetime,
    discharge.water_height,
    discharge.discharge
    -- discharge.discharge_corrected
  FROM stormwater.discharge
  JOIN stormwater.sites ON (sites.site_id = discharge.site_id)
  WHERE
  discharge.run_id IN (
    SELECT MAX(discharge.run_id)
    FROM stormwater.discharge
  )
  ORDER BY discharge.event_datetime ASC ;
  '
  
  parameterizedQuery <- sqlInterpolate(
    ANSI(),
    baseQuery
  )
  
  run_interpolated_query(parameterizedQuery)
  
}


query_discharge_site_date <- function(start, end, site) {
  
  baseQuery <- '
  SELECT
    -- discharge.discharge_id,
    -- discharge.site_id,
    sites.abbreviation AS site,
    discharge.event_datetime,
    discharge.water_height,
    discharge.discharge
    -- discharge.discharge_corrected
  FROM stormwater.discharge
  JOIN stormwater.sites ON (sites.site_id = discharge.site_id)
  WHERE 
    sites.abbreviation = ?studySite AND
    discharge.event_datetime BETWEEN ?startDate AND ?endDate
  ORDER BY
    discharge.event_datetime;'
  
  parameterizedQuery <- sqlInterpolate(
    ANSI(),
    baseQuery,
    studySite = site,
    startDate = start,
    endDate = end
  )
  
  run_interpolated_query(parameterizedQuery)
  
}
