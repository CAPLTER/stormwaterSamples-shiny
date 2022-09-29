#' @title Stormwater sampling location details from the database
#'
#' @description Makes available the list of site abbreviations for all current
#'   and recent sampling sites.

query_sample_sites <- function() {

  baseQuery <- "
  SELECT
    site_id,
    abbreviation
  FROM stormwater.sites
  WHERE abbreviation IN ('IBW', 'LM', 'SGC', 'Ave7th', 'centralNorth', 'centralSouth', 'Price');"

  sampleSites <- run_interpolated_query(baseQuery)

  return(sampleSites)

}

sampleSites <- query_sample_sites()

siteAbbreviations <- sampleSites |>
  dplyr::arrange(abbreviation) |>
  dplyr::pull(abbreviation)
