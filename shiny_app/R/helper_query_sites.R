#' @title Stormwater sampling location details from the database
#'
#' @description Makes available the list of site abbreviations for all current
#'   and recent sampling sites.

query_sample_sites <- function() {

  base_query <- "
  SELECT
    site_id,
    abbreviation
  FROM stormwater.sites
  WHERE abbreviation IN ('IBW', 'LM', 'SGC')
  ;
  "

  sample_sites <- run_interpolated_query(base_query)

  return(sample_sites)

}

sample_sites <- query_sample_sites()

siteAbbreviations <- sample_sites |>
  dplyr::arrange(abbreviation) |>
  dplyr::pull(abbreviation)
