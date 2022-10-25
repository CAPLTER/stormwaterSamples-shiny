#' @title Helper functions to process downloaded ISCO data
#'
#' @description Helper functions to process and extract data from files
#' downloaded from ISCOs.

#' @note determine from the downloaded file the site ID at which the ISCO was
#' located
#' @export
get_site_id <- function(isco_file) {

  site_id <- readr::read_csv(
    file      = isco_file$datapath,
    n_max     = 1,
    col_names = c("reportText", "siteID")
    ) |>
  dplyr::pull(siteID)

  return(site_id)

}


#' @note determine from the downloaded file the max sample data
#' @export
get_max_date <- function(isco_file) {

  maxDate <- readr::read_csv(
    file      = isco_file$datapath,
    skip      = 7,
    col_names = c("sample_datetime", "eventNumber"),
    locale    = readr::locale(tz = "America/Phoenix")
    ) |>
  dplyr::select(sample_datetime) |>
  dplyr::filter(!is.na(sample_datetime)) |>
  dplyr::mutate(
    sample_datetime = lubridate::parse_date_time(sample_datetime, c("mdY HMS p", "mdY HMS", "mdY HM"))
    ) |>
  dplyr::summarise(maxDTTM = max(sample_datetime))

  # because this function is to add a blank, set the time according to
  # prescribed approach for blank times (e.g., blank #2 = 00:00:20)
  # moved to observe event input$storm
  lubridate::hour(maxDate$maxDTTM)   = 0
  lubridate::minute(maxDate$maxDTTM) = 0
  lubridate::second(maxDate$maxDTTM) = 10

  return(maxDate$maxDTTM)

}

#' @note generate a sample report (collection times, mostly) from the
#' downloaded file
#' @export
generate_report <- function(isco_file, carousel = "1") {

  site_id  <- get_site_id(isco_file)
  max_date <- get_max_date(isco_file)

  isco_report <- readr::read_csv(
    file      = isco_file$datapath,
    skip      = 7,
    col_names = c("sample_datetime", "eventNumber"),
    locale    = readr::locale(tz = "America/Phoenix")
    ) |>
  dplyr::mutate(bottle = paste0(site_id, "_", carousel, "_", eventNumber)) |>
  dplyr::select(bottle, sample_datetime) |>
  dplyr::filter(!is.na(sample_datetime)) |>
  dplyr::mutate(sample_datetime = lubridate::parse_date_time(sample_datetime, c("mdY HMS p", "mdY HMS", "mdY HM"))) |>
  tibble::add_row(
    bottle          = paste0(site_id, "_", carousel, "_BLK"),
    sample_datetime = max_date
    ) |>
  dplyr::mutate(
    notes = NA_character_
  )

  return(isco_report)

}


#' @note harvest level (bubbler) data from the downloaded file
#' @export
harvest_level_data <- function(isco_file) {

  site_id  <- get_site_id(isco_file)

  level_data <- readr::read_csv(
    file      = isco_file$datapath,
    skip      = 7,
    col_names = c("event_datetime", "level"),
    locale    = readr::locale(tz = "America/Phoenix")
    ) |>
  dplyr::filter(!is.na(event_datetime)) |>
  dplyr::mutate(
    event_datetime = lubridate::parse_date_time(event_datetime, c("mdY HMS p", "mdY HMS")),
    event_datetime = format(event_datetime, "%Y-%m-%d %H:%M:%S"),
    site_id        = as.integer(site_id),
    source_file    = isco_file$name
    ) |>
  dplyr::select(
    site_id,
    event_datetime,
    water_height = level,
    source_file
  )

  return(level_data)

}
