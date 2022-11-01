#' @title Helper function to validate that all chemistry samples have an ID and
#' that all IDs are unique
#'
#' @description The function \code{check_sample_ids} tests whether all of the
#' samples in a mahcine output file have been associated with a sample ID, and
#' that there are not any duplicates among those IDs. Returns FALSE if all
#' tests have passed, or a message indicating which test failed.
#'
#' @export

check_sample_ids <- function(data_to_validate) { 

  data_subset <- data_to_validate |>
  dplyr::mutate(
    newSample = replace(newSample, newSample == "NULL", NA),
    samples   = dplyr::case_when(
      !is.na(newSample) ~ newSample,
      TRUE ~ samples
    )
    ) |>
  dplyr::filter(omit == FALSE)

  validation_message <- c()

  # check if any samples not flagged to omit are missing a sample ID
  if (any(is.na(data_subset[, "samples"]))) { 

    validation_message <- append(
      x      = validation_message,
      values = "at least one sample missing sample ID or flag to omit"
    )

  }


  # check for duplicate combinations of: sample ID x replicate x analyte
  # discounting samples flagged for omit
  if (anyDuplicated(data_subset[, c("samples", "replicate", "analyte_name")])) {

    validation_message <- append(
      x      = validation_message,
      values = "at least one duplicate sample ID x replicate x omit"
    )

  }

  return(validation_message)

}
