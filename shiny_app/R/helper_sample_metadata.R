#' @title Join bottle IDs in raw machine data (imported from file) to bottle
#' IDs in a picklist of samples identified by the user.
#'
#' @description The function \code{join_sample_metadata} attempts to match
#' bottle IDs in raw machine data (imported from file) with bottle IDs in a
#' list of samples identified by the user based on a range of sites and storms
#' (dates). Where matches are identified, the sample details are recorded as a
#' record (bottle ID + DT) in the samples column of the resulting data object.
#' However, joins cannot be ambiguous. This occurs when there is more than one
#' possible match among the bottle IDs in raw machine data and the bottle IDs
#' among those samples selected by the user for matching. This most often
#' occurs when there are data from multiple storms in close succession. For
#' example, if there are two bottles with the ID 11.1.11 in the suite of
#' samples identified by the user (by sites and dates) or among the bottle IDs
#' in the imported machine data, these cannot be joined unambiguously. In this
#' case, the data object is returned without any sample data attached, and all
#' samples must be annoted individually at a later step. mach
#'
#' @note rawReactive will include anywhere from none to all of the
#' corresponding samples.sample_id values depending on how many bottle IDs the
#' join was able to match. Because a complete join between the imported data
#' and sample set is highly unlikely until some samples (bottle ID + DT) are
#' identified manually (as a dynamic input), the samples.sample_id of any
#' matches identified at this stage are omitted.
#'
#' @note Caution is warranted here as each of the two inputs, the upload of the
#' raw machine data and the set of samples that the user selects, each has a
#' sample_id column. It is important the the sample_id column of the imported
#' machine file are preserved and not the sample_id field from the picklist of
#' samples.
#'
#' @export

join_sample_metadata <- function(this_machine_import, this_sample_metadata) {

  joined_data <- this_machine_import |>
    dplyr::left_join(
      this_sample_metadata[, !names(this_sample_metadata) %in% c("sample_id")],
      by = c("idToJoin" = "bottle")
    )

  # abort join if it cannnot be done without creating ambiguous samples
  if (nrow(joined_data) > nrow(this_machine_import)) {

    joined_data <- this_machine_import |>
      dplyr::mutate(samples = as.character(NA))

    shiny::showNotification(
      ui          = "cannot guess sample IDs, enter all IDs or try narrowing the range of sample choices",
      duration    = NULL,
      closeButton = TRUE,
      type        = "warning"
    )

  }

  return(joined_data)

}
