#' @title Tool(s) for restructuring RAW analysis (ICP, AQ2, Lachat, Shimadzu)
#'   output to a format amenable for inserting into the stormwater.results
#'   table.
#'
#' @description The function \code{build_insert_raw_query} constructs a query to
#'   insert raw machine (ICP, AQ2, Lachat, Shimadzu) output into the respective
#'   stormwater.x table.


format_raw <- function(annotatedData, sampleMetadata, currentTab, nitrite = FALSE) {

  # general formatting ------------------------------------------------------

  formattedData <- annotatedData |>
    dplyr::mutate(
      newSample = replace(newSample, newSample == "NULL", NA),
      samples = dplyr::case_when(
        !is.na(newSample) ~ newSample,
        TRUE ~ samples
        ),
      comments = dplyr::case_when(
        grepl("blk", sample_id, ignore.case = T) & comments == "" ~ "blank",
        grepl("blk", sample_id, ignore.case = T) & comments != "" ~ paste(comments, "blank", sep = "; "),
        TRUE ~ as.character(comments)
        ),
      replicate = as.integer(replicate)
      ) |>
  dplyr::select(-sample_id) |> # remove data sample_id to avoid conflict with database sample_id
  dplyr::inner_join(sampleMetadata, by = c("samples" = "samples"))

  # cation-specific formatting

  if (grepl("cation", currentTab, ignore.case = TRUE)) {

    formattedData <- formattedData |>
      tidyr::pivot_longer(
        cols      = starts_with(c("ca", "na", "zn")),
        names_to  = "analysis",
        values_to = "concentration"
        ) |>
    dplyr::mutate(
      analysis_id = dplyr::case_when(
        grepl("ca", analysis, ignore.case = T) ~ as.integer(8),
        grepl("na", analysis, ignore.case = T) ~ as.integer(31),
        grepl("zn", analysis, ignore.case = T) ~ as.integer(68),
        TRUE ~ NA_integer_
        ),
      data_qualifier = dplyr::case_when(
        analysis_id == 8 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.027, 1.0, 100, Inf))],
        analysis_id == 31 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.024, 1.0, 100, Inf))],
        analysis_id == 69 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.003, 0.01, 1.0, Inf))],
        TRUE ~ NA_real_
        ),
      date_analyzed  = as.POSIXct(date_analyzed, format = "%m/%d/%Y %H:%M:%S%p"),
      results        = as.double(concentration),
      data_qualifier = as.integer(data_qualifier)
    )

    # lachat-specific formatting

  } else if (grepl("lachat", currentTab, ignore.case = TRUE)) {

    formattedData <- formattedData |>
      dplyr::mutate(
        analysis_id = dplyr::case_when(
          grepl("chloride", analyte_name, ignore.case = TRUE)   ~   as.integer(12),
          grepl("phosphate", analyte_name, ignore.case = TRUE)  ~   as.integer(48),
          grepl("nitrate", analyte_name, ignore.case = TRUE)    ~   as.integer(39),
          grepl("ammonia", analyte_name, ignore.case = TRUE)    ~   as.integer(33),
          TRUE ~ NA_integer_
          ),
        detection_time = as.character(detection_time, format = "%H:%M:%S"),
        date_analyzed  = as.POSIXct(paste(detection_date, detection_time))
      )

    # if phosphate: microgram -> milligram
    formattedData <- formattedData |>
      dplyr::mutate(peak_concentration = dplyr::case_when(
          analysis_id == 48 ~ peak_concentration / 1000,
          TRUE ~ peak_concentration
      )
      )

    # address nitrite
    if (nitrite == TRUE) {

      formattedData <- formattedData |>
        dplyr::mutate(analysis_id = as.integer(37))

    }

    # aq2-specific formatting

  } else if (grepl("aq2", currentTab, ignore.case = TRUE)) {

    formattedData <- formattedData |>
      dplyr::mutate(
        analysis_id = dplyr::case_when(
          grepl("nitrate", test, ignore.case = TRUE)    ~ as.integer(65),
          grepl("phosphate", test, ignore.case = TRUE)  ~ as.integer(67),
          TRUE ~ NA_integer_
          ),
        date_analyzed = as.POSIXct(date_and_time, format = "%a %b %d %H:%M:%S %Y")
      )

    # shimadzu-specific formatting

  } else if (grepl("shimadzu", currentTab, ignore.case = TRUE)) {

    formattedData$doc <- unlist(lapply(strsplit(formattedData$result, "\\:|\\s|m"), "[", 2)) # doc_toc (19)
    formattedData$tn  <- unlist(lapply(strsplit(formattedData$result, "\\:|\\s|m"), "[", 5))  # no3t_toc_tn (42)

    formattedData <- formattedData |>
      dplyr::select(
        sample_id,
        run_id,
        replicate,
        date_time,
        comments,
        doc,
        tn
        ) |>
    tidyr::pivot_longer(
      cols      = doc:tn,
      names_to  = "analysis",
      values_to = "concentration"
      ) |>
    dplyr::mutate(
      analysis_id = dplyr::case_when(
        grepl("doc", analysis, ignore.case = TRUE)  ~ as.integer(19),
        grepl("tn", analysis, ignore.case = TRUE)   ~ as.integer(42),
        TRUE ~ NA_integer_
        ),
      date_analyzed = as.POSIXct(date_time, format = "%Y-%m-%d %H:%M:%S"),
      concentration = as.numeric(concentration)
    )

  } else {

    formattedData <- formattedData

  } # close machine-specific formatting

  formattedData <- formattedData |>
    as.data.frame()

  return(formattedData)

} # close format_raw
