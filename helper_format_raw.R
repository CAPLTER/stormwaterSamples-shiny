#' @title Tool(s) for restructuring RAW analysis (ICP, AQ2, Lachat, Shimadzu)
#'   output to a format amenable for inserting into the stormwater.results
#'   table.
#'
#' @description The function \code{build_insert_raw_query} constructs a query to
#'   insert raw machine (ICP, AQ2, Lachat, Shimadzu) output into the respective
#'   stormwater.x table.


format_raw <- function(annotatedData, sampleMetadata, currentTab, nitrite = FALSE) {
  
  # general formatting ------------------------------------------------------
  
  formattedData <- annotatedData %>%
    mutate(
      newSample = replace(newSample, newSample == "NULL", NA),
      samples = case_when(
        !is.na(newSample) ~ newSample,
        TRUE ~ samples 
      ),
      comments = case_when(
        grepl('blk', sample_id, ignore.case = T) & comments == "" ~ 'blank',
        grepl('blk', sample_id, ignore.case = T) & comments != "" ~ paste(comments, 'blank', sep = "; "),
        TRUE ~ as.character(comments)
      ),
      replicate = as.integer(replicate)
    ) %>% 
    select(-sample_id) %>% # remove data sample_id to avoid conflict with database sample_id
    inner_join(sampleMetadata, by = c("samples" = "samples"))
  
  if (grepl("cation", currentTab, ignore.case = TRUE)) {

    formattedData <- formattedData %>%
      pivot_longer(
        cols = starts_with(c("ca", "na", "zn")),
        names_to = "analysis",
        values_to = "concentration"
      ) %>%
      mutate(
        analysis_id = case_when(
          grepl('ca', analysis, ignore.case=T) ~ as.integer(8),
          grepl('na', analysis, ignore.case=T) ~ as.integer(31),
          grepl('zn', analysis, ignore.case=T) ~ as.integer(68),
          TRUE ~ NA_integer_
        ),
        data_qualifier = case_when(
          analysis_id == 8 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.027, 1.0, 100, Inf))],
          analysis_id == 31 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.024, 1.0, 100, Inf))],
          analysis_id == 69 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.003, 0.01, 1.0, Inf))],
          TRUE ~ NA_real_
        ),
        date_analyzed = as.POSIXct(date_analyzed, format = "%m/%d/%Y %H:%M:%S%p"),
        results = as.double(concentration),
        data_qualifier = as.integer(data_qualifier)
      )

  } else if (grepl("lachat", currentTab, ignore.case = TRUE)) {

    formattedData <- formattedData %>%
      mutate(
        analysis_id = case_when(
          grepl("chloride", analyte_name, ignore.case = TRUE) ~  as.integer(12),
          grepl("phosphate", analyte_name, ignore.case = TRUE) ~ as.integer(48),
          grepl("nitrate", analyte_name, ignore.case = TRUE) ~   as.integer(39),
          grepl("ammonia", analyte_name, ignore.case = TRUE) ~   as.integer(33),
          TRUE ~ NA_integer_
        ),
        detection_time = as.character(detection_time, format = "%H:%M:%S"),
        date_analyzed = as.POSIXct(paste(detection_date, detection_time))
      )
    
    if (nitrite == TRUE) {
      
    formattedData <- formattedData %>%
      mutate(analysis_id = as.integer(37))
      
    }
    
  } else {

    formattedData <- formattedData

  } # close machine-specific formatting
  
  formattedData <- formattedData %>%  
    as.data.frame()
  
  return(formattedData)
  
} # close format_raw 