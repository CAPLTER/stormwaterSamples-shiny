#' @title ICP-derived cation data processing tools
#'
#' @description The function \code{insert_raw_icp} inserts formatted ICP-derived cation
#'   data (from \code{icp_to_raw}) into the stormwater.icp table.
#'
#'
#'
build_insert_raw_cation_query <- function() {
  
  # base query
  baseQuery <- '
  INSERT INTO stormwater.icp(
    run_id,
    date_analyzed,
    icp_out_id,
    operator,
    x_3,
    ca3158,
    ca3179,
    na5889,
    ca3933,
    na5895,
    zn2025,
    zn2138,
    source_file
  )
  (
    SELECT
      run_id,
      date_analyzed,
      temp_out_id,
      operator,
      icp_id,
      ca3158,
      ca3179,
      ca3933,
      na5889,
      na5895,
      zn2025,
      zn2138,
      filename
    --FROM stormwater.?temporaryTable
    FROM stormwater.temp_raw
  );'
  
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery)
  
  
  return(parameterizedQuery)
  
}


icp_to_rslt <- function(cationDataFormatted, sampleMetadata){
  
  # pare non-sample data and results from alternate wavelengths, stack, assign
  # analysis_ids, add comment re: blanks, assign data qualifiers
  
  cationResults <- cationDataFormatted %>%
    inner_join(sampleMetadata, by = c("sampleID" = "samples")) %>%
    pivot_longer(
      cols = starts_with(c("ca", "na", "zn")),
      names_to = "analysis",
      values_to = "concentration") %>%
    mutate(
      analysis_id = case_when(
        grepl('ca', analysis, ignore.case=T) ~ as.integer(8),
        grepl('na', analysis, ignore.case=T) ~ as.integer(31),
        grepl('zn', analysis, ignore.case=T) ~ as.integer(68),
        TRUE ~ NA_integer_
      ),
      comments = case_when(
        grepl('blk', temp_out_id, ignore.case = T) & comments == "" ~ 'blank',
        grepl('blk', temp_out_id, ignore.case = T) & comments != "" ~ paste(comments, 'blank', sep = "; "),
        TRUE ~ as.character(comments)),
      # comments = case_when(
      #   grepl('blk', temp_out_id, ignore.case = T) ~ 'blank',
      #   TRUE ~ NA_character_
      # ),
      data_qualifier = case_when(
        analysis_id == 8 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.027, 1.0, 100, Inf))],
        analysis_id == 31 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.024, 1.0, 100, Inf))],
        analysis_id == 69 ~ c(17, 10, NA, 1) [findInterval(concentration, c(-Inf, 0.003, 0.01, 1.0, Inf))],
        TRUE ~ NA_real_
      ),
      date_analyzed = as.POSIXct(date_analyzed, format = "%m/%d/%Y %H:%M:%S%p"),
      results = as.double(concentration),
      data_qualifier = as.integer(data_qualifier),
      temp_out_id = str_replace_all(temp_out_id, "\\.", "\\_"),
      temp_out_id = toupper(temp_out_id),
      replicate = as.integer(replicate)
    ) %>%
    as.data.frame()
  
  return(cationResults)
  
}

build_insert_results_cation_query <- function() {
  
  
  # base query
  baseQuery <- "
  INSERT INTO stormwater.results
  (
    sample_id,
    run_id,
    replicate,
    analysis_id,
    date_analyzed,
    concentration,
    data_qualifier,
    comments
  )
  (
    SELECT
      sample_id,
      run_id,
      replicate,
      analysis_id,
      date_analyzed,
      results,
      data_qualifier,
      NULLIF(comments, '')::text
    FROM stormwater.temp_results
  );"
  
  # parameterized query
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery)
  
  return(parameterizedQuery)
  
}
