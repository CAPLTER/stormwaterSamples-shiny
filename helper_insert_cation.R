#' @title ICP-derived cation data processing tools
#'
#' @description The function \code{insert_raw_icp} inserts formatted ICP-derived cation
#'   data (from \code{icp_to_raw}) into the stormwater.icp table.
#'
#'
insert_raw_icp <- function(cationData, pool) {
  
  cationDataName <- deparse(substitute(cationData))
  
  # insert icp_raw into icp
  
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
    FROM stormwater.?temporaryTable
  );'
  
  # parameterized query
  parameterizedQuery <- gsub(pattern = "'",
                             replacement = "",
                             x = sqlInterpolate(ANSI(),
                                                baseQuery,
                                                temporaryTable = cationDataName))
  
  
  # execute parameterized query
  paramQueryReturn <- run_interpolated_execution(interpolatedQuery = parameterizedQuery)
  
  # check if number of rows added matches input
  if (paramQueryReturn == nrow(cationData)) {
    
    # remove temp table
    dbRemoveTable(conn = pool,
                  name = c('stormwater', cationDataName))
    
  } else {
    
    return("insert error, check log")
    
  }
  
}

#'
#' @export
#'

icp_to_rslt <- function(icpDataFormatted){
  
  # pare non-sample data and results from alternate wavelengths, stack, assign
  # analysis_ids, add comment re: blanks, assign data qualifiers
  icpResults <- icpDataFormatted %>%
    filter(!grepl('Blank|CalibStd|QC|Tank', temp_out_id, ignore.case=F)) %>%
    select(-c(ca3158, ca3179, na5895, zn2025, filename)) %>%
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
        grepl('blk', temp_out_id, ignore.case = T) ~ 'blank',
        TRUE ~ NA_character_
      ),
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
      temp_out_id =toupper(temp_out_id)
    ) %>%
    select(temp_out_id, run_id, analysis_id, date_analyzed, results, data_qualifier, comments) # %>%
  # as.data.frame()
  
  return(icpResults)
  
}
