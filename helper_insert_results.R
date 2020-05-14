#' @title Build queries for inserting FORMATTED analysis (ICP, AQ2, Lachat,
#'   Shimadzu) output
#'
#' @description The function \code{build_insert_results_query} constructs a
#'   query to insert formatted results (ICP, AQ2, Lachat, Shimadzu) into the
#'   stormwater.results table.
#'
#' @note Cation results require a separate query owing to the data_qualifier
#'   field that is not present in other analyis outputs.

build_insert_results_query <- function(currentTab) {
  
  # unique base query for cations due to data_qualifier field
  
  if (grepl("cation", currentTab, ignore.case = TRUE)) {
    
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
    
  } else {
    
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
      comments
    )
    (
      SELECT
        sample_id,
        run_id,
        replicate,
        analysis_id,
        date_analyzed,
        peak_concentration,
        NULLIF(comments, '')::text
      FROM stormwater.temp_results
    );"
    
  }
  
  # parameterized query
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery)
  
  return(parameterizedQuery)
  
}