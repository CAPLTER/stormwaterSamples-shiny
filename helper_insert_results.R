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

  # cation-specific query (accomodates data_qualifier field)

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

    # lachat-specfic query

  } else if (grepl("lachat", currentTab, ignore.case = TRUE)) {

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
      conc_x_adf_x_mdf,
      NULLIF(comments, '')::text
      FROM stormwater.temp_results
      );"

  # aq2-specfic query

} else if (grepl("aq2", currentTab, ignore.case = TRUE)) {

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
    results,
    NULLIF(comments, '')::text
    FROM stormwater.temp_results
    );"

  } else {

    baseQuery <- NULL

  }


# parameterized query

parameterizedQuery <- sqlInterpolate(ANSI(),
  baseQuery)

return(parameterizedQuery)

}
