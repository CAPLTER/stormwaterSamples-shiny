#' @title Write RAW machine output to a temporary database table and return the
#' query to insert those data into the corresponding table that houses the
#' results data for that machine.
#'
#' @description The function \code{prepare_results} adds RAW machine output
#' data to a temporary table in the stormwater schema and returns the
#' appropriate query to insert those data into the production table that houses
#' the reults data for that machine.
#'
#' @export

prepare_results <- function(
  results_reactive,
  samples_metadata,
  analysis,
  is_nitrite = FALSE
  ) {

  # format resultsMetadata() for insert
  temp_results <- format_raw(
    annotatedData  = results_reactive,
    sampleMetadata = samples_metadata,
    currentTab     = analysis,
    nitrite        = is_nitrite
  )

  # write temporary table: results data

  remove_table(
    schema_name = "stormwater",
    table_name  = "temp_results"
  )

  DBI::dbWriteTable(
    conn      = this_pool,
    name      = c("stormwater", "temp_results"),
    value     = temp_results,
    row.names = FALSE
  )

  # build results insert query
  insert_results_query <- build_insert_results_query(currentTab = analysis)

  return(insert_results_query)

}
