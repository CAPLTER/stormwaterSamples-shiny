#' @title Write RAW machine output to a temporary database table and return the
#' query to insert those data into the corresponding table that houses the raw
#' data for that machine.
#'
#' @description The function \code{prepare_raw} adds RAW machine output data to
#' a temporary table in the stormwater schema and returns the appropriate query
#' to insert those data into the production table that houses the raw data for
#' that machine.
#'
#' @export

prepare_raw <- function(raw_reactive, analysis) {

  temp_raw <- raw_reactive

  # write temporary table: raw data

  remove_table(
    schema_name = "stormwater",
    table_name  = "temp_raw"
  )

  DBI::dbWriteTable(
    conn      = this_pool,
    name      = c("stormwater", "temp_raw"),
    value     = temp_raw,
    row.names = FALSE
  )

  # build raw insert query
  insert_raw_query <- build_insert_raw_query(currentTab = analysis)

  return(insert_raw_query)

}
