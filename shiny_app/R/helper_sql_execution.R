#' @title helpers to facilitate sql executions
#'
#' @description The functions run_interpolated_query() and
#'  run_interpolated_execution() are (sub)helper functions that establish and
#'  close database connections to facilitate either a SQL query to return an
#'  objet or execute a statement.
#'
#' @export

run_interpolated_query <- function(interpolatedQuery) {

  tryCatch({

    queryResult <- DBI::dbGetQuery(
      conn      = this_pool,
      statement = interpolatedQuery
    )

    return(queryResult)

  }, warning = function(warn) {

    shiny::showNotification(
      ui          = paste("there is a warning:  ", warn),
      duration    = NULL,
      closeButton = TRUE,
      type        = "warning"
    )

    print(paste("WARNING: ", warn))

  }, error = function(err) {

    shiny::showNotification(
      ui          = paste("there was an error:  ", err),
      duration    = NULL,
      closeButton = TRUE,
      type        = "error"
    )

    print(paste("ERROR: ", err))
    print("ROLLING BACK TRANSACTION")

  }) # close try catch

} # close run_interpolated_query

#' @note execute a database action (other than querying data)
#' @export

run_interpolated_execution <- function(
  interpolatedQuery,
  show_notification = FALSE
  ) {

  tryCatch({

    pool::poolWithTransaction(
      pool = this_pool,
      func = function(conn) {
        DBI::dbExecute(
          conn,
          interpolatedQuery
        )
      }
    )

    if (show_notification == TRUE) {

      shiny::showNotification(
        ui          = "successfully uploaded",
        duration    = NULL,
        closeButton = TRUE,
        type        = "message",
        action      = a(href = "javascript:location.reload();", "reload the page")
      )

    }

  }, warning = function(warn) {

    shiny::showNotification(
      ui          = paste("there is a warning:  ", warn),
      duration    = NULL,
      closeButton = TRUE,
      type        = "warning"
    )

    print(paste("WARNING: ", warn))

  }, error = function(err) {

    shiny::showNotification(
      ui          = paste("there was an error:  ", err),
      duration    = NULL,
      closeButton = TRUE,
      type        = "error"
    )

    print(paste("ERROR: ", err))
    print("ROLLING BACK TRANSACTION")

  }) # close try catch

} # close run_interpolated_execution


#' @note remove table if exists
#' @export

remove_table <- function(schema_name, table_name) {

  if (
    DBI::dbExistsTable(
      conn = this_pool,
      c(schema_name, table_name)
    )
    ) {

    DBI::dbRemoveTable(
      conn = this_pool,
      c(schema_name, table_name)
    )

  }

}
