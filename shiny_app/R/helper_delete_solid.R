#' @title helper: delete an existing stormwater.solids record
#'
#' @description Function to delete an existing stormwater.solids record from
#' the database.
#'
#' @export

delete_solid <- function(solid_to_delete) {

  solid_to_delete <- as.integer(solid_to_delete)

  parameterized_query <- glue::glue_sql("
    DELETE FROM stormwater.solids
    WHERE solid_id = { solid_to_delete }
    ;
    ",
    .con = DBI::ANSI()
  )

  run_interpolated_execution(parameterized_query)

}
