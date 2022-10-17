#' @title helper: delete an existing stormwater.samples record
#'
#' @description Function to delete an existin stormwater.samples record from
#' the database.
#'
#' @note This is a non-cascading delete so the query will fail if there are
#' data (solids, chem) tied to the sample record. This is by design and the
#' desired behaviour.
#'
#' @export

delete_sample <- function(sample_to_delete) {

  sample_to_delete <- as.integer(sample_to_delete)

  parameterized_query <- glue::glue_sql("
    DELETE FROM stormwater.samples
    WHERE sample_id = { sample_to_delete }
    ;
    ",
    .con = DBI::ANSI()
  )

  run_interpolated_execution(parameterized_query)

}
