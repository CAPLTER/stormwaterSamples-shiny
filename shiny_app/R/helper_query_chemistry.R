#' @title helper: query chemistry data for review
#'
#' @description The \code{query_chemistry} function query_single_sample)
#' facilitate querying stormwater chemsitry and associated data from the
#' database for general querying and reviewing of chemistry data that exist in
#' the database.
#'
#' @export

query_chemistry <- function() {

  parameterized_query <- glue::glue_sql("
    SELECT
      sites.abbreviation AS site,
      samples.sample_datetime::TEXT,
      samples.bottle,
      results.replicate,
      analysis.analysis_name,
      results.date_analyzed::TEXT,
      results.concentration,
      samples.comments AS samples_comments,
      results.comments AS results_comments,
      CASE
        WHEN icp_file.source_file IS NOT NULL THEN icp_file.source_file
        WHEN lachat_file.source_file IS NOT NULL THEN lachat_file.source_file
        WHEN aq2_file.source_file IS NOT NULL THEN aq2_file.source_file
        WHEN shimadzu_file.source_file IS NOT NULL THEN shimadzu_file.source_file
      ELSE NULL
      END AS source_file
    FROM stormwater.results
    JOIN stormwater.samples ON (samples.sample_id = results.sample_id)
    JOIN stormwater.sites ON (samples.site_id = sites.site_id)
    JOIN stormwater.analysis ON (results.analysis_id = analysis.analysis_id)
    LEFT JOIN (
      SELECT
        run_id,
        source_file
      FROM stormwater.icp
      WHERE
        source_file IS NOT NULL
      GROUP BY
        run_id,
        source_file
      ) AS icp_file ON (icp_file.run_id = results.run_id)
    LEFT JOIN (
      SELECT
        run_id,
        source_file
      FROM stormwater.lachat
      WHERE
        source_file IS NOT NULL
      GROUP BY
        run_id,
        source_file
      ) AS lachat_file ON (lachat_file.run_id = results.run_id)
    LEFT JOIN (
      SELECT
        run_id,
        source_file
      FROM stormwater.aq2
      WHERE
        source_file IS NOT NULL
      GROUP BY
        run_id,
        source_file
      ) AS aq2_file ON (aq2_file.run_id = results.run_id)
    LEFT JOIN (
      SELECT
        run_id,
        source_file
      FROM stormwater.shimadzu
      WHERE
        source_file IS NOT NULL
      GROUP BY
        run_id,
        source_file
      ) AS shimadzu_file ON (shimadzu_file.run_id = results.run_id)
    WHERE
      samples.sample_datetime >= { Sys.Date() - lubridate::years(3) }
    ORDER BY
      results.run_id DESC,
      results.result_id ASC
    ;
    ",
    .con = DBI::ANSI()
  )

  run_interpolated_query(parameterized_query)

}
