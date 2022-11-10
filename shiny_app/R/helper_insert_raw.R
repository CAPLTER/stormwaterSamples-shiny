#' @title Build queries for inserting RAW analysis (ICP, AQ2, Lachat, Shimadzu)
#' output
#'
#' @description The function \code{build_insert_raw_query} constructs a query
#' to insert raw machine (ICP, AQ2, Lachat, Shimadzu) output into the
#' respective stormwater.x table.
#'
#' @export

build_insert_raw_query <- function(currentTab) {

  # raw: cations ------------------------------------------------------------

  if (grepl("cation", currentTab, ignore.case = TRUE)) {

    baseQuery <- "
    INSERT INTO stormwater.icp(
      run_id,
      date_analyzed,
      icp_out_id,
      rack_tube,
      type,
      ca_183_944_nm_ppm,
      ca_315_887_nm_ppm,
      ca_317_933_nm_ppm,
      na_588_995_nm_ppm,
      na_589_592_nm_ppm,
      y_371_029_nm_ratio,
      zn_202_548_nm_ppm,
      zn_206_200_nm_ppm,
      zn_213_857_nm_ppm,
      instrument,
      source_file
    )
    (
      SELECT
        run_id,
        date_time,
        solution_label,
        rack_tube,
        type,
        ca_183_944_nm_ppm,
        ca_315_887_nm_ppm,
        ca_317_933_nm_ppm,
        na_588_995_nm_ppm,
        na_589_592_nm_ppm,
        y_371_029_nm_ratio,
        zn_202_548_nm_ppm,
        zn_206_200_nm_ppm,
        zn_213_857_nm_ppm,
        'Agilent ICP-OES'
        filename
      FROM stormwater.temp_raw
      );
    "

  # raw: lachat ------------------------------------------------------------

  } else if (grepl("lachat", currentTab, ignore.case = TRUE)) {

    baseQuery <- '
    INSERT INTO stormwater.lachat
    (
      -- analysis_id,
      run_id,
      lachat_out_id,
      sample_type,
      replicate_number,
      repeat_number,
      cup_number,
      manual_dilution_factor,
      auto_dilution_factor,
      weight_units,
      weight,
      units,
      detection_date,
      detection_time,
      user_name,
      run_file_name,
      description,
      channel_number,
      analyte_name,
      peak_concentration,
      determined_conc,
      concentration_units,
      peak_area,
      peak_height,
      calibration_equation,
      retention_time,
      inject_to_peak_start,
      conc_x_adf,
      conc_x_mdf,
      conc_x_adf_x_mdf,
      source_file
    )
    (
      SELECT
        -- analysis_id,
        run_id,
        sample_id,
        sample_type,
        replicate_number,
        repeat_number,
        cup_number,
        manual_dilution_factor,
        auto_dilution_factor,
        weight_units,
        weight,
        units,
        detection_date,
        detection_time,
        user_name,
        run_file_name,
        description,
        channel_number,
        analyte_name,
        peak_concentration,
        determined_conc_,
        concentration_units,
        peak_area,
        peak_height,
        calibration_equation,
        retention_time,
        inject_to_peak_start,
        conc_x_adf,
        conc_x_mdf,
        conc_x_adf_x_mdf,
        filename
      FROM stormwater.temp_raw
    );'

  # raw: aq2 ------------------------------------------------------------

  } else if (grepl("aq2", currentTab, ignore.case = TRUE)) {

    baseQuery <- "
    INSERT INTO stormwater.aq2
    (
      run_id,
      aq2_out_id,
      sample_details,
      test,
      results,
      units,
      absorbance,
      date_and_time,
      operator,
      pre_dil_factor,
      auto_dil_factor,
      source_file
    )
    (
      SELECT
        run_id,
        sample_id,
        sample_details,
        test,
        results,
        units,
        absorbance,
        date_and_time,
        operator,
        pre_dil_factor,
        auto_dil_factor,
        filename
      FROM stormwater.temp_raw
      );"

  # raw: shimadzu ------------------------------------------------------------

  } else if (grepl("shimadzu", currentTab, ignore.case = TRUE)) {

    baseQuery <- "
    INSERT INTO stormwater.shimadzu
    (
      run_id,
      type,
      analysis,
      sample_name,
      shimadzu_out_id,
      origin,
      result,
      date_time,
      vial,
      source_file
    )
    (
      SELECT
        run_id,
        type,
        analysis,
        sample_name,
        sample_id,
        origin,
        result,
        date_time,
        vial,
        filename
      FROM stormwater.temp_raw
      );"

  } else {

    baseQuery <- NULL

  }

  # parameterized query
  parameterizedQuery <- DBI::sqlInterpolate(
    ANSI(),
    baseQuery
  )

  return(parameterizedQuery)

}
