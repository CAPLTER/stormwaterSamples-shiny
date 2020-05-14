#' @title Build queries for inserting RAW analysis (ICP, AQ2, Lachat, Shimadzu)
#'   output
#'
#' @description The function \code{build_insert_raw_query} constructs a query to
#'   insert raw machine (ICP, AQ2, Lachat, Shimadzu) output into the respective
#'   stormwater.x table.

build_insert_raw_query <- function(currentTab) {
  
  # raw: cations ------------------------------------------------------------
  
  if (grepl("cation", currentTab, ignore.case = TRUE)) {
    
    baseQuery <- "
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
        sample_id,
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
      FROM stormwater.temp_raw
    );"
    
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
    
  } else {
    
    baseQuery <- NULL
    
  }
  
  # parameterized query
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery)
  
  return(parameterizedQuery)
  
}