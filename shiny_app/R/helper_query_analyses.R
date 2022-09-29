#' @title Stormwater analysis details from the database
#'
#' @description Makes available the list of analyses ids and names

query_analyses <- function() {
  
  baseQuery <- "
  SELECT
    analysis_id,
    analysis_name
  FROM
    stormwater.analysis
  WHERE
    analysis_name IN (
      'NO3T_AQ2',
      'ZnD_ICP',
      'DOC_TOC',
      'PO4T_LACHAT',
      'NO3D_LACHAT',
      'ClD_LACHAT',
      'KD_ICP',
      'NaD_ICP',
      'SO4D_IC',
      'CaD_FLAME_AA',
      'PO4T_TRAACS',
      'NO3T_TRAACS',
      'MgD_ICP',
      'NO2D_LACHAT',
      'PO4T_AQ2',
      'PbD_ICP',
      'NiD_ICP',
      'NO3D_IC',
      'PO4D_LACHAT',
      'CaD_ICP',
      'NH4_LACHAT',
      'NO3T_TOC_TN'
  )
  ORDER BY
    analysis_name;"
  
  commonAnalyses <- run_interpolated_query(baseQuery)
  
  return(commonAnalyses)
  
}

analyses <- query_analyses()

analysesNames <- analyses |>
  dplyr::pull(analysis_name)
