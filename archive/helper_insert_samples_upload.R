#' @title Insert invertebrate sample
#'
#' @description The functions included here (insert_invert_sample_with_note and
#'   insert_invert_sample_sans_note) facilitate inserting a new invertebrate
#'   sample into the database.

# statement to insert a new invertebrate sample if a note is included
insert_invert_sample_with_note <- function(site, reach, surveyor, year, month, day, replicate, note) {
  
  baseQuery <- "
  INSERT INTO ltreb_syc.invertebrate_samples(
    site_id,
    reach_id,
    surveyor_id,
    year,
    month,
    day,
    replicate, 
    note_invertebrate_sample
  )
  VALUES
  (
    (SELECT sites.id FROM ltreb_syc.sites WHERE site_code = ?newSite),
    (SELECT reaches.id FROM ltreb_syc.reaches WHERE reach_code = ?newReach),
    (SELECT surveyors.id FROM ltreb_syc.surveyors WHERE sur_name = ?newSurveyor),
    ?newYear,
    ?newMonth,
    ?newDay,
    ?newReplicate,
    ?newNote
  );"
  
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery,
                                       newSite = site,
                                       newReach = reach,
                                       newSurveyor = surveyor,
                                       newYear = year,
                                       newMonth = month,
                                       newDay = day,
                                       newReplicate = replicate,
                                       newNote = note)
  
  run_interpolated_execution(parameterizedQuery)
  
}


# statement to insert a new invertebrate sample if a note is NOT included
insert_invert_sample_sans_note <- function(site, reach, surveyor, year, month, day, replicate) {
  
  baseQuery <- "
  INSERT INTO ltreb_syc.invertebrate_samples(
    site_id,
    reach_id,
    surveyor_id,
    year,
    month,
    day,
    replicate
  )
  VALUES
  (
    (SELECT sites.id FROM ltreb_syc.sites WHERE site_code = ?newSite),
    (SELECT reaches.id FROM ltreb_syc.reaches WHERE reach_code = ?newReach),
    (SELECT surveyors.id FROM ltreb_syc.surveyors WHERE sur_name = ?newSurveyor),
    ?newYear,
    ?newMonth,
    ?newDay,
    ?newReplicate
  );"
  
  parameterizedQuery <- sqlInterpolate(ANSI(),
                                       baseQuery,
                                       newSite = site,
                                       newReach = reach,
                                       newSurveyor = surveyor,
                                       newYear = year,
                                       newMonth = month,
                                       newReplicate = replicate,
                                       newDay = day)
  
  run_interpolated_execution(parameterizedQuery)
  
}