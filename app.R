
# README ------------------------------------------------------------------

# UPDATE 2018-06-20: Added functionality to view discharge data in the database.
# Changed discharge insert/upload funtionality to do nothing (i.e., skip
# insert/upload) for duplicate samples - this allows, for example, a Flowlink
# file with a mix of new and already uploaded data to be uploaded without having
# to manually remove the data that are already in the database. Added pagination
# to samples viewer.

# Note regarding Javascript: There are several code chunks that have the comment
# 'notable stmt' associated with them. These statements are related to the
# Javascript functionality required for the interactivity of a file upload,
# i.e., adding temp, cond, etc. to the table following a file uplooad. See
# https://stackoverflow.com/questions/40020600/r-shiny-selectedinput-inside-renderdatatable-cells
# and
# https://groups.google.com/forum/#!msg/shiny-discuss/ZUMBGGl1sss/zfcG9c6MBAAJ
# for details.

# Note regarding DOC and AFDM identifiers: This app was built initially with the
# idea that bottle/vial IDs for DOC and AFDM would be entered by the
# technicians. However, the technician team moved to an approach of simply
# labelling both DOC and AFDM bottles with the bottle ID corresponding to a
# sample (e.g., 11_2_2). The functionality for adding DOC and AFDM IDs was left
# in the code but simply commented out of execution, and it should be possible
# to simply uncomment those code chunks and change the database statements if
# there is interest to revert.


# call global R -----------------------------------------------------------

source('global.R')


# UI ----------------------------------------------------------------------

ui <- tagList(
  tags$head(
    tags$style(
      HTML("#leftPanel { background: #D3D3D3; }"),
      HTML("#submitFileUpload { background-color: #800000;
                          color: white; }"),
      HTML("#addSample { border-style: solid;
                         border-color: #0000ff; }"),
      HTML("#deleteManualEntryRow { border-style: solid;
                                    border-color: #ff0000 }"),
      HTML("#submitEnteredData { background-color: #800000;
                                 color: white; }"),
      HTML("#submitDischargeFileUpload { background-color: #800000;
                                         color: white; }"),
      HTML("#submitAFDM { background-color: #800000;
                          color: white; }")
    ) # close tags$head
  ), # close tagss$style
  navbarPage("stormwater",
             
             # isco tab ----------------------------------------------------------------
             
             tabPanel("isco",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 fileInput("file1", "choose csv file",
                                           multiple = FALSE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
                                 sliderInput(inputId = "storm",
                                             label = "storm/carousel",
                                             min = 1,
                                             max = 5,
                                             value = 1),
                                 textAreaInput("fileUploadNotes",
                                               "notes (applied to all samples)",
                                               resize = 'vertical',
                                               value = NULL),
                                 actionButton("submitFileUpload",
                                              "submit data"),
                                 br(),
                                 br()
                          ), # close the left col
                          
                          column(id = "fileUploadMiddlePanel", 5,
                                 DT::dataTableOutput("buttonsInTable"),
                                 tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
          Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                            })")) # notable stmt
                          ), # close the middle col
                          
                          column(id = "fileUploadRightPanel", 5,
                                 DT::dataTableOutput("checked")
                          ), # close the right col
                          
                        ) # close the row
                      ) # close the page
             ), # clase file upload tab panel
             
             # samples tab -------------------------------------------------------------
             
             tabPanel("samples",
                      modifySamplesUI("modifySamples") 
             ), # close maintenance log tab 
             
             tabPanel("discharge: file upload",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 fileInput("dischargeFile", "Choose CSV File",
                                           multiple = FALSE,
                                           accept = c("text/csv",
                                                      "text/comma-separated-values,text/plain",
                                                      ".csv")),
                                 actionButton("submitDischargeFileUpload",
                                              "submit data"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "dischargeRightPanel", 10,
                                 strong('Data Preview'),
                                 hr(),
                                 tableOutput("levelDataPreview")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # close discharge: file upload
             
             # discharge tab -----------------------------------------------------------
             
             tabPanel("discharge: viewer",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 selectizeInput("dischargeViewerSiteID",
                                                "Site ID",
                                                choices = c("IBW",
                                                            "centralNorth",
                                                            "centralSouth",
                                                            "price",
                                                            "kiwanisPark",
                                                            "camelback",
                                                            "pierce",
                                                            "martinResidence",
                                                            "montessori",
                                                            "sweetwater",
                                                            "bellaVista",
                                                            "lakeMarguerite",
                                                            "silveradoGolfCourse",
                                                            "encantada",
                                                            "ave7th"
                                                ),
                                                selected = NULL,
                                                multiple = FALSE),
                                 br(),
                                 tags$b("date range:",
                                        style = "text-align:center"),
                                 br(),
                                 br(),
                                 dateInput("dischargeStartDate",
                                           "start:",
                                           format = "yyyy-mm-dd"),
                                 dateInput("dischargeEndDate",
                                           "end:",
                                           format = "yyyy-mm-dd"),
                                 actionButton(inputId = "queryDischarge",
                                              label = "view discharge",
                                              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "dischargeViewerRightPanel", 10,
                                 DT::dataTableOutput("dischargeData")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # close discharge: viewer
             
             # solids tab --------------------------------------------------------------
             
             tabPanel("solids",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 selectizeInput("afdmSiteID",
                                                "Site ID",
                                                choices = c("IBW",
                                                            "lakeMarguerite",
                                                            "silveradoGolfCourse",
                                                            "centralNorth",
                                                            "centralSouth",
                                                            "Price"),
                                                selected = NULL,
                                                multiple = FALSE),
                                 br(),
                                 tags$b("date range:",
                                        style = "text-align:center"),
                                 br(),
                                 br(),
                                 dateInput("afdmStartDate",
                                           "start:",
                                           format = "yyyy-mm-dd"),
                                 dateInput("afdmEndDate",
                                           "end:",
                                           format = "yyyy-mm-dd"),
                                 actionButton(inputId = "querySamplesAFDM",
                                              label = "get samples",
                                              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
                                 br(),
                                 br(),
                                 textAreaInput("afdmUploadNotes",
                                               "notes",
                                               resize = 'vertical'),
                                 actionButton("submitAFDM",
                                              "submit data"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "afdmRightPanel", 10,
                                 DT::dataTableOutput("afdmDataEntryTable"),
                                 tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
          Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                            })")), # notable stmt
                                 strong('data preview'),
                                 hr(),
                                 tableOutput("previewAfdmUpload")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ) # close AFDM tab panel
             
             # closing UI --------------------------------------------------------------
             
  ) # close navbar/page
) # close tagList


# server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # file upload -------------------------------------------------------------
  
  # helper function for reading checkbox
  shinyInputForCheckbox = function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
      inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
  }
  
  
  # site id from file upload
  sampleReportSiteId <- reactive({
    
    req(input$file1)
    
    read_csv(input$file1$datapath,
             n_max = 1,
             col_names = c("reportText", "siteID")) %>%
      dplyr::select(siteID) %>%
      unlist(., use.names=FALSE)
    
  })
  
  
  # max date (for adding blank) from file upload
  sampleReportMaxDate <- reactive({
    
    req(input$file1)
    
    maxDate <- read_csv(input$file1$datapath,
                        skip = 7,
                        col_names = c("sample_datetime", "eventNumber")) %>% 
      dplyr::select(sample_datetime) %>% 
      dplyr::filter(!is.na(sample_datetime)) %>%
      dplyr::mutate(
        sample_datetime = parse_date_time(sample_datetime, c("mdY HMS p", "mdY HMS", "mdY HM"))
      ) %>% 
      dplyr::summarise(maxDTTM = max(sample_datetime))
    
    # because this function is to add a blank, set the time according to
    # prescribed approach for blank times (e.g., blank #2 = 00:00:20)
    hour(maxDate$maxDTTM) = 0
    minute(maxDate$maxDTTM) = 0
    second(maxDate$maxDTTM) = input$storm*10
    
    return(maxDate$maxDTTM)
    
  })
  
  
  # sample data from file upload
  sampleReportData <- reactive({
    
    session$sendCustomMessage('unbind-DT', 'buttonsInTable') # notable stmt
    
    req(input$file1)
    
    read_csv(input$file1$datapath,
             skip = 7,
             col_names = c("sample_datetime", "eventNumber")) %>% 
      mutate(bottle = paste0(sampleReportSiteId(), "_", input$storm, "_", eventNumber)) %>%
      dplyr::select(bottle, sample_datetime) %>% 
      dplyr::filter(!is.na(sample_datetime)) %>% 
      dplyr::mutate(sample_datetime = parse_date_time(sample_datetime, c("mdY HMS p", "mdY HMS", "mdY HM"))) %>% 
      add_row(bottle = paste0(sampleReportSiteId(), "_", input$storm, "_BLK"),
              sample_datetime = sampleReportMaxDate()) %>% 
      mutate(sample_datetime = format(sample_datetime, "%Y-%m-%d %H:%M:%S"))
    
  })
  
  
  # render the file upload and data input tools
  output$buttonsInTable <- DT::renderDataTable({
    
    sampleReportData() %>% 
      mutate(
        omit = shinyInputForCheckbox(checkboxInput,
                                     nrow(sampleReportData()),
                                     "omit_",
                                     value = FALSE,
                                     width = "20px")
      )
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F,
                 preDrawCallback = JS('function() { 
                           Shiny.unbindAll(this.api().table().node()); }'), 
                 drawCallback = JS('function() { 
                        Shiny.bindAll(this.api().table().node()); } ') 
  ),
  rownames = F) # close renderDataTable
  
  # helper function for reading checkbox
  shinyValue = function(id, len) { 
    unlist(lapply(seq_len(len), function(i) { 
      value = input[[paste0(id, i)]] 
      if (is.null(value)) NA else value 
    })) 
  } 
  
  # capture file upload and provided data
  combinedData <- reactive({
    
    sampleReportData() %>%
      mutate(
        omit = shinyValue("omit_",
                          nrow(sampleReportData())),
        notes = input$fileUploadNotes
      ) %>% 
      filter(omit == FALSE)
    
  })
  
  # preview data table with upload and provided values
  output$checked <- DT::renderDataTable({
    
    combinedData() %>% 
      mutate(sample_datetime = as.character(sample_datetime)) %>% 
      select(-omit)
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F
  ),
  rownames = F) # close output$checked
  
  # write uploaded file and edits to database
  observeEvent(input$submitFileUpload, {
    
    # modify data object as needed for the DB
    sampleUploadToWrite <- combinedData() %>% 
      mutate(
        sample_datetime = as.POSIXct(sample_datetime, format = "%Y-%m-%d %H:%M:%S"),
        site_id = sampleReportSiteId()
      ) %>% 
      select(-omit)
    
    # write new samples to samples_temp
    if (dbExistsTable(stormPool, c('stormwater', 'samples_temp'))) dbRemoveTable(stormPool, c('stormwater', 'samples_temp'))
    dbWriteTable(stormPool, c('stormwater', 'samples_temp'), value = sampleUploadToWrite, row.names = F)
    
    # remove timezone type generated by dbWriteTable function
    alterTableQuery <- '
    ALTER TABLE stormwater.samples_temp
    ALTER COLUMN sample_datetime TYPE TIMESTAMP WITHOUT TIME ZONE;'
    
    run_interpolated_execution(alterTableQuery)
    
    # insert from temp into samples
    baseInsert <- "
    INSERT INTO stormwater.samples
    (
      site_id,
      sample_datetime,
      comments,
      bottle,
      doc_vial_id,
      afdm_bottle_id
    )
    (
      SELECT
        site_id,
        sample_datetime,
        NULLIF(notes, '')::text,
        bottle,
        bottle,
        bottle
      FROM
      stormwater.samples_temp
    );"
    
    run_interpolated_query(baseInsert)
    
  })
  
  
  # call to modifySamples module --------------------------------------------
  
  callModule(module = modifySamples,
             id = "modifySamples")
  
  
  # call to modifySolids module ---------------------------------------------
  
  # callModule(module = modifySolids,
  #            id = "modifySolids")
  
  
  # discharge viewer --------------------------------------------------------
  
  # everything below is starting with merely a copy of the samples viewer
  
  # function to translate text site id to numeric site id (as in the DB) this is
  # a duplicate of the samplesViewerSiteId - here recreated for convenience but
  # if ever refactoring, coding this once for both purposes would be a better
  # approach.
  dischargeViewerSiteId <- reactive({
    
    if (input$dischargeViewerSiteID == 'IBW') {
      numericSiteCode <- 11
    } else if (input$dischargeViewerSiteID == 'centralNorth') {
      numericSiteCode <- 14
    } else if (input$dischargeViewerSiteID == 'centralSouth') {
      numericSiteCode <- 15
    } else if (input$dischargeViewerSiteID  == 'price') {
      numericSiteCode <- 16
    } else if (input$dischargeViewerSiteID  == 'kiwanisPark') {
      numericSiteCode <- 1
    } else if (input$dischargeViewerSiteID  == 'camelback') {
      numericSiteCode <- 2
    } else if (input$dischargeViewerSiteID  == 'pierce') {
      numericSiteCode <- 4
    } else if (input$dischargeViewerSiteID  == 'martinResidence') {
      numericSiteCode <- 5
    } else if (input$dischargeViewerSiteID  == 'montessori') {
      numericSiteCode <- 6
    } else if (input$dischargeViewerSiteID  == 'sweetwater') {
      numericSiteCode <- 7
    } else if (input$dischargeViewerSiteID  == 'bellaVista') {
      numericSiteCode <- 8
    } else if (input$dischargeViewerSiteID  == 'lakeMarguerite') {
      numericSiteCode <- 9
    } else if (input$dischargeViewerSiteID  == 'silveradoGolfCourse') {
      numericSiteCode <- 10
    } else if (input$dischargeViewerSiteID  == 'encantada') {
      numericSiteCode <- 12
    } else if (input$dischargeViewerSiteID  == 'ave7th') {
      numericSiteCode <- 13
    } 
    
    return(numericSiteCode)
    
  })
  
  # query discharge data from database for viewing
  viewerDischargeData <- reactive({
    
    req(input$queryDischarge)
    
    pg <- stormPool()
    
    baseDischargeDataQuery <- '
    SELECT
      s.abbreviation AS site,
      d.event_datetime,
      d.water_height,
      d.discharge,
      d.discharge_corrected AS discharge_corr
    FROM
      stormwater.discharge d
    JOIN
      stormwater.sites s ON (s.site_id = d.site_id)
    WHERE 
      s.site_id = ?siteId AND
      d.event_datetime BETWEEN ?start AND ?end
    ORDER BY
    	s.abbreviation,
    	d.event_datetime;    
    '
    
    isolate(
      dischargeDataQuery <- sqlInterpolate(ANSI(),
                                           baseDischargeDataQuery,
                                           siteId = dischargeViewerSiteId(),
                                           start = as.character(input$dischargeStartDate),
                                           end = as.character(input$dischargeEndDate))
    )
    
    dischargeSamples <- dbGetQuery(pg,
                                   dischargeDataQuery)
    
    dbDisconnect(pg)
    
    
    # rather than a simple return, we need to address conditions when there are
    # not any data that match the search criteria
    
    # format the date if there are data
    if (nrow(dischargeSamples) >= 1) {
      
      dischargeSamples <- dischargeSamples %>% 
        mutate(event_datetime = format(event_datetime, "%Y-%m-%d %H:%M:%S"))
      
      # else build an empty frame with just NAs
    } else {
      
      dischargeSamples <- data.frame(
        sample_id = "no samples match those criteria"
      )
      
    }
    
    return(dischargeSamples)
    
  }) 
  
  
  # render discharge data for viewing
  output$dischargeData <- DT::renderDataTable({
    
    viewerDischargeData()
    
  },
  selection = 'none',
  escape = FALSE,
  server = TRUE,
  options = list(paging = TRUE,
                 pageLength = 25,
                 ordering = TRUE,
                 searching = FALSE),
  rownames = F) # close renderDataTable
  
  
  # discharge upload --------------------------------------------------------
  
  # site id from file upload
  levelDataSiteId <- reactive({
    
    req(input$dischargeFile)
    
    read_csv(input$dischargeFile$datapath,
             n_max = 1,
             col_names = c("reportText", "siteID")) %>% 
      dplyr::select(siteID) %>% 
      unlist(., use.names=FALSE)
    
  })
  
  
  # level data from file upload
  levelData <- reactive({
    
    req(input$dischargeFile)
    
    read_csv(input$dischargeFile$datapath,
             skip = 7,
             col_names = c("event_datetime", "level")) %>% 
      dplyr::filter(!is.na(event_datetime)) %>% 
      mutate(
        event_datetime = parse_date_time(event_datetime, c("mdY HMS p", "mdY HMS")),
        event_datetime = format(event_datetime, "%Y-%m-%d %H:%M:%S")
      ) %>% 
      mutate(site_id = levelDataSiteId()) %>% 
      select(site_id, event_datetime, level)
    
  })
  
  
  # preview data table with upload and provided values
  output$levelDataPreview <- renderTable({
    
    levelData()
    
  })
  
  
  # write discharge data to database
  observeEvent(input$submitDischargeFileUpload, {
    
    # modify data object as needed for the DB
    dischargeToWrite <- levelData() %>%
      mutate(event_datetime = as.POSIXct(event_datetime, format = "%Y-%m-%d %H:%M:%S")) 
    
    # beging DB sequence
    
    # establish db connection
    pg <- stormPool()
    
    # insert into samples. the 'ON CONFLICT' clause allows for skipping
    # inserting any data that are already in the database.
    insertLevelDataQuery <- '
    INSERT INTO stormwater.discharge
    (
      site_id,
      event_datetime,
      water_height
    )
    (
      SELECT
        site_id,
        event_datetime,
        level
      FROM
      stormwater.discharge_temp
    )
    ON CONFLICT ON CONSTRAINT discharge_unique_observations DO NOTHING;'
    
    
    tryCatch({
      
      dbGetQuery(pg, "BEGIN TRANSACTION")
      
      # write new samples to samples_temp
      if (dbExistsTable(pg, c('stormwater', 'discharge_temp'))) dbRemoveTable(pg, c('stormwater', 'discharge_temp'))
      dbWriteTable(pg, c('stormwater', 'discharge_temp'), value = dischargeToWrite, row.names = F)
      
      # remove timezone type generated by dbWriteTable function
      dbExecute(pg,'
            ALTER TABLE stormwater.discharge_temp
            ALTER COLUMN event_datetime TYPE TIMESTAMP WITHOUT TIME ZONE;')
      
      # execute insert query
      dbExecute(pg, insertLevelDataQuery)
      
      # clean up
      dbRemoveTable(pg, c('stormwater', 'discharge_temp'))
      
      dbCommit(pg)
      
      showNotification(ui = "successfully uploaded",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'message',
                       action = a(href = "javascript:location.reload();", "reload the page"))
      
    }, warning = function(warn) {
      
      showNotification(ui = paste("there is a warning:  ", warn),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'warning')
      
      print(paste("WARNING: ", warn))
      
    }, error = function(err) {
      
      showNotification(ui = paste("there was an error:  ", err),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
      print(paste("ERROR: ", err))
      print("ROLLING BACK TRANSACTION")
      
      dbRollback(pg)
      
    }) # close try catch
    
    # close db connection
    dbDisconnect(pg)
    
  })
  
  
  # ash free dry mass -------------------------------------------------------
  
  # function to translate text site id to numeric site id (as in the DB) this is
  # a duplicate of the samplesViewerSiteId & dischargeViewerSiteId - here
  # recreated for convenience but if ever refactoring, coding this once for both
  # purposes would be a better approach.
  dbAfdmSiteId <- reactive({
    
    if (input$afdmSiteID == 'IBW') {
      numericSiteCode <- 11
    } else if (input$afdmSiteID == 'centralNorth') {
      numericSiteCode <- 14
    } else if (input$afdmSiteID == 'centralSouth') {
      numericSiteCode <- 15
    } else if (input$afdmSiteID == 'Price') {
      numericSiteCode <- 16
    } else if (input$afdmSiteID == 'lakeMarguerite') {
      numericSiteCode <- 9
    } else if (input$afdmSiteID == 'silveradoGolfCourse') {
      numericSiteCode <- 10
    } 
    
    return(numericSiteCode)
    
  })
  
  
  # query sample-afdm data from database
  afdmSampleData <- reactive({
    
    req(input$querySamplesAFDM)
    
    session$sendCustomMessage('unbind-DT', 'afdmDataEntryTable') # notable stmt
    
    # pg <- stormPool()
    
    baseSamplesDataQuery <- '
    SELECT
      samples.sample_id,
      samples.site_id,
      samples.sample_datetime,
      samples.bottle, 
      samples.afdm_bottle_id
    FROM stormwater.samples
    LEFT JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
    WHERE
      samples.site_id = ?siteId AND
    	samples.sample_datetime BETWEEN ?start AND ?end AND
      (
        solids.filter_initial IS NULL AND
        solids.filter_dry IS NULL AND
        solids.volume_filtered IS NULL AND
        solids.filter_ashed IS NULL
      )
    ORDER BY samples.sample_datetime ASC;'
    
    isolate(
      samplesDataQuery <- sqlInterpolate(ANSI(),
                                         baseSamplesDataQuery,
                                         siteId = dbAfdmSiteId(),
                                         start = as.character(input$afdmStartDate),
                                         end = as.character(input$afdmEndDate))
    )
    
    afdmStormSamples <- dbGetQuery(stormPool,
                                   samplesDataQuery)
    
    # dbDisconnect(pg)
    
    # rather than a simple return, we need to address conditions when there
    # are not any data that match the search criteria
    
    # format the date if there are data
    if (nrow(afdmStormSamples) >= 1) {
      
      afdmStormSamples <- afdmStormSamples %>% 
        mutate(sample_datetime = format(sample_datetime, "%Y-%m-%d %H:%M:%S"))
      
      # else build an empty frame with just NAs
    } else {
      
      afdmStormSamples <- data.frame(
        sample_id = NA,
        site_id = NA,
        sample_datetime = NA,
        bottle = NA, 
        afdm_bottle_id = NA
      )
      
    }
    
    # return data or empty data frame
    return(afdmStormSamples)
    
  }) 
  
  
  # render the afdm samples and data entry fields
  output$afdmDataEntryTable <- DT::renderDataTable({
    
    afdmSampleData() %>% 
      mutate(omit_sample = shinyInputForCheckbox(checkboxInput,
                                      nrow(afdmSampleData()),
                                      "omit_afdm_sample_",
                                      value = FALSE,
                                      width = "20px"),
             filter_wt = shinyInputForCheckbox(numericInput,
                                    nrow(afdmSampleData()),
                                    "filter_wt_",
                                    value = NULL,
                                    min = 0,
                                    width = "90px"),
             filter_dry = shinyInputForCheckbox(numericInput,
                                     nrow(afdmSampleData()),
                                     "filter_dry_",
                                     value = NULL,
                                     min = 0,
                                     width = "90px"),
             volume = shinyInputForCheckbox(numericInput,
                                 nrow(afdmSampleData()),
                                 "volume_",
                                 value = NULL,
                                 min = 0,
                                 width = "90px"),
             filter_ashed = shinyInputForCheckbox(numericInput,
                                       nrow(afdmSampleData()),
                                       "filter_ashed_",
                                       value = NULL,
                                       min= 0,
                                       width = "90px")
      )
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F,
                 preDrawCallback = JS('function() { 
                           Shiny.unbindAll(this.api().table().node()); }'), 
                 drawCallback = JS('function() { 
                        Shiny.bindAll(this.api().table().node()); } ') 
  ),
  rownames = F) # close renderDataTable
  
  
  # capture file upload and provided data
  afdmSamplesAndData <- reactive({
    
    afdmSampleData() %>%
      mutate(omit = shinyValue("omit_afdm_sample_",
                               nrow(afdmSampleData())),
             filter_wt = shinyValue('filter_wt_',
                                    nrow(afdmSampleData())),
             filter_dry = shinyValue('filter_dry_',
                                     nrow(afdmSampleData())),
             volume = shinyValue('volume_',
                                 nrow(afdmSampleData())),
             filter_ashed = shinyValue('filter_ashed_',
                                       nrow(afdmSampleData()))
      ) %>% 
      filter(omit == FALSE)
    
  })
  
  
  # preview data table with upload and provided values
  output$previewAfdmUpload <- renderTable({
    
    afdmSamplesAndData() %>%
      mutate(sample_datetime = as.character(sample_datetime)) %>%
      select(-omit)
    
  })
  
  
  # write afdm data to database
  observeEvent(input$submitAFDM, {
    
    # # modify data object as needed for the DB
    afdmDataToWrite <- afdmSamplesAndData() %>%
      mutate(sample_datetime = as.POSIXct(sample_datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(comments = ifelse(input$afdmUploadNotes == '', NA, input$afdmUploadNotes)) %>%
      mutate(filter_wt = as.numeric(filter_wt)) %>%
      mutate(filter_dry = as.numeric(filter_dry)) %>%
      mutate(volume = as.numeric(volume)) %>%
      mutate(filter_ashed = as.numeric(filter_ashed)) %>%
      select(-omit)
    
    # beging DB sequence
    
    # establish db connection
    # pg <- stormPool()
    
    # insert into samples
    insertAfdmQuery <-
      'INSERT INTO stormwater.solids
      (
        sample_id,
        filter_initial,
        filter_dry,
        volume_filtered,
        filter_ashed,
        replicate,
        comments
      )
      (
        SELECT
          sample_id,
          filter_wt,
          filter_dry,
          volume,
          filter_ashed,
          1,
          comments
        FROM
        stormwater.solids_temp
      );'
    
    tryCatch({
      
      dbGetQuery(stormPool, "BEGIN TRANSACTION")
      
      # # write new samples to samples_temp
      if (dbExistsTable(stormPool, c('stormwater', 'solids_temp'))) dbRemoveTable(stormPool, c('stormwater', 'solids_temp'))
      dbWriteTable(stormPool, c('stormwater', 'solids_temp'), value = afdmDataToWrite, row.names = F)
      
      # remove timezone type generated by dbWriteTable function
      dbExecute(stormPool,'
            ALTER TABLE stormwater.solids_temp
            ALTER COLUMN sample_datetime TYPE TIMESTAMP WITHOUT TIME ZONE;')
      
      # # execute insert query
      dbExecute(stormPool, insertAfdmQuery)
      
      # clean up
      dbRemoveTable(stormPool, c('stormwater', 'solids_temp'))
      
      dbCommit(stormPool)
      
      showNotification(ui = "successfully uploaded",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'message',
                       action = a(href = "javascript:location.reload();", "reload the page"))
      
    }, warning = function(warn) {
      
      showNotification(ui = paste("there is a warning:  ", warn),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'warning')
      
      print(paste("WARNING: ", warn))
      
    }, error = function(err) {
      
      showNotification(ui = paste("there was an error:  ", err),
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
      print(paste("ERROR: ", err))
      print("ROLLING BACK TRANSACTION")
      
      dbRollback(stormPool)
      
    }) # close try catch
    
    # close db connection
    # dbDisconnect(pg)
    
  })
  
  
  # close server ------------------------------------------------------------
  
}

# Run the application 
shinyApp(ui = ui, server = server)