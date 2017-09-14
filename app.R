
# libraries
library(shiny)
library(dplyr)
library(readr)
library(DT)
library(stringr)
library(lubridate)
library(DBI)
library(RPostgreSQL)

# Note regarding Javascript
# There are several code chunks that have the comment 'notable stmt' associated
# with them. These statements are related to the Javascript functionality
# required for the interactivity of a file upload, i.e., adding temp, cond, etc.
# to the table following a file uplooad. See
# https://stackoverflow.com/questions/40020600/r-shiny-selectedinput-inside-renderdatatable-cells
# and
# https://groups.google.com/forum/#!msg/shiny-discuss/ZUMBGGl1sss/zfcG9c6MBAAJ
# for details.

# Note regarding DOC and AFDM identifiers
# This app was built initially with the idea that bottle/vial IDs for DOC and
# AFDM would be entered by the technicians. However, the technician team moved
# to an approach of simply labelling both DOC and AFDM bottles with the bottle
# ID corresponding to a sample (e.g., 11_2_2). The functionality for adding DOC
# and AFDM IDs was left in the code but simply commented out of execution, and
# it should be possible to simply uncomment those code chunks and change the
# database statements if there is interest to revert.

# generate list of bottle IDs for manual data entry
# MOVE THIS TO SERVER IF YOU SPLIT THE APP INTO TWO FILES
bottleList <- read_csv('allPossibleBottleCombinations.csv',
                       col_names = TRUE)

# ui ----
ui <- tagList(
  tags$head(
    tags$style(
      HTML("#leftPanel { background: #D3D3D3; }"),
      HTML("#submitFileUpload { background-color: #800000;
                          color: white; }"),
      HTML("#addSample {  border-style: solid;
                          border-color: #0000ff; }"),
      HTML("#deleteManualEntryRow { border-style: solid;
                                    border-color: #ff0000 }"),
      HTML("#submitEnteredData {  background-color: #800000;
                                  color: white; }"),
      HTML("#submitDischargeFileUpload {  background-color: #800000;
                                          color: white; }"),
      HTML("#submitAFDM { background-color: #800000;
                          color: white; }")
    ) # close tags$head
  ), # close tagss$style
  navbarPage("stormwater",
             tabPanel("samples: file upload",
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
                                               "notes",
                                               resize = 'vertical',
                                               value = NULL),
                                 actionButton("submitFileUpload",
                                              "submit data"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "fileUploadRightPanel", 10,
                                 DT::dataTableOutput("buttonsInTable"),
                                 tags$script(HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
          Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());
                            })")), # notable stmt
                                 hr(),
                                 strong('data preview'),
                                 tableOutput("checked")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # clase Salt River tab panel
             
             tabPanel("samples: manual entry",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 3,
                                 selectizeInput("siteID",
                                                "Site ID",
                                                choices = c("IBW",
                                                            "centralNorth",
                                                            "centralSouth",
                                                            "Price"),
                                                selected = NULL,
                                                multiple = FALSE),
                                 uiOutput("bottleID"),
                                 dateInput("sampleDate",
                                           "sample date",
                                           format = "yyyy-mm-dd"),
                                 textInput(inputId = "time",
                                           label = "time",
                                           value = "00:00:00",
                                           placeholder = "HH:MM:SS"),
                                 # textInput("DOC",
                                 #           "DOC"),
                                 numericInput("temp",
                                              "temp",
                                              value = NULL,
                                              min = 0),
                                 numericInput("cond",
                                              "cond",
                                              value = NULL,
                                              min = 0),
                                 # textInput("AFDM",
                                 #           "AFDM"),
                                 br(),
                                 actionButton("addSample",
                                              "add sample"),
                                 br(),
                                 hr(),
                                 strong("delete a sample?"),
                                 br(),
                                 uiOutput("deletableBottles"),
                                 actionButton(inputId = "deleteManualEntryRow",
                                              label = "delete selected sample"),
                                 hr(),
                                 textAreaInput("manualEntryNotes",
                                               "notes",
                                               resize = 'vertical',
                                               value = NULL),
                                 actionButton("submitEnteredData",
                                              "submit data"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "manualEntryRightPanel", 9,
                                 tableOutput('manualEntryTable')                                    
                          ) # close the right col
                        ) # close the row
                      ) # close the page        
             ), # close manual entry tab panel
             
             tabPanel("samples: viewer",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 selectizeInput("samplesViewerSiteID",
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
                                 dateInput("samplesStartDate",
                                           "start:",
                                           format = "yyyy-mm-dd"),
                                 dateInput("samplesEndDate",
                                           "end:",
                                           format = "yyyy-mm-dd"),
                                 actionButton(inputId = "querySamples",
                                              label = "get samples",
                                              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
                                 br(),
                                 br()
                          ), # close the left col
                          column(id = "samplesViewerRightPanel", 10,
                                 DT::dataTableOutput("samplesData")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # close sampels viewer tab panel
             
             tabPanel("discharge",
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
             ), # close discharge tab panel
             
             tabPanel("AFDM",
                      fluidPage(
                        fluidRow( 
                          column(id = 'leftPanel', 2,
                                 selectizeInput("afdmSiteID",
                                                "Site ID",
                                                choices = c("IBW",
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
             
  ) # close navbar/page
) # close tagList


# server ----
server <- function(input, output, session) {
  
  # file upload ----
  
  # helper function for reading checkbox
  shinyInput = function(FUN, len, id, ...) { 
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
                        col_names = c("sample_datetime", "eventNumber"),
                        col_types = list(
                          sample_datetime = col_datetime(format = "%m/%d/%Y %H:%M"),
                          eventNumber = col_integer()
                        )) %>% 
      dplyr::select(sample_datetime) %>% 
      dplyr::filter(!is.na(sample_datetime)) %>% 
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
             col_names = c("sample_datetime", "eventNumber"),
             col_types = list(
               sample_datetime = col_datetime(format = "%m/%d/%Y %H:%M"),
               eventNumber = col_integer()
             )) %>% 
      mutate(bottle = paste0(sampleReportSiteId(), "_", input$storm, "_", eventNumber)) %>%
      dplyr::select(bottle, sample_datetime) %>% 
      dplyr::filter(!is.na(sample_datetime)) %>% 
      add_row(bottle = paste0(sampleReportSiteId(), "_", input$storm, "_BLK"),
              sample_datetime = sampleReportMaxDate()) %>% 
      mutate(sample_datetime = format(sample_datetime, "%Y-%m-%d %H:%M:%S"))
    
  })

  
  # render the file upload and data input tools
  output$buttonsInTable <- DT::renderDataTable({
    
    sampleReportData() %>% 
      mutate(omit = shinyInput(checkboxInput,
                               nrow(sampleReportData()),
                               "omit_",
                               value = FALSE,
                               width = "20px"),
             # DOC = shinyInput(textInput,
             #                  nrow(sampleReportData()),
             #                  "doc_",
             #                  # value = 'NA',
             #                  width = "90px"),
             temp = shinyInput(numericInput,
                               nrow(sampleReportData()),
                               "temp_",
                               value = NULL,
                               min = 0,
                               width = "90px"),
             cond = shinyInput(numericInput,
                               nrow(sampleReportData()),
                               "cond_",
                               value = NULL,
                               min= 0,
                               width = "90px") #,
             # AFDM = shinyInput(textInput,
             #                   nrow(sampleReportData()),
             #                   "afdm_",
             #                   # value = NULL,
             #                   width = "90px")
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
      mutate(omit = shinyValue("omit_",
                               nrow(sampleReportData())),
             # DOC = shinyValue("doc_",
             #                  nrow(sampleReportData())),
             temp = shinyValue('temp_',
                               nrow(sampleReportData())),
             cond = shinyValue('cond_',
                               nrow(sampleReportData())) # ,
             # AFDM = shinyValue('afdm_',
             #                   nrow(sampleReportData()))
      ) %>% 
      filter(omit == FALSE) # %>% 
      # mutate(DOC = replace(DOC, DOC == '', NA)) %>% 
      # mutate(AFDM = replace(AFDM, AFDM == '', NA))
    
  })
  
  
  # preview data table with upload and provided values
  output$checked <- renderTable({
    
    combinedData() %>% 
      mutate(sample_datetime = as.character(sample_datetime)) %>% 
      select(-omit)
    
  })
  
  
  # write uploaded file and edits to database
  observeEvent(input$submitFileUpload, {
    
    # modify data object as needed for the DB
    sampleUploadToWrite <- combinedData() %>% 
      mutate(sample_datetime = as.POSIXct(sample_datetime, format = "%Y-%m-%d %H:%M:%S")) %>% 
      mutate(site_id = sampleReportSiteId()) %>% 
      mutate(comments = ifelse(input$fileUploadNotes == '', NA, input$fileUploadNotes)) %>%
      mutate(temp = as.numeric(temp)) %>% 
      mutate(cond = as.numeric(cond)) %>% 
      select(-omit)
    
    # beging DB sequence
    
    # establish db connection
    pg <- databaseConn()
    
    # insert into samples
    insertSamplesUploadQuery <-
      'INSERT INTO stormwater.samples
        (
          site_id,
          sample_datetime,
          comments,
          lab_temperature,
          lab_conductance,
          bottle,
          doc_vial_id,
          afdm_bottle_id
        )
        (
          SELECT
            site_id,
            sample_datetime,
            comments,
            temp,
            cond,
            bottle,
            bottle,
            bottle
          FROM
          stormwater.samples_temp
        );'
    
    tryCatch({
      
      dbGetQuery(pg, "BEGIN TRANSACTION")
      
      # # write new samples to samples_temp
      if (dbExistsTable(pg, c('stormwater', 'samples_temp'))) dbRemoveTable(pg, c('stormwater', 'samples_temp'))
      dbWriteTable(pg, c('stormwater', 'samples_temp'), value = sampleUploadToWrite, row.names = F)
      
      # remove timezone type generated by dbWriteTable function
      dbExecute(pg,'
            ALTER TABLE stormwater.samples_temp
            ALTER COLUMN sample_datetime TYPE TIMESTAMP WITHOUT TIME ZONE;')
      
      # execute insert query
      dbExecute(pg, insertSamplesUploadQuery)
      
      # clean up
      dbRemoveTable(pg, c('stormwater', 'samples_temp'))
      
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

  
# manual data entry ----
  
  # function to translate text site id to numeric site id (as in the DB)
  dbSiteId <- reactive({
    
    if (input$siteID == 'IBW') {
      numericSiteCode <- 11
    } else if (input$siteID == 'centralNorth') {
      numericSiteCode <- 14
    } else if (input$siteID == 'centralSouth') {
      numericSiteCode <- 15
    } else if (input$siteID == 'Price') {
      numericSiteCode <- 16
    } 
    
    return(numericSiteCode)
    
  })
  
  
  # generate list of possible bottle IDs based on site selected
  output$bottleID = renderUI({
    
    bottleOptions <- bottleList %>% 
      filter(str_extract(bottles, '^[0-9]+') == dbSiteId()) %>% 
      filter(!bottles %in% c(manualEntry$df$bottle))
    
    selectizeInput('bottleIdentity', 
                   'bottle', 
                   choices = bottleOptions)
    
  })
  
  
  # create an empty data frame to hold entered data
  manualEntry <- reactiveValues()
  
  manualEntry$df <- data.frame(site_id = NA,
                               bottle = NA,
                               sample_datetime = NA,
                               temp = NA,
                               cond = NA
                               )
  
  
  # observe manual data entry via `addSample` button and new data to data frame
  newManualEntry <- observe({
    
    if (input$addSample > 0) {
      
      newLine <- isolate(c(dbSiteId(),
                           input$bottleIdentity,
                           paste(input$sampleDate, input$time),
                           input$temp,
                           input$cond
      ))
      
      isolate(manualEntry$df <- rbind(manualEntry$df,
                                      newLine))
    }
    
  })
  
  
  # generate a preview table of entered data
  output$manualEntryTable <- renderTable({
    
    manualEntry$df %>% 
      filter(!is.na(bottle))
    
  })
 
  
  # generate a list of possible bottle IDs in the current table to delete
  output$deletableBottles = renderUI({
    
    bottlesThatCouldBeDeleted <- manualEntry$df %>% 
      filter(!is.na(bottle)) %>% 
      select(bottle)
    
    selectInput(inputId = 'bottlesToDelete', 
                label = NULL, 
                choices = bottlesThatCouldBeDeleted)
    
  })
 
  
  # allow user to delete a row based on bottle id
  observeEvent(input$deleteManualEntryRow, {
    
    isolate(
      manualEntry$df <- manualEntry$df %>%
        filter(bottle != input$bottlesToDelete)
    )
    
  })
  
  
  # write manually entered data to database
  observeEvent(input$submitEnteredData, {

    # modify data object as needed for the DB
    manualEntryToWrite <- manualEntry$df %>%
      filter(!is.na(bottle)) %>% 
      mutate(sample_datetime = as.POSIXct(sample_datetime, format = "%Y-%m-%d %H:%M:%S")) %>%
      mutate(comments = ifelse(input$manualEntryNotes == '', NA, input$manualEntryNotes)) %>%
      mutate(temp = as.numeric(temp)) %>%
      mutate(cond = as.numeric(cond)) %>%
      mutate(site_id = as.integer(site_id))

    # beging DB sequence

    # establish db connection
    pg <- databaseConn()

    # insert into samples
    insertSamplesUploadQuery <-
      'INSERT INTO stormwater.samples
        (
          site_id,
          sample_datetime,
          comments,
          lab_temperature,
          lab_conductance,
          bottle,
          doc_vial_id,
          afdm_bottle_id
        )
        (
          SELECT
            site_id,
            sample_datetime,
            comments,
            temp,
            cond,
            bottle,
            bottle,
            bottle
          FROM
          stormwater.samples_temp
        );'

    tryCatch({

      dbGetQuery(pg, "BEGIN TRANSACTION")

      # # write new samples to samples_temp
      if (dbExistsTable(pg, c('stormwater', 'samples_temp'))) dbRemoveTable(pg, c('stormwater', 'samples_temp'))
      dbWriteTable(pg, c('stormwater', 'samples_temp'), value = manualEntryToWrite, row.names = F)

      # remove timezone type generated by dbWriteTable function
      dbExecute(pg,'
            ALTER TABLE stormwater.samples_temp
            ALTER COLUMN sample_datetime TYPE TIMESTAMP WITHOUT TIME ZONE;')

      # execute insert query
      dbExecute(pg, insertSamplesUploadQuery)

      # clean up
      dbRemoveTable(pg, c('stormwater', 'samples_temp'))

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
  
  
# samples viewer ----
  
  # function to translate text site id to numeric site id (as in the DB)
  samplesViewerSiteId <- reactive({
    
    if (input$samplesViewerSiteID == 'IBW') {
      numericSiteCode <- 11
    } else if (input$samplesViewerSiteID == 'centralNorth') {
      numericSiteCode <- 14
    } else if (input$samplesViewerSiteID == 'centralSouth') {
      numericSiteCode <- 15
    } else if (input$samplesViewerSiteID  == 'price') {
      numericSiteCode <- 16
    } else if (input$samplesViewerSiteID  == 'kiwanisPark') {
      numericSiteCode <- 1
    } else if (input$samplesViewerSiteID  == 'camelback') {
      numericSiteCode <- 2
    } else if (input$samplesViewerSiteID  == 'pierce') {
      numericSiteCode <- 4
    } else if (input$samplesViewerSiteID  == 'martinResidence') {
      numericSiteCode <- 5
    } else if (input$samplesViewerSiteID  == 'montessori') {
      numericSiteCode <- 6
    } else if (input$samplesViewerSiteID  == 'sweetwater') {
      numericSiteCode <- 7
    } else if (input$samplesViewerSiteID  == 'bellaVista') {
      numericSiteCode <- 8
    } else if (input$samplesViewerSiteID  == 'lakeMarguerite') {
      numericSiteCode <- 9
    } else if (input$samplesViewerSiteID  == 'silveradoGolfCourse') {
      numericSiteCode <- 10
    } else if (input$samplesViewerSiteID  == 'encantada') {
      numericSiteCode <- 12
    } else if (input$samplesViewerSiteID  == 'ave7th') {
      numericSiteCode <- 13
    } 
    
    return(numericSiteCode)
    
  })
  
  # query sample data from database for viewing
  viewerSamplesData <- reactive({
    
    req(input$querySamples)
    
    pg <- databaseConn()
    
    baseSamplesDataQuery <- '
    SELECT
      samples.sample_id,
      samples.site_id,
      samples.sample_datetime,
      samples.bottle,
      samples.lab_temperature as temp,
      samples.lab_conductance as cond,
      samples.comments,
      samples.afdm_bottle_id as afdm_bottle,
      solids.filter_initial,
      solids.filter_dry,
      solids.volume_filtered,
      solids.filter_ashed
    FROM stormwater.samples
    LEFT JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
    WHERE
      samples.site_id = ?siteId AND
    	samples.sample_datetime BETWEEN ?start AND ?end
    ORDER BY samples.sample_datetime ASC;'
    
    isolate(
      samplesDataQuery <- sqlInterpolate(ANSI(),
                                         baseSamplesDataQuery,
                                         siteId = samplesViewerSiteId(),
                                         start = as.character(input$samplesStartDate),
                                         end = as.character(input$samplesEndDate))
    )
    
    stormSamples <- dbGetQuery(pg,
                               samplesDataQuery)
    
    dbDisconnect(pg)
    
    
    # rather than a simple return, we need to address conditions when there
    # are not any data that match the search criteria
    
    # format the date if there are data
    if (nrow(stormSamples) >= 1) {
      
      stormSamples <- stormSamples %>% 
        mutate(sample_datetime = format(sample_datetime, "%Y-%m-%d %H:%M:%S"))
      
    # else build an empty frame with just NAs
    } else {
      
      stormSamples <- data.frame(
        sample_id = "no samples match those criteria"
      )
      
    }
    
    return(stormSamples)
    
  }) 
  
  
  # render the afdm samples and data entry fields
  output$samplesData <- DT::renderDataTable({
    
    viewerSamplesData()
    
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(paging = F),
  rownames = F) # close renderDataTable
  
  
# discharge ----
  
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
             col_names = c("event_datetime", "level"),
             col_types = list(
               event_datetime = col_datetime(format = "%m/%d/%Y %H:%M"),
               level = col_double()
             )) %>% 
      dplyr::filter(!is.na(event_datetime)) %>% 
      mutate(event_datetime = format(event_datetime, "%Y-%m-%d %H:%M:%S")) %>% 
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
    pg <- databaseConn()

    # insert into samples
    insertLevelDataQuery <-
      'INSERT INTO stormwater.discharge
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
        );'

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
  
  
# afdm ----
  
  # function to translate text site id to numeric site id (as in the DB)
  dbAfdmSiteId <- reactive({
    
    if (input$afdmSiteID == 'IBW') {
      numericSiteCode <- 11
    } else if (input$afdmSiteID == 'centralNorth') {
      numericSiteCode <- 14
    } else if (input$afdmSiteID == 'centralSouth') {
      numericSiteCode <- 15
    } else if (input$afdmSiteID == 'Price') {
      numericSiteCode <- 16
    } 
    
    return(numericSiteCode)
    
  })
  
  
  # query sample-afdm data from database
  afdmSampleData <- reactive({
    
    req(input$querySamplesAFDM)
    
    session$sendCustomMessage('unbind-DT', 'afdmDataEntryTable') # notable stmt
    
    pg <- databaseConn()
    
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
    
    afdmStormSamples <- dbGetQuery(pg,
                                   samplesDataQuery)
    
    dbDisconnect(pg)
    
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
      mutate(omit_sample = shinyInput(checkboxInput,
                                      nrow(afdmSampleData()),
                                      "omit_afdm_sample_",
                                      value = FALSE,
                                      width = "20px"),
             filter_wt = shinyInput(numericInput,
                                    nrow(afdmSampleData()),
                                    "filter_wt_",
                                    value = NULL,
                                    min = 0,
                                    width = "90px"),
             filter_dry = shinyInput(numericInput,
                                     nrow(afdmSampleData()),
                                     "filter_dry_",
                                     value = NULL,
                                     min = 0,
                                     width = "90px"),
             volume = shinyInput(numericInput,
                                 nrow(afdmSampleData()),
                                 "volume_",
                                 value = NULL,
                                 min = 0,
                                 width = "90px"),
             filter_ashed = shinyInput(numericInput,
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
    pg <- databaseConn()

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

      dbGetQuery(pg, "BEGIN TRANSACTION")

      # # write new samples to samples_temp
      if (dbExistsTable(pg, c('stormwater', 'solids_temp'))) dbRemoveTable(pg, c('stormwater', 'solids_temp'))
      dbWriteTable(pg, c('stormwater', 'solids_temp'), value = afdmDataToWrite, row.names = F)

      # remove timezone type generated by dbWriteTable function
      dbExecute(pg,'
            ALTER TABLE stormwater.solids_temp
            ALTER COLUMN sample_datetime TYPE TIMESTAMP WITHOUT TIME ZONE;')

      # # execute insert query
      dbExecute(pg, insertAfdmQuery)

      # clean up
      dbRemoveTable(pg, c('stormwater', 'solids_temp'))

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
  

} # close server


# Run the application 
shinyApp(ui = ui, server = server)