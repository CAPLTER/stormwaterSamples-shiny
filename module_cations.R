#' @title Module: cations
#' 
#' @description The module cations facilitates uploading cation data. The user
#'   attaches the appropriate sample details to uploaded data. Upon execution,
#'   the munged data with sample and analysis details is written to
#'   stormwater.results upon which, if successful, the imported data are written
#'   to stormwater.icp.


# cations UI --------------------------------------------------------------

# vector of last five year for filtering sample ids
lastFiveYears <- rev(seq(from = as.numeric(format(Sys.Date(),'%Y'))-5,
                         to = as.numeric(format(Sys.Date(),'%Y')),
                         by = 1))

cationsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(
        HTML("#leftPanel { background: #D3D3D3; color: #484848; }")
      ) # close tags$style
    ), # close tagss$head
    fluidPage(
      fluidRow( 
        column(id = 'leftPanel', 2,
               fileInput(inputId = ns("cationFile"),
                         label = "select file",
                         multiple = FALSE),
               br(),
               strong("narrow sample choices",
                      style = "text-align: center; color: black"),
               selectizeInput(inputId = ns("narrowSamplesSite"),
                              "site",
                              choices = siteAbbreviations,
                              selected = NULL,
                              multiple = TRUE),
               selectizeInput(inputId = ns("narrowSampleMonth"),
                              "month",
                              choices = month.abb,
                              selected = NULL,
                              multiple = TRUE),
               selectizeInput(inputId = ns("narrowSampleYear"),
                              "year",
                              choices = lastFiveYears,
                              selected = NULL,
                              multiple = FALSE),
               actionButton("importCationFile",
                            "submit data"),
               br()
        ), # close the left col
        column(id = "dischargeRightPanel", 10,
               strong('cation data'),
               hr(),
               DT::dataTableOutput(ns("rawView"))
        ) # close the right col
      ) # close the row
    ) # close the page
  ) # close tagList
  
} # close cationsUI 


# cations main ------------------------------------------------------------

cations <- function(input, output, session) {
  
  # added to facilitate renderUIs
  ns <- session$ns
  
  # create listener for adding and deleting records
  listener <- reactiveValues(dbVersion = 0)
  
  # build (reactive) list of bottle IDs for given site, year, and month
  samplesSelection <- reactive({
    
    req(
      input$narrowSamplesSite,
      input$narrowSampleMonth,
      input$narrowSampleYear
    )
    
    # integerMonth <- grep(input$narrowSampleMonth, month.abb, ignore.case = TRUE)
    monthTibble <- tibble(number = seq(1:12), abbr = month.abb)
    integerMonths <- glue::glue_sql(
      "{monthTibble[monthTibble$abbr %in% c(input$narrowSampleMonth),]$number*}"
    )
    
    # integerSite <- sampleSites[sampleSites$abbreviation == input$narrowSamplesSite,]$site_id
    integerSites <- glue::glue_sql(
      "{sampleSites[sampleSites$abbreviation %in% input$narrowSamplesSite,]$site_id*}"
    )
    
    baseQuery <- "
    SELECT CONCAT(samples.bottle, '_', samples.sample_datetime)
    FROM stormwater.samples
    WHERE
      samples.site_id IN (?theseSites) AND
      (EXTRACT (MONTH FROM sample_datetime) IN (?theseMonth) AND EXTRACT (YEAR FROM sample_datetime) = ?thisYear)
    ORDER BY
  		EXTRACT (MONTH FROM sample_datetime),
  		samples.bottle;"
    
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         theseSites = integerSites,
                                         theseMonth = integerMonths,
                                         thisYear = input$narrowSampleYear
    )
    
    bottleOptions <- run_interpolated_query(parameterizedQuery)
    
    return(bottleOptions)
    # return(parameterizedQuery)
    
  })
  
  
  rawReactive <- reactive({
    
    req(input$cationFile)
    
    cationsUpload <- read_excel(path = input$cationFile$datapath,
                                skip = 4)
    
    colnames(cationsUpload) <- tolower(colnames(cationsUpload)) # colnames to lowercase
    colnames(cationsUpload) <- gsub("\\.", "\\_", colnames(cationsUpload)) # replace dots with underscores
    
    colnames(cationsUpload)[1] <- "date_analyzed"
    colnames(cationsUpload)[2] <- "temp_out_id"
    colnames(cationsUpload)[3] <- "operator"
    colnames(cationsUpload)[4] <- "icp_id"
    
    cationsUpload <- cationsUpload %>%
      mutate(
        sampleID = shinyInputOther(FUN = selectInput,
                                   len = nrow(cationsUpload),
                                   id = 'sampID_',
                                   choices=samplesSelection(),
                                   width = "220px"),
        omit = shinyInputOther(checkboxInput,
                               nrow(cationsUpload),
                               "omit_",
                               value = FALSE,
                               width = "20px"),
        replicate = shinyInputOther(FUN = selectInput,
                                    len = nrow(cationsUpload),
                                    id = 'rep_',
                                    choices=c(1,2,3),
                                    width = "40px")
      ) %>% 
      select(sampleID, omit, replicate, everything())
    
    return(cationsUpload) 
    
  })
  
  output$rawView <- DT::renderDataTable({
    
    rawReactive()
    
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
  rownames = F) # close output$rawView
  
  # # queryType: default vs parameterized query for transects
  # queryType <- reactiveValues(default = "default")
  # 
  # # actionButton filterSamples = parameterized query type
  # observeEvent(input$filterSamples, {
  #   
  #   queryType$default <- "param"
  #   
  # })
  # 
  # # query samples data
  # samplesDataReactive <- reactive({
  #   
  #   # add listener for adding and deleting records
  #   listenModifySamples$dbVersion
  #   
  #   if (queryType$default == "default") {
  #     
  #     samplesData <- query_samples_default()
  #     
  #   } else {
  #     
  #     # parameters cannot be passed to function directly
  #     filterStart <- as.character(input$viewSamplesStartDate)
  #     filterEnd <- as.character(input$viewSamplesEndDate)
  #     filterSite <- input$viewSamplesSite
  #     
  #     # run query with params
  #     samplesData <- query_samples_site_date(start = filterStart,
  #                                            end = filterEnd,
  #                                            site = filterSite)
  #     
  #   }
  #   
  #   if (nrow(samplesData) == 0) {
  #     
  #     samplesData <- data.frame(
  #       id = NA,
  #       site = NA,
  #       sample_datetime = NA,
  #       comments = as.character("match not found"),
  #       temp = NA,
  #       pH = NA,
  #       cond = NA,
  #       bottle = NA,
  #       doc_vial_id = NA,
  #       afdm_bottle_id = NA)
  #     
  #   } else {
  #     
  #     # add delete button to samples data
  #     # remember that session$ns is required for modules!!!
  #     samplesData <- samplesData %>%
  #       mutate(delete = shinyInput(reactiveObject = samplesData,
  #                                  FUN = actionButton,
  #                                  len = nrow(samplesData),
  #                                  id = '',
  #                                  label = "delete",
  #                                  onclick = sprintf('Shiny.setInputValue("%s",  this.id)', session$ns("button_delete_sample")))
  #       )
  #     
  #   }
  #   
  #   return(samplesData)
  #   
  # })
  # 
  # # render editable table of samples data
  # output$samplesDataView <- DT::renderDT({
  #   
  #   samplesDataReactive()
  #   
  # },
  # escape = FALSE,
  # selection = "none",
  # rownames = FALSE,
  # editable = list(target = 'cell',
  #                 disable = list(columns = c(0,1,2,7,8,9,10))),
  # options = list(bFilter = 0,
  #                bLengthChange = FALSE,
  #                bPaginate = FALSE,
  #                bSort = FALSE,
  #                autoWidth = TRUE,
  #                columnDefs = list(list(width = '100px', targets = c(1)))
  # )
  # ) # close output$samplesDataView
  # 
  # 
  # # add new stormwater sample -----------------------------------------------
  # 
  # # generate list of possible bottle IDs based on site selected
  # output$bottleID = renderUI({
  #   
  #   siteNum <- sampleSites %>% 
  #     filter(abbreviation == input$newSampleSite) %>% 
  #     pull(site_id)
  #   
  #   bottleOptions <- bottleList %>%
  #     filter(str_extract(bottles, '^[0-9]+') == siteNum)
  #   
  #   selectizeInput(ns('bottleIdentity'),
  #                  'bottle',
  #                  choices = bottleOptions)
  #   
  # })
  # 
  # 
  # # function: addNewSample - write new sample to the database
  # addNewSample <- function(sampleSite,
  #                          sampleDate,
  #                          sampleTime,
  #                          sampleBottle) {
  #   
  #   siteCode <- sampleSites %>%
  #     filter(abbreviation == sampleSite) %>%
  #     pull(site_id)
  #   
  #   dateTime <- paste(sampleDate, sampleTime)
  #   
  #   baseQuery <- "
  #   INSERT INTO stormwater.samples
  #   (
  #     site_id,
  #     sample_datetime,
  #     bottle,
  #     doc_vial_id,
  #     afdm_bottle_id
  #   )
  #   VALUES
  #   (
  #     ?newSite,
  #     ?newDateTime,
  #     ?newBottle,
  #     ?newBottle,
  #     ?newBottle
  #   );"
  #   
  #   parameterizedQuery <- sqlInterpolate(ANSI(),
  #                                        baseQuery,
  #                                        newSite = siteCode,
  #                                        newDateTime = dateTime,
  #                                        newBottle = sampleBottle
  #   )
  #   
  #   run_interpolated_execution(parameterizedQuery)
  #   
  #   # change listener state when adding a record
  #   listenModifySamples$dbVersion <- isolate(listenModifySamples$dbVersion + 1)
  #   
  # }
  # 
  # # add a new reach extent measure
  # observeEvent(input$newSampleGo, {
  #   
  #   # designate requirements for this event
  #   req(input$newSampleSite)
  #   req(input$bottleIdentity)
  #   req(input$newSampleDate)
  #   req(input$newSampleTime)
  #   
  #   # parameters cannot be passed to function directly
  #   newSampleSite <- input$newSampleSite
  #   bottleID <- input$bottleIdentity
  #   newSampleDate <- as.character(input$newSampleDate)
  #   newSampleTime <- input$newSampleTime
  #   
  #   tryCatch({
  #     
  #     # call function
  #     addNewSample(sampleSite = newSampleSite,
  #                  sampleDate = newSampleDate,
  #                  sampleTime = newSampleTime,
  #                  sampleBottle = bottleID)
  #     
  #     # reset query type
  #     queryType$default <- "default"
  #     
  #   }, warning = function(warn) {
  #     
  #     showNotification(ui = paste("there is a warning:  ", warn),
  #                      duration = NULL,
  #                      closeButton = TRUE,
  #                      type = 'warning')
  #     
  #     print(paste("WARNING: ", warn))
  #     
  #   }, error = function(err) {
  #     
  #     showNotification(ui = paste("there was an error:  ", err),
  #                      duration = NULL,
  #                      closeButton = TRUE,
  #                      type = 'error')
  #     
  #   }) # close try catch
  #   
  # }) # close add a new reach extent measure
  # 
  # 
  # # delete sample -----------------------------------------------------------
  # 
  # # function: deleteSample - delete selected sample from database
  # deleteSample <- function(row_to_delete) {
  #   
  #   baseQuery <- '
  #   DELETE FROM stormwater.samples
  #   WHERE samples.sample_id = ?RWE_ID;'
  #   
  #   parameterizedQuery <- sqlInterpolate(ANSI(),
  #                                        baseQuery,
  #                                        RWE_ID = as.numeric(row_to_delete))
  #   
  #   run_interpolated_execution(parameterizedQuery)
  #   
  #   # change listener state when deleting a record
  #   listenModifySamples$dbVersion <- isolate(listenModifySamples$dbVersion + 1)
  #   
  # }
  # 
  # # call function::deleteSample - delete prescribed sample from the database
  # observeEvent(input$button_delete_sample, {
  #   
  #   deleteSample(row_to_delete = input$button_delete_sample)
  #   
  # })
  # 
  # 
  # # update reach extent measure ---------------------------------------------
  # 
  # # function: updateSample - write edited cell change to the database
  # updateSample <- function(reactiveData, cellEdited) {
  #   
  #   reactiveDataColNames <- as.list(colnames(reactiveData))
  #   editedColumn <- reactiveDataColNames[cellEdited$col + 1]
  #   editedRow <- reactiveData[cellEdited$row, ][['id']]
  #   newValue <- cellEdited[['value']]
  #   
  #   # change temp, ph, cond to appropriate data type
  #   if (grepl("temp|pH|cond", editedColumn, ignore.case = TRUE)) {
  #     newValue <- as.numeric(newValue)
  #   }
  #   
  #   # recast temp, ph, cond from simple names to names per DB schema
  #   if (grepl("temp", editedColumn)) {
  #     editedColumn <- "lab_temperature"
  #   }
  #   
  #   if (grepl("pH", editedColumn, ignore.case = T)) {
  #     editedColumn <- "\"lab_pH\""
  #   }
  #   
  #   if (grepl("cond", editedColumn)) {
  #     editedColumn <- "lab_conductance"
  #   }
  #   
  #   # inexplicable behaviour in this case where the edited column is quoted,
  #   # which is not permissible in postgres; use SQL quoting to obtain proper
  #   # formatting
  #   editedColumn <- SQL(editedColumn) 
  #   
  #   baseQuery <- '
  #   UPDATE stormwater.samples
  #   SET ?editedCol = ?updatedValue
  #   WHERE sample_id = ?tuple;'
  #   
  #   parameterizedQuery <- sqlInterpolate(ANSI(),
  #                                        baseQuery,
  #                                        editedCol = editedColumn,
  #                                        updatedValue = newValue,
  #                                        tuple = editedRow)
  #   
  #   run_interpolated_execution(parameterizedQuery)
  #   
  # }
  # 
  # 
  # # call function updateSample - write edited cell change to the database
  # observeEvent(input$samplesDataView_cell_edit, {
  #   
  #   tryCatch({
  #     
  #     updateSample(reactiveData = samplesDataReactive(),
  #                  cellEdited = input$samplesDataView_cell_edit)
  #     
  #   }, warning = function(warn) {
  #     
  #     showNotification(ui = paste("there is a warning:  ", warn),
  #                      duration = NULL,
  #                      closeButton = TRUE,
  #                      type = 'warning')
  #     
  #     print(paste("WARNING: ", warn))
  #     
  #   }, error = function(err) {
  #     
  #     showNotification(ui = paste("there was an error:  ", err),
  #                      duration = NULL,
  #                      closeButton = TRUE,
  #                      type = 'error')
  #     
  #   }) # close try catch
  #   
  # })
  
  # debugging: module level -------------------------------------------------
  
  ############# START debugging
  observe(print({ samplesSelection() }))
  # observe(print({ queryType$default }))
  # observe(print({ input$ReachPatchs_cell_edit }))
  ############# END debugging
  
  
  # close module modifySamples ----------------------------------------------
  
} # close module::modifySamples