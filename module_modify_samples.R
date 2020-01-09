#' @title Module: modifySamples
#' 
#' @description The module modifySamples facilitates viewing stormwater samples,
#'   adding new samples, editing data (temp, cond, etc), and deleting existing
#'   records.

# modify reach extent UI --------------------------------------------------

modifySamplesUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(
        HTML("#samplesLeftPanel { background: #D3D3D3; color: #484848; }")
      ) # close tags$style
    ), # close tagss$head
    fluidPage(
      fluidRow( 
        column(id = 'samplesLeftPanel', 2,
               # filter existing
               strong("filter samples",
                      style = "text-align: center; color: black"),
               selectizeInput(inputId = ns("viewSamplesSite"),
                              "site",
                              choices = siteAbbreviations,
                              selected = NULL,
                              multiple = FALSE),
               br(),
               dateInput(inputId = ns("viewSamplesStartDate"),
                         "start:",
                         format = "yyyy-mm-dd"),
               dateInput(inputId = ns("viewSamplesEndDate"),
                         "end:",
                         format = "yyyy-mm-dd"),
               actionButton(inputId = ns("filterSamples"),
                            label = "view samples",
                            style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
               hr(),
               # add new
               strong("add new",
                      style = "text-align: center; color: black"),
               selectizeInput(inputId = ns("newSampleSite"),
                              "site",
                              choices = siteAbbreviations,
                              selected = NULL,
                              multiple = FALSE),
               uiOutput(ns("bottleID")),
               dateInput(inputId = ns("newSampleDate"),
                         "sample date",
                         format = "yyyy-mm-dd"),
               textInput(inputId = ns("newSampleTime"),
                         label = "time",
                         value = "00:00:00",
                         placeholder = "HH:MM:SS"),
               br(),
               actionButton(inputId = ns("newSampleGo"),
                            label = "add sample",
                            style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
        ), # close the left col
        column(id = "samplesRightPanel", 10,
               DT::DTOutput(ns("samplesDataView"))
        ) # close the right col
      ) # close the row
    ) # close the page
  ) # close tagList
  
} # close modifySamplesUI


# modify reach patches main -----------------------------------------------

modifySamples <- function(input, output, session, samplesID) {
  
  # added to facilitate renderUIs
  ns <- session$ns
  
  # create listener for adding and deleting records
  listenModifySamples <- reactiveValues(dbVersion = 0)
  
  # queryType: default vs parameterized query for transects
  queryType <- reactiveValues(default = "default")
  
  # actionButton filterSamples = parameterized query type
  observeEvent(input$filterSamples, {
    
    queryType$default <- "param"
    
  })
  
  # query samples data
  samplesDataReactive <- reactive({
    
    # add listener for adding and deleting records
    listenModifySamples$dbVersion
    
    if (queryType$default == "default") {
      
      samplesData <- query_samples_default()
      
    } else {
      
      # parameters cannot be passed to function directly
      filterStart <- as.character(input$viewSamplesStartDate)
      filterEnd <- as.character(input$viewSamplesEndDate)
      filterSite <- input$viewSamplesSite
      
      # run query with params
      samplesData <- query_samples_site_date(start = filterStart,
                                             end = filterEnd,
                                             site = filterSite)
      
    }
    
    if (nrow(samplesData) == 0) {
      
      samplesData <- data.frame(
        id = NA,
        site = NA,
        sample_datetime = NA,
        comments = as.character("match not found"),
        temp = NA,
        pH = NA,
        cond = NA,
        bottle = NA,
        doc_vial_id = NA,
        afdm_bottle_id = NA)
      
    } else {
      
      # add delete button to samples data
      # remember that session$ns is required for modules!!!
      samplesData <- samplesData %>%
        mutate(delete = shinyInput(reactiveObject = samplesData,
                                   FUN = actionButton,
                                   len = nrow(samplesData),
                                   id = '',
                                   label = "delete",
                                   onclick = sprintf('Shiny.setInputValue("%s",  this.id)', session$ns("button_delete_sample")))
        )
      
    }
    
    return(samplesData)
    
  })
  
  # render editable table of samples data
  output$samplesDataView <- DT::renderDT({
    
    samplesDataReactive()
    
  },
  escape = FALSE,
  selection = "none",
  rownames = FALSE,
  editable = list(target = 'cell',
                  disable = list(columns = c(0,1,2,7,8,9,10))),
  options = list(bFilter = 0,
                 bLengthChange = FALSE,
                 bPaginate = FALSE,
                 bSort = FALSE,
                 autoWidth = TRUE,
                 columnDefs = list(list(width = '100px', targets = c(1)))
  )
  ) # close output$samplesDataView
  
  
  # add new stormwater sample -----------------------------------------------
  
  # generate list of possible bottle IDs based on site selected
  output$bottleID = renderUI({
    
    siteNum <- sampleSites %>% 
      filter(abbreviation == input$newSampleSite) %>% 
      pull(site_id)
    
    bottleOptions <- bottleList %>%
      filter(str_extract(bottles, '^[0-9]+') == siteNum)
    
    selectizeInput(ns('bottleIdentity'),
                   'bottle',
                   choices = bottleOptions)
    
  })
  
  
  # function: addNewSample - write new sample to the database
  addNewSample <- function(sampleSite,
                           sampleDate,
                           sampleTime,
                           sampleBottle) {
    
    siteCode <- sampleSites %>%
      filter(abbreviation == sampleSite) %>%
      pull(site_id)
    
    dateTime <- paste(sampleDate, sampleTime)
    
    baseQuery <- "
    INSERT INTO stormwater.samples
    (
      site_id,
      sample_datetime,
      bottle,
      doc_vial_id,
      afdm_bottle_id
    )
    VALUES
    (
      ?newSite,
      ?newDateTime,
      ?newBottle,
      ?newBottle,
      ?newBottle
    );"
    
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         newSite = siteCode,
                                         newDateTime = dateTime,
                                         newBottle = sampleBottle
    )
    
    run_interpolated_execution(parameterizedQuery)
    
    # change listener state when adding a record
    listenModifySamples$dbVersion <- isolate(listenModifySamples$dbVersion + 1)
    
  }
  
  # add a new reach extent measure
  observeEvent(input$newSampleGo, {
    
    # designate requirements for this event
    req(input$newSampleSite)
    req(input$bottleIdentity)
    req(input$newSampleDate)
    req(input$newSampleTime)
    
    # parameters cannot be passed to function directly
    newSampleSite <- input$newSampleSite
    bottleID <- input$bottleIdentity
    newSampleDate <- as.character(input$newSampleDate)
    newSampleTime <- input$newSampleTime
    
    tryCatch({
      
      # call function
      addNewSample(sampleSite = newSampleSite,
                   sampleDate = newSampleDate,
                   sampleTime = newSampleTime,
                   sampleBottle = bottleID)
      
      # reset query type
      queryType$default <- "default"
      
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
      
    }) # close try catch
    
  }) # close add a new reach extent measure
  
  
  # delete sample -----------------------------------------------------------
  
  # function: deleteSample - delete selected sample from database
  deleteSample <- function(row_to_delete) {
    
    baseQuery <- '
    DELETE FROM stormwater.samples
    WHERE samples.sample_id = ?RWE_ID;'
    
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         RWE_ID = as.numeric(row_to_delete))
    
    run_interpolated_execution(parameterizedQuery)
    
    # change listener state when deleting a record
    listenModifySamples$dbVersion <- isolate(listenModifySamples$dbVersion + 1)
    
  }
  
  # call function::deleteSample - delete prescribed sample from the database
  observeEvent(input$button_delete_sample, {
    
    deleteSample(row_to_delete = input$button_delete_sample)
    
  })
  
  
  # update reach extent measure ---------------------------------------------
  
  # function: updateSample - write edited cell change to the database
  updateSample <- function(reactiveData, cellEdited) {
    
    reactiveDataColNames <- as.list(colnames(reactiveData))
    editedColumn <- reactiveDataColNames[cellEdited$col + 1]
    editedRow <- reactiveData[cellEdited$row, ][['id']]
    newValue <- cellEdited[['value']]
    
    # change temp, ph, cond to appropriate data type
    if (grepl("temp|pH|cond", editedColumn, ignore.case = TRUE)) {
      newValue <- as.numeric(newValue)
    }
    
    # recast temp, ph, cond from simple names to names per DB schema
    if (grepl("temp", editedColumn)) {
      editedColumn <- "lab_temperature"
    }
    
    if (grepl("pH", editedColumn, ignore.case = T)) {
      editedColumn <- "\"lab_pH\""
    }
    
    if (grepl("cond", editedColumn)) {
      editedColumn <- "lab_conductance"
    }
    
    # inexplicable behaviour in this case where the edited column is quoted,
    # which is not permissible in postgres; use SQL quoting to obtain proper
    # formatting
    editedColumn <- SQL(editedColumn) 
    
    baseQuery <- '
    UPDATE stormwater.samples
    SET ?editedCol = ?updatedValue
    WHERE sample_id = ?tuple;'
    
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         editedCol = editedColumn,
                                         updatedValue = newValue,
                                         tuple = editedRow)
    
    run_interpolated_execution(parameterizedQuery)
    
  }
  
  
  # call function updateSample - write edited cell change to the database
  observeEvent(input$samplesDataView_cell_edit, {
    
    tryCatch({
      
      updateSample(reactiveData = samplesDataReactive(),
                   cellEdited = input$samplesDataView_cell_edit)
      
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
      
    }) # close try catch
    
  })
  
  # debugging: module level -------------------------------------------------
  
  ############# START debugging
  # observe(print({ queryType }))
  # observe(print({ queryType$default }))
  # observe(print({ input$ReachPatchs_cell_edit }))
  ############# END debugging
  
  
  # close module modifySamples ----------------------------------------------
  
} # close module::modifySamples
