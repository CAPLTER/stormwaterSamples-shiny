#' @title Module: modifySolids
#'
#' @description The module modifySolids facilitates viewing stormwater solids,
#'   adding new entries, editing values, and deleting existing
#'   records.

# modify solids -----------------------------------------------------------

modifySolidsUI <- function(id) {
  
  ns <- NS(id)
  
  tagList(
    # uiOutput(ns("reachExtentsStyling")),
    DT::DTOutput(ns("solidsData")),
    uiOutput(ns("addNewSolidUI"))
  ) # close tagList
  
} # close modifySolidsUI


# modify solids main ------------------------------------------------------

modifySolids <- function(input, output, session, sampleID) {
  
  # added to facilitate renderUIs
  ns <- session$ns
  
  # create listener for adding and deleting records
  listenModifySolids <- reactiveValues(dbVersion = 0)
  
  # query samples data
  solidsDataReactive <- reactive({
    
    # add listener for adding and deleting records
    listenModifySolids$dbVersion
    
    baseQuery <- '
    SELECT
      solids.solid_id AS id,
      samples.sample_id,
      sites.abbreviation AS site,
      samples.sample_datetime::TEXT,
      samples.bottle,
      samples.afdm_bottle_id,
      solids.filter_initial,
      solids.filter_dry,
      solids.volume_filtered,
      solids.filter_ashed,
      solids.replicate,
      solids.comments
    FROM stormwater.samples
    JOIN stormwater.sites ON (samples.site_id = sites.site_id)
    LEFT JOIN stormwater.solids ON (solids.sample_id = samples.sample_id)
    WHERE
      samples.sample_id = ?sample_id;'
    
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         sample_id = sampleID())
    
    solidsData <- run_interpolated_query(parameterizedQuery)
    
    if (is.na(solidsData[1,]$id)) {
      
      solidsData <- solidsData[,c("id", "sample_id", "site", "sample_datetime", "bottle", "afdm_bottle_id", "comments")]
      solidsData$comments <- "solids not entered for this sample"
      
    } else {
      
      # add delete button to solids data
      # remember that session$ns is required for modules!!!
      solidsData <- solidsData %>%
        mutate(delete = shinyInput(reactiveObject = solidsData,
                                   FUN = actionButton,
                                   len = nrow(solidsData),
                                   id = '',
                                   label = "delete",
                                   onclick = sprintf('Shiny.setInputValue("%s",  this.id)', session$ns("button_delete_solid")))
        )
      
    }
    
    return(solidsData)
    
  })
  
  # render editable table of solids data
  output$solidsData <- DT::renderDT({
    
    solidsDataReactive()
    
  },
  escape = FALSE,
  selection = "none",
  rownames = FALSE,
  editable = list(target = 'cell',
                  disable = list(columns = c(0,1,2,3,4,5,12))),
  options = list(bFilter = 0,
                 bLengthChange = FALSE,
                 bPaginate = FALSE,
                 bSort = FALSE,
                 autoWidth = TRUE,
                 columnDefs = list(list(width = '100px', targets = c(1)))
  )
  ) # close output$solidsData
  
  
  # delete solid ------------------------------------------------------------
  
  # function: deleteSolid - delete selected solid from database
  deleteSolid <- function(row_to_delete) {
    
    baseQuery <- '
    DELETE FROM stormwater.solids
    WHERE solids.solid_id = ?RWE_ID;'
    
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         RWE_ID = as.numeric(row_to_delete))
    
    run_interpolated_execution(parameterizedQuery)
    
    # change listener state when deleting a record
    listenModifySolids$dbVersion <- isolate(listenModifySolids$dbVersion + 1)
    
  }
  
  # call function deleteSolid - delete prescribed solid from the database
  observeEvent(input$button_delete_solid, {
    
    deleteSolid(row_to_delete = input$button_delete_solid)
    
  })
  
  
  # update solids -----------------------------------------------------------
  
  # function: updateSolid - write edited cell change to the database
  updateSolid <- function(reactiveData, cellEdited) {
    
    reactiveDataColNames <- as.list(colnames(reactiveData))
    editedColumn <- reactiveDataColNames[cellEdited$col + 1]
    editedRow <- reactiveData[cellEdited$row, ][['id']]
    newValue <- cellEdited[['value']]
    
    # change numerics to appropriate data type
    if (grepl("filter|replicate", editedColumn, ignore.case = TRUE)) {
      newValue <- as.numeric(newValue)
    }
    
    # inexplicable behaviour in this case where the edited column is quoted,
    # which is not permissible in postgres; use SQL quoting to obtain proper
    # formatting
    editedColumn <- SQL(editedColumn)
    
    baseQuery <- '
    UPDATE stormwater.solids
    SET ?editedCol = ?updatedValue
    WHERE solid_id = ?tuple;'
    
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         editedCol = editedColumn,
                                         updatedValue = newValue,
                                         tuple = editedRow)
    
    run_interpolated_execution(parameterizedQuery)
    
  }
  
  
  # call function updateSample - write edited cell change to the database
  observeEvent(input$solidsData_cell_edit, {
    
    updateSolid(reactiveData = solidsDataReactive(),
                cellEdited = input$solidsData_cell_edit)
    
  })
  
  # add new solids sample...
  
  output$sampleUnderEdit <- renderText({ sampleID() })
  
  # generate UI for adding a new solid
  output$addNewSolidUI <- renderUI({
    
    tagList(
      tags$head(
        tags$style(
          HTML(paste0("#", ns("sampleUnderEdit"), "{ color: DarkGray; }"))
        ) # close tags$style
      ), # close tagss$head
      fluidRow(id = "newSolidRow",
               column(1,
                      tags$b("sample_id"),
                      p(""),
                      textOutput(ns("sampleUnderEdit"))
               ),
               column(2,
                      numericInput(ns("newFilterInitial"),
                                   label = "initial",
                                   value = NULL)
               ),
               column(2,
                      numericInput(ns("newFilterDry"),
                                   label = "dry",
                                   value = NULL)
               ),
               column(1,
                      numericInput(ns("newVolumeFiltered"),
                                   label = "volume",
                                   value = NULL)
               ),
               column(2,
                      numericInput(ns("newFilterAshed"),
                                   label = "ashed",
                                   value = NULL)
               ),
               column(1,
                      numericInput(ns("newReplicate"),
                                   label = "replicate",
                                   value = NULL)
               ),
               column(2,
                      textInput(ns("newComments"),
                                label = "comments")
               ),
               column(1,
                      style = "margin-top: 25px",
                      actionButton(ns("addNewSolid"),
                                   label = "add new")
               ) # close last column
      ) # close fluidRow
    ) # close tag list
    
  })
  
  
  # function: addSolid - write new solid to the database
  addSolid <- function(initial, dry, volume, ashed, replicate, comments) {
    
    baseQuery <- "
    INSERT INTO stormwater.solids
    (
      sample_id,
      filter_initial,
      filter_dry,
      volume_filtered,
      filter_ashed,
      replicate,
      comments
    )
    VALUES
    (
      ?sampleIdent,
      ?filterInitial,
      ?filterDry,
      ?filterVolume,
      ?filterAshed,
      ?solidReplicate,
      NULLIF(?solidComment, '')::text 
    );"
    
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         sampleIdent = as.numeric(sampleID()),
                                         filterInitial = initial,
                                         filterDry = dry,
                                         filterVolume = volume,
                                         filterAshed = ashed,
                                         solidReplicate = replicate,
                                         solidComment = comments
    )
    
    run_interpolated_execution(parameterizedQuery)
    
    # change listener state when adding a record
    listenModifySolids$dbVersion <- isolate(listenModifySolids$dbVersion + 1)
    
  }
  
  # add a new reach extent measure
  observeEvent(input$addNewSolid, {
    
    addSolid(initial = input$newFilterInitial,
             dry = input$newFilterDry,
             volume = input$newVolumeFiltered,
             ashed = input$newFilterAshed,
             replicate = input$newReplicate,
             comments = input$newComments)
    
  })
  
  
  # debugging: module level -------------------------------------------------
  
  ############# START debugging
  # observe(print({ solidsDataReactive() }))
  # observe(print({ listenModifySolids$dbVersion }))
  # observe(print({ queryType$default }))
  # observe(print({ input$solidsData_cell_edit }))
  ############# END debugging
  
  
  # close module modifySamples ----------------------------------------------
  
} # close module::modifySamples
