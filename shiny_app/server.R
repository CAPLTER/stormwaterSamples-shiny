server <- function(input, output, session) {

  # file upload -------------------------------------------------------------

  # helper function for reading input functions; for reasons that are completely
  # unclear, this function only works if included in app.R (i.e., it is loaded
  # but does not seem to work if loaded from helper_shiny_input.R)
  shinyValue <- function(id, len) {
    unlist(
      lapply(seq_len(len), function(i) {
        value = input[[paste0(id, i)]]
        if (is.null(value)) NA else value
}
      )
    )
  }


  # listeners ------------------------------------------------------------------

  listener_init("update_sample")


  # modules --------------------------------------------------------------------

  upload_report("upload_report") # upload 6700 sample report
  samples_inventory("samples_inventory") # manage samples data
  solids_inventory("solids_inventory") # manage samples data


  # solids ------------------------------------------------------------------

#   # create listener for adding, deleting, and updating solids data
#   listenModifySamplesSolids <- reactiveValues(dbVersion = 0)
# 
#   # queryType: default vs parameterized query for transects
#   queryTypeSamplesSolids <- reactiveValues(default = "default")
# 
#   # actionButton filterSamples = parameterized query type
#   observeEvent(input$filterSamplesSolids, {
# 
#     queryTypeSamplesSolids$default <- "param"
# 
#   })
# 
#   # query samples-solids data
#   samplesSolidsDataReactive <- reactive({
# 
#     # add listener for adding, deleting, and editing records
#     listenModifySamplesSolids$dbVersion
# 
#     if (queryTypeSamplesSolids$default == "default") {
# 
#       samplesSolidsData <- query_solids_default()
# 
#     } else {
# 
#       # parameters cannot be passed to function directly
#       filterSamplesSolidsStart <- as.character(input$viewSamplesSolidsStartDate)
#       filterSamplesSolidsEnd <- as.character(input$viewSamplesSolidsEndDate)
#       filterSamplesSolidsSite <- input$viewSamplesSolidsSite
# 
#       # run query with params
#       samplesSolidsData <- query_solids_site_date(start = filterSamplesSolidsStart,
#         end = filterSamplesSolidsEnd,
#         site = filterSamplesSolidsSite)
# 
#     }
# 
#     if (nrow(samplesSolidsData) == 0) {
# 
#       samplesSolidsData <- data.frame(
#         solid_id = NA,
#         sample_id = NA,
#         site = NA,
#         sample_datetime = NA,
#         bottle = NA,
#         afdm_bottle_id = NA,
#         filter_initial = NA,
#         filter_dry = NA,
#         volume_filtered = NA,
#         filter_ashed = NA,
#         replicate = NA,
#         comments = as.character("match not found")
#       )
# 
#     } else {
# 
#       # add and delete buttons to samplesSolidsData data
#       samplesSolidsData <- samplesSolidsData |>
#       dplyr::mutate(
#         add = shinyInputFlex(
#           reactiveObject = samplesSolidsData,
#           FUN = actionButton,
#           len = nrow(samplesSolidsData),
#           id = 'sample_id',
#           label = "add",
#           onclick = sprintf('Shiny.setInputValue("%s",  this.id)', "button_add_solids_sample")
#           ),
#         delete = shinyInputFlex(
#           reactiveObject = samplesSolidsData,
#           FUN = actionButton,
#           len = nrow(samplesSolidsData),
#           id = 'solid_id',
#           label = "delete",
#           onclick = sprintf('Shiny.setInputValue("%s",  this.id)', "button_delete_solids_sample")
#         )
#       )
# 
#     }
# 
#     return(samplesSolidsData)
# 
#   })
# 
#   # render table of samples-solids data
#   output$samplesSolidsDataView <- DT::renderDT({
# 
#     samplesSolidsDataReactive()
# 
#   },
#   escape = FALSE,
#   selection = "none",
#   rownames = FALSE,
#   editable = list(
#     target = "cell",
#     disable = list(columns = c(0,1,2,3,4,5,12,13))
#     ),
#   options = list(
#     bFilter = FALSE,
#     bLengthChange = TRUE,
#     bPaginate = TRUE,
#     bSort = FALSE,
#     autoWidth = TRUE,
#     columnDefs = list(list(width = "100px", targets = c(1))
#     )
#   )
#   ) # close output$samplesSolidsDataView
# 
#   output$sampleUnderEdit <- renderText({ input$button_add_solids_sample })
# 
# 
#   # solids: add new ---------------------------------------------------------
# 
#   # generate UI for adding a new solid
#   observeEvent(input$button_add_solids_sample, {
# 
#     output$addNewSolidUI <- renderUI({
# 
#       tagList(
#         hr(),
#         p("new solids data",
#           style = "text-align: left; background-color: LightGray; color: black;"),
#         tags$head(
#           tags$style(
#             HTML(paste0("#", "sampleUnderEdit", "{ color: DarkGray; }"))
#           ) # close tags$style
#           ), # close tagss$head
#         fluidRow(id = "newSolidTopRow",
#           column(1,
#             tags$b("sample_id"),
#             p(""),
#             textOutput("sampleUnderEdit")
#             ),
#           column(2,
#             numericInput("newFilterInitial",
#               label = "initial",
#               value = NULL)
#             ),
#           column(2,
#             numericInput("newFilterDry",
#               label = "dry",
#               value = NULL)
#             ),
#           column(2,
#             numericInput("newVolumeFiltered",
#               label = "volume",
#               value = NULL)
#             ),
#           column(2,
#             numericInput("newFilterAshed",
#               label = "ashed",
#               value = NULL)
#             ),
#           column(1,
#             numericInput("newReplicate",
#               label = "replicate",
#               value = 1)
#           )
#           ), # close fluidRow top
#         fluidRow(id = "newSolidBottomRow",
#           column(5,
#             textInput("newComments",
#               label = "comments")
#             ),
#           column(1,
#             style = "margin-top: 25px",
#             actionButton("addNewSolid",
#               label = "add new",
#               style = "text-align:center; border-sytle:solid; border-color:#0000ff;")
#           ) # close last column
#         ) # close fluidRow bottom
#       ) # close tag list
# 
#     }) # close output$addNewSolidUI
# 
#   }) # close observeEvent(input$button_add_solids_sample...
# 
#   # function: addSolid - write new solid to the database
#   addSolid <- function(sampleID, initial, dry, volume, ashed, replicate, comments) {
# 
#     baseQuery <- "
#     INSERT INTO stormwater.solids
#     (
#       sample_id,
#       filter_initial,
#       filter_dry,
#       volume_filtered,
#       filter_ashed,
#       replicate,
#       comments
#     )
#     VALUES
#     (
#       ?sampleIdent,
#       ?filterInitial,
#       ?filterDry,
#       ?filterVolume,
#       ?filterAshed,
#       ?solidReplicate,
#       NULLIF(?solidComment, '')::text
#       );"
# 
#     parameterizedQuery <- sqlInterpolate(
#       ANSI(),
#       baseQuery,
#       sampleIdent = as.numeric(sampleID),
#       filterInitial = initial,
#       filterDry = dry,
#       filterVolume = volume,
#       filterAshed = ashed,
#       solidReplicate = replicate,
#       solidComment = comments
#     )
# 
#     run_interpolated_execution(parameterizedQuery)
# 
#     # change listener state when adding a record
#     listenModifySamplesSolids$dbVersion <- isolate(listenModifySamplesSolids$dbVersion + 1)
# 
#   }
# 
#   # add a new reach extent measure
#   observeEvent(input$addNewSolid, {
# 
#     addSolid(
#       sampleID = input$button_add_solids_sample,
#       initial = input$newFilterInitial,
#       dry = input$newFilterDry,
#       volume = input$newVolumeFiltered,
#       ashed = input$newFilterAshed,
#       replicate = input$newReplicate,
#       comments = input$newComments
#     )
# 
#     })
# 
# 
#   # solids: delete existing -------------------------------------------------
# 
#   # function: deleteSolid - delete selected solid from database
#   deleteSolid <- function(row_to_delete) {
# 
#     baseQuery <- '
#     DELETE FROM stormwater.solids
#     WHERE solids.solid_id = ?RWE_ID;'
# 
#     parameterizedQuery <- sqlInterpolate(ANSI(),
#       baseQuery,
#       RWE_ID = as.numeric(row_to_delete))
# 
#     run_interpolated_execution(parameterizedQuery)
# 
#     # change listener state when deleting a record
#     listenModifySamplesSolids$dbVersion <- isolate(listenModifySamplesSolids$dbVersion + 1)
# 
#   }
# 
#   # call function deleteSolid - delete prescribed solid from the database
#   observeEvent(input$button_delete_solids_sample, {
# 
#     deleteSolid(row_to_delete = input$button_delete_solids_sample)
# 
#       })
# 
# 
#   # solids: update existing -------------------------------------------------
# 
#   # function: updateSolid - write edited cell change to the database
#   updateSolid <- function(reactiveData, cellEdited) {
# 
#     reactiveDataColNames <- as.list(colnames(reactiveData))
#     editedColumn <- reactiveDataColNames[cellEdited$col + 1]
#     editedRow <- reactiveData[cellEdited$row, ][['solid_id']]
#     newValue <- cellEdited[['value']]
# 
#     # change numerics to appropriate data type
#     if (grepl("filter|replicate", editedColumn, ignore.case = TRUE)) {
#       newValue <- as.numeric(newValue)
#     }
# 
#     # inexplicable behaviour in this case where the edited column is quoted,
#     # which is not permissible in postgres; use SQL quoting to obtain proper
#     # formatting
#     editedColumn <- SQL(editedColumn)
# 
#     baseQuery <- '
#     UPDATE stormwater.solids
#     SET ?editedCol = ?updatedValue
#     WHERE solid_id = ?tuple;'
# 
#     parameterizedQuery <- sqlInterpolate(ANSI(),
#       baseQuery,
#       editedCol = editedColumn,
#       updatedValue = newValue,
#       tuple = editedRow)
# 
#     run_interpolated_execution(parameterizedQuery)
# 
#     # change listener state when deleting a record
#     listenModifySamplesSolids$dbVersion <- isolate(listenModifySamplesSolids$dbVersion + 1)
# 
#   }
# 
#   # call function updateSample - write edited cell change to the database
#   observeEvent(input$samplesSolidsDataView_cell_edit, {
# 
#     updateSolid(reactiveData = samplesSolidsDataReactive(),
#       cellEdited = input$samplesSolidsDataView_cell_edit)
# 
#       })


  # call to viewDischarge module --------------------------------------------

  callModule(
    module = viewDischarge,
    id = "viewDischarge"
  )


  # discharge upload --------------------------------------------------------

  # site id from file upload
  levelDataSiteId <- reactive({

    req(input$dischargeFile)

    readr::read_csv(
      file = input$dischargeFile$datapath,
      n_max = 1,
      col_names = c("reportText", "siteID")) %>%
    dplyr::select(siteID) %>%
    unlist(., use.names = FALSE)

  })


  # level data from file upload
  levelData <- reactive({

    req(input$dischargeFile)

    readr::read_csv(
      file = input$dischargeFile$datapath,
      skip = 7,
      col_names = c("event_datetime", "level")
      ) %>%
    dplyr::filter(!is.na(event_datetime)) %>%
    dplyr::mutate(
      event_datetime = lubridate::parse_date_time(event_datetime, c("mdY HMS p", "mdY HMS")),
      event_datetime = format(event_datetime, "%Y-%m-%d %H:%M:%S"),
      site_id = levelDataSiteId(),
      site_id = as.integer(site_id),
      source_file = input$dischargeFile$name
      ) %>%
    dplyr::select(
      site_id,
      event_datetime,
      water_height = level,
      source_file
    )

  })


  # preview data table with upload and provided values
  output$levelDataPreview <- renderTable({

    levelData()

  })


  # write discharge data to database
  observeEvent(input$submitDischargeFileUpload, {

    # modify data object as needed for the DB
    dischargeToWrite <- levelData() %>%
      dplyr::mutate(
        event_datetime = as.POSIXct(event_datetime, format = "%Y-%m-%d %H:%M:%S")
      )

    # add run identifier as maxrun
    maxrun <- as.numeric(run_interpolated_query(interpolatedQuery = "SELECT MAX(run_id) FROM stormwater.discharge ;"))

    if (is.na(maxrun) | is.null(maxrun)) { maxrun <- 0 }

    dischargeToWrite$run_id <- maxrun + 1

    # write temp table
    if (dbExistsTable(stormPool, c('stormwater', 'discharge_temp'))) {

      dbRemoveTable(stormPool, c('stormwater', 'discharge_temp'))

    }

    dbWriteTable(
      conn = stormPool,
      name = c('stormwater', 'discharge_temp'),
      value = dischargeToWrite,
      row.names = F
    )

    # remove timezone type generated by dbWriteTable function
    remove_timezone_query <- '
    ALTER TABLE stormwater.discharge_temp
    ALTER COLUMN event_datetime TYPE TIMESTAMP WITHOUT TIME ZONE ;'

    run_interpolated_execution(interpolatedQuery = remove_timezone_query)

    # write temp data to database

    # insert into samples. the 'ON CONFLICT' clause allows for skipping
    # inserting any data that are already in the database.
    insertLevelDataQuery <- '
    INSERT INTO stormwater.discharge
    (
      site_id,
      event_datetime,
      water_height,
      source_file,
      run_id
    )
    (
      SELECT
      site_id,
      event_datetime,
      water_height,
      source_file,
      run_id
      FROM
      stormwater.discharge_temp
    )
    ON CONFLICT ON CONSTRAINT discharge_unique_observations DO NOTHING;'

    # write temp data to database
    run_interpolated_execution(
      interpolatedQuery = insertLevelDataQuery, 
      success_notice = TRUE
    )

    # remove temporary table
    if (dbExistsTable(stormPool, c('stormwater', 'discharge_temp'))) {

      dbRemoveTable(stormPool, c('stormwater', 'discharge_temp'))

    }


  }) # close observe event upload level data


  # establish tab position as input to modules ------------------------------

  tabID <- reactive({ input$tabs })


  # call to cations module --------------------------------------------------

  callModule(module = cations,
    id = "icpCations",
    tab = tabID)


  # call to lachat module ---------------------------------------------------

  callModule(module = lachat,
    id = "lachat",
    tab = tabID)


  # call to aq2 module ---------------------------------------------------

  callModule(module = aq2,
    id = "aq2",
    tab = tabID)


  # call to shimadzu module ---------------------------------------------------

  callModule(module = shimadzu,
    id = "shimadzu",
    tab = tabID)


  # call to chem viewer module ----------------------------------------------

  ChemViewer1$call()

  # call to chem inventory module ----------------------------------------------

  ChemInventory1$call()


  # debugging ---------------------------------------------------------------

  # observe(print({ input$tabs }))
  # observe(print({ solidsDataReactive() }))
  # observe(print({ listenModifySolids$dbVersion }))
  # observe(print({ queryType$default }))
  # observe(print({ input$solidsData_cell_edit }))


  # close server ------------------------------------------------------------

}
