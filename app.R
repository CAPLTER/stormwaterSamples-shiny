
# README ------------------------------------------------------------------

# Note regarding Javascript: There are several code chunks that have the comment
# 'notable stmt' associated with them. These statements are related to the
# Javascript functionality required for the interactivity of a file upload,
# i.e., adding temp, cond, etc. to the table following a file uplooad. See
# https://stackoverflow.com/questions/40020600/r-shiny-selectedinput-inside-renderdatatable-cells
# and
# https://groups.google.com/forum/#!msg/shiny-discuss/ZUMBGGl1sss/zfcG9c6MBAAJ
# for details.


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
  navbarPage(title = "stormwater",
             id = "tabs", # use explicit id to access tab position

             # isco tab ----------------------------------------------------------------

             tabPanel("isco",
                      fluidPage(
                        fluidRow(
                          column(id = "leftPanel", 2,
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
                                               resize = "vertical",
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
                          ) # close the right col

                        ) # close the row
                      ) # close the page
             ), # clase file upload tab panel

             # samples tab -------------------------------------------------------------

             tabPanel("samples",
                      modifySamplesUI("modifySamples")
             ), # close samples tab

             # discharge upload tab ----------------------------------------------------

             tabPanel("discharge: upload",
                      fluidPage(
                        fluidRow(
                          column(id = "leftPanel", 2,
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
                                 strong("Data Preview"),
                                 hr(),
                                 tableOutput("levelDataPreview")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # close discharge: file upload

             # discharge viewing tab ---------------------------------------------------

             tabPanel("discharge",
                      viewDischargeUI("viewDischarge")
             ), # close discharge tab

             # solids tab --------------------------------------------------------------

             tabPanel("solids",
                      fluidPage(
                        fluidRow(
                          column(id = "leftPanel", 2,
                                 # filter existing
                                 strong("filter samples",
                                        style = "text-align: center; color: black"),
                                 selectizeInput(inputId = "viewSamplesSolidsSite",
                                                "site",
                                                choices = siteAbbreviations,
                                                selected = NULL,
                                                multiple = FALSE),
                                 dateInput(inputId = "viewSamplesSolidsStartDate",
                                           "start:",
                                           format = "yyyy-mm-dd"),
                                 dateInput(inputId = "viewSamplesSolidsEndDate",
                                           "end:",
                                           format = "yyyy-mm-dd"),
                                 actionButton(inputId = "filterSamplesSolids",
                                              label = "view samples",
                                              style = "text-align:center; border-sytle:solid; border-color:#0000ff;")
                          ), # close the left col
                          column(id = "rightPanel", 10,
                                 DT::DTOutput("samplesSolidsDataView"),
                                 DT::DTOutput("solidsData"),
                                 uiOutput("addNewSolidUI"),
                                 div(id = "modifySolidsDiv")
                          ) # close the right col
                        ) # close the row
                      ) # close the page
             ), # close AFDM tab panel

             # cations tab -------------------------------------------------------------

             tabPanel("cations",
                      cationsUI("icpCations")
             ), # close cations tab

             # lachat tab --------------------------------------------------------------

             tabPanel("lachat",
                      lachatUI("lachat")
             ), # close lachat tab


             # aq2 tab --------------------------------------------------------------

             tabPanel("aq2",
                      aq2UI("aq2")
             ), # close aq2 tab

             # chem data view ----------------------------------------------------------

             tabPanel("chemistry: data viewer",
                      ChemViewer1$ui()
             ) # close 'chemistry: data viewer' tab panel

             # closing UI --------------------------------------------------------------

  ) # close navbar/page
) # close tagList


# server ------------------------------------------------------------------

server <- function(input, output, session) {

  # file upload -------------------------------------------------------------

  # helper function for reading input functions; for reasons that are completely
  # unclear, this function only works if included in app.R (i.e., it is loaded
  # but does not seem to work if loaded from helper_shiny_input.R)
  shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
      value = input[[paste0(id, i)]]
      if (is.null(value)) NA else value
    }))
  }


  # site id from file upload
  sampleReportSiteId <- reactive({

    req(input$file1)

    read_csv(input$file1$datapath,
             n_max = 1,
             col_names = c("reportText", "siteID")) %>%
      dplyr::select(siteID) %>%
      unlist(., use.names = FALSE)

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
        omit = shinyInputOther(checkboxInput,
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

    tryCatch({

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

      run_interpolated_execution(baseInsert)

      showNotification(ui = "sample data uploaded",
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

    }) # close try catch

  }) # close observeEvent(input$submitFileUpload...


  # call to modifySamples module --------------------------------------------

  callModule(module = modifySamples,
             id = "modifySamples")


  # solids ------------------------------------------------------------------

  # create listener for adding, deleting, and updating solids data
  listenModifySamplesSolids <- reactiveValues(dbVersion = 0)

  # queryType: default vs parameterized query for transects
  queryTypeSamplesSolids <- reactiveValues(default = "default")

  # actionButton filterSamples = parameterized query type
  observeEvent(input$filterSamplesSolids, {

    queryTypeSamplesSolids$default <- "param"

  })

  # query samples-solids data
  samplesSolidsDataReactive <- reactive({

    # add listener for adding, deleting, and editing records
    listenModifySamplesSolids$dbVersion

    if (queryTypeSamplesSolids$default == "default") {

      samplesSolidsData <- query_solids_default()

    } else {

      # parameters cannot be passed to function directly
      filterSamplesSolidsStart <- as.character(input$viewSamplesSolidsStartDate)
      filterSamplesSolidsEnd <- as.character(input$viewSamplesSolidsEndDate)
      filterSamplesSolidsSite <- input$viewSamplesSolidsSite

      # run query with params
      samplesSolidsData <- query_solids_site_date(start = filterSamplesSolidsStart,
                                                  end = filterSamplesSolidsEnd,
                                                  site = filterSamplesSolidsSite)

    }

    if (nrow(samplesSolidsData) == 0) {

      samplesSolidsData <- data.frame(
        solid_id = NA,
        sample_id = NA,
        site = NA,
        sample_datetime = NA,
        bottle = NA,
        afdm_bottle_id = NA,
        filter_initial = NA,
        filter_dry = NA,
        volume_filtered = NA,
        filter_ashed = NA,
        replicate = NA,
        comments = as.character("match not found")
      )

    } else {

      # add and delete buttons to samplesSolidsData data
      samplesSolidsData <- samplesSolidsData %>%
        mutate(
          add = shinyInputFlex(reactiveObject = samplesSolidsData,
                               FUN = actionButton,
                               len = nrow(samplesSolidsData),
                               id = 'sample_id',
                               label = "add",
                               onclick = sprintf('Shiny.setInputValue("%s",  this.id)', "button_add_solids_sample")),
          delete = shinyInputFlex(reactiveObject = samplesSolidsData,
                                  FUN = actionButton,
                                  len = nrow(samplesSolidsData),
                                  id = 'solid_id',
                                  label = "delete",
                                  onclick = sprintf('Shiny.setInputValue("%s",  this.id)', "button_delete_solids_sample"))
        )

    }

    return(samplesSolidsData)

  })

  # render table of samples-solids data
  output$samplesSolidsDataView <- DT::renderDT({

    samplesSolidsDataReactive()

  },
  escape = FALSE,
  selection = "none",
  rownames = FALSE,
  editable = list(target = "cell",
                  disable = list(columns = c(0,1,2,3,4,5,12,13))),
  options = list(bFilter = FALSE,
                 bLengthChange = TRUE,
                 bPaginate = TRUE,
                 bSort = FALSE,
                 autoWidth = TRUE,
                 columnDefs = list(list(width = "100px", targets = c(1)))
  )
  ) # close output$samplesSolidsDataView

  output$sampleUnderEdit <- renderText({ input$button_add_solids_sample })


  # solids: add new ---------------------------------------------------------

  # generate UI for adding a new solid
  observeEvent(input$button_add_solids_sample, {

    output$addNewSolidUI <- renderUI({

      tagList(
        hr(),
        p("new solids data",
          style = "text-align: left; background-color: LightGray; color: black;"),
        tags$head(
          tags$style(
            HTML(paste0("#", "sampleUnderEdit", "{ color: DarkGray; }"))
          ) # close tags$style
        ), # close tagss$head
        fluidRow(id = "newSolidTopRow",
                 column(1,
                        tags$b("sample_id"),
                        p(""),
                        textOutput("sampleUnderEdit")
                 ),
                 column(2,
                        numericInput("newFilterInitial",
                                     label = "initial",
                                     value = NULL)
                 ),
                 column(2,
                        numericInput("newFilterDry",
                                     label = "dry",
                                     value = NULL)
                 ),
                 column(2,
                        numericInput("newVolumeFiltered",
                                     label = "volume",
                                     value = NULL)
                 ),
                 column(2,
                        numericInput("newFilterAshed",
                                     label = "ashed",
                                     value = NULL)
                 ),
                 column(1,
                        numericInput("newReplicate",
                                     label = "replicate",
                                     value = 1)
                 )
        ), # close fluidRow top
        fluidRow(id = "newSolidBottomRow",
                 column(5,
                        textInput("newComments",
                                  label = "comments")
                 ),
                 column(1,
                        style = "margin-top: 25px",
                        actionButton("addNewSolid",
                                     label = "add new",
                                     style = "text-align:center; border-sytle:solid; border-color:#0000ff;")
                 ) # close last column
        ) # close fluidRow bottom
      ) # close tag list

    }) # close output$addNewSolidUI

  }) # close observeEvent(input$button_add_solids_sample...

  # function: addSolid - write new solid to the database
  addSolid <- function(sampleID, initial, dry, volume, ashed, replicate, comments) {

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
                                         sampleIdent = as.numeric(sampleID),
                                         filterInitial = initial,
                                         filterDry = dry,
                                         filterVolume = volume,
                                         filterAshed = ashed,
                                         solidReplicate = replicate,
                                         solidComment = comments
    )

    run_interpolated_execution(parameterizedQuery)

    # change listener state when adding a record
    listenModifySamplesSolids$dbVersion <- isolate(listenModifySamplesSolids$dbVersion + 1)

  }

  # add a new reach extent measure
  observeEvent(input$addNewSolid, {

    addSolid(sampleID = input$button_add_solids_sample,
             initial = input$newFilterInitial,
             dry = input$newFilterDry,
             volume = input$newVolumeFiltered,
             ashed = input$newFilterAshed,
             replicate = input$newReplicate,
             comments = input$newComments)

  })


  # solids: delete existing -------------------------------------------------

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
    listenModifySamplesSolids$dbVersion <- isolate(listenModifySamplesSolids$dbVersion + 1)

  }

  # call function deleteSolid - delete prescribed solid from the database
  observeEvent(input$button_delete_solids_sample, {

    deleteSolid(row_to_delete = input$button_delete_solids_sample)

  })


  # solids: update existing -------------------------------------------------

  # function: updateSolid - write edited cell change to the database
  updateSolid <- function(reactiveData, cellEdited) {

    reactiveDataColNames <- as.list(colnames(reactiveData))
    editedColumn <- reactiveDataColNames[cellEdited$col + 1]
    editedRow <- reactiveData[cellEdited$row, ][['solid_id']]
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

    # change listener state when deleting a record
    listenModifySamplesSolids$dbVersion <- isolate(listenModifySamplesSolids$dbVersion + 1)

  }

  # call function updateSample - write edited cell change to the database
  observeEvent(input$samplesSolidsDataView_cell_edit, {

    updateSolid(reactiveData = samplesSolidsDataReactive(),
                cellEdited = input$samplesSolidsDataView_cell_edit)

  })


  # call to viewDischarge module --------------------------------------------

  callModule(module = viewDischarge,
             id = "viewDischarge")


  # discharge upload --------------------------------------------------------

  # site id from file upload
  levelDataSiteId <- reactive({

    req(input$dischargeFile)

    read_csv(input$dischargeFile$datapath,
             n_max = 1,
             col_names = c("reportText", "siteID")) %>%
      dplyr::select(siteID) %>%
      unlist(., use.names = FALSE)

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

      dbGetQuery(stormPool, "BEGIN TRANSACTION")

      # write new samples to samples_temp
      if (dbExistsTable(stormPool, c('stormwater', 'discharge_temp'))) dbRemoveTable(stormPool, c('stormwater', 'discharge_temp'))
      dbWriteTable(stormPool, c('stormwater', 'discharge_temp'), value = dischargeToWrite, row.names = F)

      # remove timezone type generated by dbWriteTable function
      dbExecute(stormPool,'
            ALTER TABLE stormwater.discharge_temp
            ALTER COLUMN event_datetime TYPE TIMESTAMP WITHOUT TIME ZONE;')

      # execute insert query
      dbExecute(stormPool, insertLevelDataQuery)

      # clean up
      dbRemoveTable(stormPool, c('stormwater', 'discharge_temp'))

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

  })


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


  # call to chem viewer module ----------------------------------------------

  ChemViewer1$call()


  # debugging ---------------------------------------------------------------

  # observe(print({ input$tabs }))
  # observe(print({ solidsDataReactive() }))
  # observe(print({ listenModifySolids$dbVersion }))
  # observe(print({ queryType$default }))
  # observe(print({ input$solidsData_cell_edit }))


  # close server ------------------------------------------------------------

}

# Run the application
shinyApp(ui = ui, server = server)
