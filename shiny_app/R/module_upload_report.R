upload_reportUI <- function(id) {

  ns <- shiny::NS(id)

  tagList(

    shiny::fluidRow(

      shiny::column(
        id = "leftPanel", 2,
        fileInput(
          ns("file1"),
          "choose csv file",
          multiple = FALSE,
          accept = c(
            "text/csv",
            "text/comma-separated-values,text/plain",
            ".csv")
          ),
        shiny::sliderInput(
          inputId = ns("storm"),
          label   = "storm/carousel",
          min     = 1,
          max     = 5,
          value   = 1
          ),
        shiny::textAreaInput(
          ns("fileUploadNotes"),
          "notes (applied to all samples)",
          resize = "vertical",
          value = NULL
          ),
        shiny::actionButton(
          ns("submitFileUpload"),
          "submit data"
          ),
        br(),
        br()
        ), # close the left col

      shiny::column(
        id = "fileUploadMiddlePanel", 5,
        DT::dataTableOutput(ns("buttonsInTable")),
        tags$script(
          HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
            Shiny.unbindAll($('#'+id).find('table').DataTable().table().node()); })")
        ) # notable stmt
        ), # close the middle col

      shiny::column(
        id = "fileUploadRightPanel", 5,
        DT::dataTableOutput(ns("checked"))
      ) # close the right col

    ) # close the row


  ) # close the taglist

} # close upload_reportUI


upload_report <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

  # site id from file upload
  sampleReportSiteId <- reactive({

    req(input$file1)

    readr::read_csv(
      input$file1$datapath,
      n_max = 1,
      col_names = c("reportText", "siteID")) |>
    dplyr::select(siteID) |>
    unlist(., use.names = FALSE)

  })


  # max date (for adding blank) from file upload
  sampleReportMaxDate <- reactive({

    maxDate <- readr::read_csv(
      input$file1$datapath,
      skip = 7,
      col_names = c("sample_datetime", "eventNumber")) |>
    dplyr::select(sample_datetime) |>
    dplyr::filter(!is.na(sample_datetime)) |>
    dplyr::mutate(
      sample_datetime = lubridate::parse_date_time(sample_datetime, c("mdY HMS p", "mdY HMS", "mdY HM"))
      ) |>
    dplyr::summarise(maxDTTM = max(sample_datetime))

    # because this function is to add a blank, set the time according to
    # prescribed approach for blank times (e.g., blank #2 = 00:00:20)
    lubridate::hour(maxDate$maxDTTM)   = 0
    lubridate::minute(maxDate$maxDTTM) = 0
    lubridate::second(maxDate$maxDTTM) = input$storm*10

    return(maxDate$maxDTTM)

  }
  )


  # sample data from file upload
  sampleReportData <- reactive({

    session$sendCustomMessage('unbind-DT', 'buttonsInTable') # notable stmt

    req(input$file1)

    readr::read_csv(
      input$file1$datapath,
      skip = 7,
      col_names = c("sample_datetime", "eventNumber")
      ) |>
    dplyr::mutate(bottle = paste0(sampleReportSiteId(), "_", input$storm, "_", eventNumber)) |>
    dplyr::select(bottle, sample_datetime) |>
    dplyr::filter(!is.na(sample_datetime)) |>
    dplyr::mutate(sample_datetime = lubridate::parse_date_time(sample_datetime, c("mdY HMS p", "mdY HMS", "mdY HM"))) |>
    tibble::add_row(
      bottle = paste0(sampleReportSiteId(), "_", input$storm, "_BLK"),
      sample_datetime = sampleReportMaxDate()) |>
    dplyr::mutate(sample_datetime = format(sample_datetime, "%Y-%m-%d %H:%M:%S"))

  })


  # render the file upload and data input tools
  output$buttonsInTable <- DT::renderDataTable({

    sampleReportData() |>
    dplyr::mutate(
      omit = shinyInputOther(
        checkboxInput,
        nrow(sampleReportData()),
        "omit_",
        value = FALSE,
        width = "20px"
      )
    )
  },
  selection = 'none',
  escape = FALSE,
  server = FALSE,
  options = list(
    bFilter = 0,
    bLengthChange = FALSE,
    bPaginate = FALSE,
    bSort = FALSE,
    preDrawCallback = JS('function() {
      Shiny.unbindAll(this.api().table().node()); }'),
      drawCallback = JS('function() {
        Shiny.bindAll(this.api().table().node()); } ')
        ),
      rownames = FALSE
    ) # close renderDataTable


    # capture file upload and provided data
    combinedData <- reactive({

      sampleReportData() |>
      dplyr::mutate(
        omit = shinyValue(
          "omit_",
          nrow(sampleReportData())
          ),
        notes = input$fileUploadNotes
        ) |>
      filter(omit == FALSE)

    })

    # preview data table with upload and provided values
    output$checked <- DT::renderDataTable({

      combinedData() |>
      dplyr::mutate(sample_datetime = as.character(sample_datetime)) |>
      dplyr::select(-omit)
    },
    selection = 'none',
    escape = FALSE,
    server = FALSE,
    options = list(
      bFilter = 0,
      bLengthChange = FALSE,
      bPaginate = FALSE,
      bSort = FALSE
      ),
    rownames = FALSE
    ) # close output$checked

    # write uploaded file and edits to database
    observeEvent(input$submitFileUpload, {

      # modify data object as needed for the DB
      sampleUploadToWrite <- combinedData() |>
      dplyr::mutate(
        sample_datetime = as.POSIXct(sample_datetime, format = "%Y-%m-%d %H:%M:%S"),
        site_id = sampleReportSiteId()
        ) |>
      dplyr::select(-omit)

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

        shiny::showNotification(
          ui = "sample data uploaded",
          duration = NULL,
          closeButton = TRUE,
          type = 'message',
          action = a(href = "javascript:location.reload();", "reload the page")
        )

      }, warning = function(warn) {

        shiny::showNotification(
          ui = paste("there is a warning:  ", warn),
          duration = NULL,
          closeButton = TRUE,
          type = 'warning'
        )

        print(paste("WARNING: ", warn))

      }, error = function(err) {

        shiny::showNotification(
          ui = paste("there was an error:  ", err),
          duration = NULL,
          closeButton = TRUE,
          type = 'error'
        )

        print(paste("ERROR: ", err))

      }) # close try catch

    }) # close observeEvent(input$submitFileUpload...

  }) # close moduleServer

} # close module function
