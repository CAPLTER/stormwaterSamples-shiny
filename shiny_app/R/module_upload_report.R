#' @title module: upload report
#'
#' @description Module to facilitate uploading sample collection details from
#' the ISCO 6700; allows the user to attach notes to some or all samples, and
#' to identify sample that should not be uploaded to the database.
#'
#' @export

upload_reportUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
        id = "readme_row", width = 12,
        shiny::div(id = "readme_box",
          shiny::strong("README"),
          "Navigate to the 6700 report file and import using the Browse button. Identify the appropriate carousel using the slider. Use the notes text box to add a note or notes that apply to all samples. You can add or edit the note of any given sample by clicking the notes field corresponding to the sample. Note, however, that editing the text dialogue box that applies notes to all samples will overwrite any notes applied to individual samples, so make sure that you get any notes that you want to apply to all samples exactly as you would like them before editing notes for an individual sample. Highlight samples (click any where in a row or <shift> and drag) in the report that should be excluded from the upload. When all notes have been applied and any samples not intended for upload have been highlighted, use the submit data button to upload data to the database."
        ) # close readme div
      ) # close readme column
      ), # close readme row

    shiny::fluidRow(

      shiny::column(
        id = "leftPanel", 2,
        shiny::wellPanel(
          shiny::fileInput(
            inputId  = ns("isco_6700_report_file"),
            label    = "choose csv file",
            multiple = FALSE,
            accept   = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
            ),
          shiny::sliderInput(
            inputId = ns("storm"),
            label   = "storm/carousel",
            min     = 1,
            max     = 5,
            value   = 1
            ),
          shiny::textAreaInput(
            inputId = ns("fileUploadNotes"),
            label   = "notes (applied to all samples)",
            resize  = "vertical",
            value   = ""
            ),
          shiny::actionButton(
            inputId = ns("submitFileUpload"),
            label   = "submit data",
            class   = "btn-success",
            style   = "color: #fff;",
            icon    = shiny::icon("plus"),
            width   = "100%"
            ),
          shiny::br()
        ) # close the left col
        ), # close the well panel

      shiny::column(
        id = "fileUploadMiddlePanel", 10,
        DT::dataTableOutput(ns("sample_report_table"))
      ) # close the right col

    ) # close the row
  ) # close the taglist
} # close upload_reportUI


upload_report <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # establish reactive placeholder to store report data
    isco_data <- shiny::reactiveValues()


    # helper: get the max data from the report for adding a blank
#     get_max_date <- function(isco_file) {
# 
#       maxDate <- readr::read_csv(
#         file      = isco_file$datapath,
#         skip      = 7,
#         col_names = c("sample_datetime", "eventNumber"),
#         locale    = readr::locale(tz = "America/Phoenix")
#         ) |>
#       dplyr::select(sample_datetime) |>
#       dplyr::filter(!is.na(sample_datetime)) |>
#       dplyr::mutate(
#         sample_datetime = lubridate::parse_date_time(sample_datetime, c("mdY HMS p", "mdY HMS", "mdY HM"))
#         ) |>
#       dplyr::summarise(maxDTTM = max(sample_datetime))
# 
#       # because this function is to add a blank, set the time according to
#       # prescribed approach for blank times (e.g., blank #2 = 00:00:20)
#       # moved to observe event input$storm
#       lubridate::hour(maxDate$maxDTTM)   = 0
#       lubridate::minute(maxDate$maxDTTM) = 0
#       lubridate::second(maxDate$maxDTTM) = 10
# 
#       return(maxDate$maxDTTM)
# 
#     }


    # helper: get the site ID for the report
#     get_site_id <- function(isco_file) {
# 
#       site_id <- readr::read_csv(
#         file      = isco_file$datapath,
#         n_max     = 1,
#         col_names = c("reportText", "siteID")
#         ) |>
#       dplyr::pull(siteID)
# 
#       return(site_id)
# 
#     }


    # helper: generate the report form the uploaded file
#     generate_report <- function(isco_file, carousel = "1") {
# 
#       site_id  <- get_site_id(isco_file)
#       max_date <- get_max_date(isco_file)
# 
#       isco_report <- readr::read_csv(
#         file      = isco_file$datapath,
#         skip      = 7,
#         col_names = c("sample_datetime", "eventNumber"),
#         locale    = readr::locale(tz = "America/Phoenix")
#         ) |>
#       dplyr::mutate(bottle = paste0(site_id, "_", carousel, "_", eventNumber)) |>
#       dplyr::select(bottle, sample_datetime) |>
#       dplyr::filter(!is.na(sample_datetime)) |>
#       dplyr::mutate(sample_datetime = lubridate::parse_date_time(sample_datetime, c("mdY HMS p", "mdY HMS", "mdY HM"))) |>
#       tibble::add_row(
#         bottle          = paste0(site_id, "_", carousel, "_BLK"),
#         sample_datetime = max_date
#         ) |>
#       dplyr::mutate(
#         notes = NA_character_
#       )
# 
#       return(isco_report)
# 
#     }


    # import the report data
    shiny::observeEvent(input$isco_6700_report_file, {

      isco_data[["samples"]] <- generate_report(input$isco_6700_report_file)

      })


    # adjust the sample data to reflect the appropriate carousel
    shiny::observeEvent(input$storm, {

      carousel_string    <- paste0("_", as.character(input$storm), "_")
      blank_time_seconds <- input$storm * 10

      if (!is.null(isco_data[["samples"]])) {

        isco_data[["samples"]] <- isco_data[["samples"]] |>
        dplyr::mutate(
          bottle = stringr::str_replace(bottle, "_\\d{1,2}_", carousel_string)
        )

        lubridate::second(isco_data[["samples"]][grepl("BLK", isco_data[["samples"]][["bottle"]], ignore.case = TRUE),]$sample_datetime) = blank_time_seconds

      }

      })


    # add notes to be applied to all samples
    shiny::observeEvent(input$fileUploadNotes, {

      if (!is.null(isco_data[["samples"]])) {

        isco_data[["samples"]] <- isco_data[["samples"]] |>
        dplyr::mutate(notes = input$fileUploadNotes)

      }

      })


    # render the file upload and data input tools
    output$sample_report_table <- DT::renderDataTable({

      isco_data[["samples"]]

    },
    selection = "multiple",
    escape    = FALSE,
    server    = TRUE, # must use server side for proxy
    rownames  = FALSE,
    options   = list(
      bFilter       = 0,
      bLengthChange = FALSE,
      bPaginate     = FALSE,
      bSort         = FALSE,
      autoWidth     = FALSE,
      columnDefs = list(
        list(
          targets = c(0, 1),
          width = "120px"
        )
      )
      ),
    editable = list(
      target  = "cell",
      disable = list(columns = c(0, 1))
    )
    ) # close renderDataTable


    # proxy the rendered table to it does not have to be redrawn with each edit
    sample_report_proxy <- DT::dataTableProxy(outputId = "sample_report_table")


    # edit the notes field of individual samples
    shiny::observeEvent(input$sample_report_table_cell_edit, {

      info <- input$sample_report_table_cell_edit
      i    <- info$row
      j    <- info$col + 1L
      v    <- info$value

      isco_data[["samples"]][i, j] <- DT::coerceValue(v, isco_data[["samples"]][i, j])

      DT::replaceData(
        proxy       = sample_report_proxy,
        data        = isco_data[["samples"]],
        resetPaging = FALSE,
        rownames    = FALSE # must match render table configuration
      )

    })


    # upload the data to the database
    shiny::observeEvent(input$submitFileUpload, {

      samples_upload <- isco_data[["samples"]] |>
      dplyr::mutate(
        sample_datetime = as.POSIXct(sample_datetime, format = "%Y-%m-%d %H:%M:%S"),
        site_id         = as.integer(stringr::str_extract(bottle, "^\\d{1,2}")),
        notes           = dplyr::case_when(
          notes == "" ~ NA_character_,
          TRUE ~ notes
          ),
        notes = gsub("[\r\n]", "; ", notes),
        notes = gsub(",", ";", notes)
        ) |>
      dplyr::rename(comments = notes)


      # remove highlighted samples
      if (length(input$sample_report_table_rows_selected)) {

        samples_upload <- samples_upload[-c(input$sample_report_table_rows_selected), ]

      }

      tryCatch({

        pool::poolWithTransaction(
          pool = this_pool,
          func = function(conn) {

            samples_inserted <- DBI::dbWriteTable(
              conn      = this_pool,
              name      = c("stormwater", "samples"),
              value     = samples_upload,
              overwrite = FALSE,
              append    = TRUE,
              row.names = FALSE,
              col.names = c(site_id, sample_datetime, bottle, comments)
            )

          }
        ) # close poolWithTransaction

        shiny::showNotification(
          ui          = paste0("uploaded ", nrow(samples_upload), " samples"),
          duration    = NULL,
          closeButton = TRUE,
          type        = "message",
          action      = a(href = "javascript:location.reload();", "reload the page")
        )

        shiny::updateTextAreaInput(
          inputId = "fileUploadNotes",
          label   = "notes (applied to all samples)",
          value   = ""
        )

      }, warning = function(warn) {

        shiny::showNotification(
          ui          = paste("there is a warning:  ", warn),
          duration    = NULL,
          closeButton = TRUE,
          type        = "warning"
        )

      }, error = function(err) {

        shiny::showNotification(
          ui          = paste("there was an error:  ", err),
          duration    = NULL,
          closeButton = TRUE,
          type        = "error"
        )

      }) # close upload try catch

    }) # close shiny::observeEvent(input$submitFileUpload)


    # debugging: module level --------------------------------------------------

    # observe(print({ input$sample_report_table_cell_edit }))
    # observe(print({ isco_data[["samples"]][1, 3] }))
    # observe(print({ head(isco_data[["samples"]]) }))
    # observe(print({ input$sample_report_table_rows_selected }))
    # observe(print({ length(input$sample_report_table_rows_selected) }))
    # observe(print({ class(isco_data[["samples"]]) }))
    # observe(print({ head(sampleReportData()) }))
    # observe(print({ head(sampleReportData()[["bottle"]]) }))

  }) # close moduleServer
} # close module function
