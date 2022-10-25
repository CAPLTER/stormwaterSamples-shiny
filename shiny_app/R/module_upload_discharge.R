#' @title module: upload report
#'
#' @description Module to facilitate uploading level (bubbler) data from the
#' ISCO 6700
#'
#' @export

upload_dischargeUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidRow(
      shiny::column(
        id = "readme_row", width = 12,
        shiny::div(id = "readme_box",
          shiny::strong("README"),
          "Navigate to the 6700 file with the level data. If imported correctly, the first and last few lines of the data in the ISCO file along with a plot of all data will be displayed. If the data appear accurate, especially check the site and start- and ending-times, use the submit data button to upload the data to the database. You will need to reload the app after uploading."
        ) # close readme div
      )   # close readme column
      ),  # close readme row

    shiny::fluidRow(

      shiny::column(
        id = "dischage_left_column", 2,
        shiny::wellPanel(
          shiny::fileInput(
            inputId  = ns("isco_6700_level_file"),
            label    = "choose csv file",
            multiple = FALSE,
            accept   = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
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
        )   # close the left column
        ),  # close the well panel

      shiny::column(
        id = "dischage_middle_column", 5,

        div(id = "div_head"),
        shiny::tableOutput(ns("level_data_head")),

        div(id = "div_tail"),
        shiny::tableOutput(ns("level_data_tail"))
        ), # close the middle column

      shiny::column(
        id = "dischage_right_column", 5,
        shiny::plotOutput(ns("level_data_plot"))
      ) # close the right column

    ) # close the row
  ) # close the taglist
} # close dischargeUI


upload_discharge <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # establish reactive placeholder to store report data
    discharge_data <- shiny::reactiveValues()


    # import the level data
    shiny::observeEvent(input$isco_6700_level_file, {

      discharge_data[["level"]] <- harvest_level_data(input$isco_6700_level_file)

      shiny::insertUI(
        selector = "#div_head",
        where    = "beforeBegin",
        ui       = tags$h4("top of record")
      )

      shiny::insertUI(
        selector = "#div_tail",
        where    = "beforeBegin",
        ui       = tags$h4("bottom of record")
      )

      })


    # report head of imported level data
    output$level_data_head <- shiny::renderTable({

      head(discharge_data[["level"]])

    }, 
    striped = TRUE
    )

    # report tail of imported level data
    output$level_data_tail <- shiny::renderTable({

      tail(discharge_data[["level"]])

    }, 
    striped = TRUE
    )


    # report plot of imported level data
    output$level_data_plot <- shiny::renderPlot({

      plot_discharge <- function(discharge_data) {

        data_to_plot <- discharge_data |>
          dplyr::mutate(event_datetime = as.POSIXct(event_datetime, format = "%Y-%m-%d %H:%M:%S"))

        plot(
          x    = data_to_plot$event_datetime,
          y    = data_to_plot$water_height,
          pch  = 21,
          col  = "blue",
          bg   = "blue",
          xaxt = "n",
          xlab = "date_time",
          ylab = "water_height"
        )

        axis(
          side      = 1,
          at        = data_to_plot$event_datetime,
          labels    = format(data_to_plot$event_datetime, format = "%H:%M:%S"),
          col.ticks = NA
        )

      }

      if (!is.null(discharge_data[["level"]])) {

        plot_discharge(discharge_data[["level"]])

      }      

    })


    # upload the data to the database
    shiny::observeEvent(input$submitFileUpload, {

      maxrun <- as.numeric(run_interpolated_query(interpolatedQuery = "SELECT MAX(run_id) FROM stormwater.discharge ;"))
      maxrun <- maxrun + 1

      discharge_upload <- discharge_data[["level"]] |>
        dplyr::mutate(run_id = maxrun)

      tryCatch({

        pool::poolWithTransaction(
          pool = this_pool,
          func = function(conn) {

            samples_inserted <- DBI::dbWriteTable(
              conn      = this_pool,
              name      = c("stormwater", "discharge"),
              value     = discharge_upload,
              overwrite = FALSE,
              append    = TRUE,
              row.names = FALSE,
              col.names = c(site_id, event_datetime, water_height, source_file, run_id)
            )

          }
        ) # close poolWithTransaction

        shiny::showNotification(
          ui          = paste0("uploaded ", nrow(discharge_upload), " discharge"),
          duration    = NULL,
          closeButton = TRUE,
          type        = "message",
          action      = a(href = "javascript:location.reload();", "reload the page")
        )

        shiny::updateTextAreaInput(
          inputId = "fileUploadNotes",
          label   = "notes (applied to all discharge)",
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

    # observe(print({ head(discharge_data[["level"]]) }))
    # observe(print({ input$sample_report_table_rows_selected }))
    # observe(print({ length(input$sample_report_table_rows_selected) }))
    # observe(print({ class(discharge_data[["discharge"]]) }))
    # observe(print({ head(sampleReportData()) }))
    # observe(print({ head(sampleReportData()[["bottle"]]) }))

  }) # close moduleServer
} # close module function
