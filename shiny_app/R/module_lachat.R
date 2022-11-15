#' @title Module to faciliate uploading lachat data
#'
#' @description The module upload_lachat facilitates uploading lachat data. The
#' user attaches the appropriate sample details to uploaded data. Upon
#' execution, the munged data with sample and analysis details are written to
#' stormwater.results and the raw, imported data are written to
#' stormwater.lachat.
#'
#' @export

# upload UI ---------------------------------------------------------------

upload_lachatUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(
      shiny::fluidRow(

        shiny::column(
          id = "leftPanel", 2,

          shiny::div(
            id = "nitrite_box",
            "IMPORTANT: check this box first if data to upload are nitrite (NO2)",
            shiny::checkboxInput(
              inputId = ns("nitriteFlag"),
              label   = "data are nitrite"
            )
            ), # close nitrite box div
          machineInputUI(ns("samples_for_lachat")) # ns(wrap call to inner mod)
          ), # close the left col

        shiny::column(
          id = "rightPanel", 10,
          DT::dataTableOutput(ns("resultView")),
          shiny::uiOutput(ns("mergedPreviewDivider")),
          DT::dataTableOutput(ns("resultsMetadataView"))
        ) # close the right col

      ) # close the row
    ) # close the page

  ) # close tagList

} # close lachatUI


# upload main -------------------------------------------------------------

upload_lachat <- function(id, tab = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # call module machineInput: builds sample list & machine file import
    machineInputs <- machineInput("samples_for_lachat")


    # helper function for reading input functions; for reasons that are not
    # clear, this function only works if included in the .R file from which  it
    # is called.
    shinyValue <- function(id, len) {
      unlist(lapply(seq_len(len), function(i) {
          value = input[[paste0(id, i)]]
          if (is.null(value)) NA else value
      }))
    }


    # import and process machine output ---------------------------------------

    # raw data imported from output (file)
    rawReactive <- shiny::reactive({

      # require file input
      req(machineInputs$machineFile())

      # import file and remove rows without concentration data
      machine_import <- readxl::read_excel(
        path = machineInputs$machineFile()$datapath
        ) |>
      dplyr::filter(!is.na(`Peak Concentration`))

      # check data structure - warning only, does not break workflow
      if (ncol(machine_import) != 28) {

        shiny::showNotification(
          ui          = "unexpected data structure: check number and names of columns",
          duration    = NULL,
          closeButton = TRUE,
          type        = "warning"
        )

      }

      # add filename as a variable
      machine_import$filename <- machineInputs$machineFile()$name

      # format column names
      colnames(machine_import) <- tolower(colnames(machine_import))             # colnames to lowercase
      colnames(machine_import) <- gsub("\\.", "\\_", colnames(machine_import))  # replace dots with underscores
      colnames(machine_import) <- gsub(" ", "\\_", colnames(machine_import))    # replace spaces with underscores

      machine_import <- machine_import |>
      dplyr::rename(weight_units = `weight_(units)`)

      # add run identifier as maxrun
      maxrun                <- as.numeric(run_interpolated_query(interpolatedQuery = "SELECT MAX(run_id) FROM stormwater.results;"))
      machine_import$run_id <- maxrun + 1

      # add a join field
      machine_import <- machine_import |>
      dplyr::mutate(
        idToJoin = toupper(trimws(sample_id)),
        idToJoin = stringr::str_extract(idToJoin, "([0-9]+\\.[0-9]+\\.\\w+)"),
        idToJoin = dplyr::case_when(
          grepl("unknown", sample_type, ignore.case = T) ~ gsub("\\.", "\\_", idToJoin),
          TRUE ~ idToJoin
        )
      )

      machine_import <- join_sample_metadata(
        this_machine_import  = machine_import,
        this_sample_metadata = machineInputs$samples()
      )

    })


    # results reactive --------------------------------------------------------

    resultReactive <- shiny::reactive({

      lachatResults <- rawReactive() |>
      dplyr::filter(
        grepl("unknown", sample_type, ignore.case = TRUE),
        !grepl("om|tres|tr", sample_id, ignore.case = TRUE),
      )

      return(lachatResults)

    })


    # add visual separator between dynamic data and preview of data to upload
    output$mergedPreviewDivider <- shiny::renderUI({

      req(machineInputs$machineFile())

      shiny::tagList(
        br(),
        p("preview data to upload",
          style = "text-align: left; background-color: LightGray; color: black;")
      )

    })


    # render results ----------------------------------------------------------

    output$resultView <- DT::renderDataTable({

      resultReactive() |>
      dplyr::mutate(
        newSample = shinyInputOther(
          FUN     = selectInput,
          len     = nrow(resultReactive()),
          id      = paste0(session$ns("newSample_")),
          choices = c("NULL", machineInputs$samples()$samples),
          width   = "220px"
          ),
        omit = shinyInputOther(
          FUN   = checkboxInput,
          len   = nrow(resultReactive()),
          id    = paste0(session$ns("omit_")),
          value = FALSE,
          width = "20px"
          ),
        replicate = shinyInputOther(
          FUN     = selectInput,
          len     = nrow(resultReactive()),
          id      = paste0(session$ns("rep_")),
          choices = c(1, 2, 3),
          width   = "40px"
          ),
        comments = shinyInputOther(
          FUN    = textInput,
          len    = nrow(resultReactive()),
          id     = paste0(session$ns("comments_")),
          width  = "120px"
        )
        ) |>
      dplyr::select(samples, newSample, omit, replicate, comments, everything()) |>
      dplyr::select(-idToJoin, -run_id)

    },
    selection = "none",
    escape    = FALSE,
    server    = TRUE, # use server-side to accomodate large tables
    rownames  = FALSE,
    options   = list(
      scrollX         = TRUE,
      autoWidth       = TRUE,
      bFilter         = 0,
      bLengthChange   = FALSE,
      bPaginate       = FALSE,
      bSort           = FALSE,
      preDrawCallback = JS('function() {
        Shiny.unbindAll(this.api().table().node()); }'
        ),
      drawCallback    = JS('function() {
        Shiny.bindAll(this.api().table().node()); } '
        ),
      columnDefs      = list(
        list(
          targets = c(0),
          width   = "180px"
        )
      )
    )
    ) # close output$resultView


    # capture file upload and provided data
    resultsMetadata <- shiny::reactive({

      resultReactive() |>
      dplyr::mutate(
        newSample = shinyValue(
          id = "newSample_",
          len = nrow(resultReactive())
          ),
        omit = shinyValue(
          id = "omit_",
          len = nrow(resultReactive())
          ),
        replicate = shinyValue(
          id = "rep_",
          len = nrow(resultReactive())
          ),
        comments = shinyValue(
          id = "comments_",
          len = nrow(resultReactive())
        )
        ) |>
      dplyr::mutate(
        newSample = as.character(newSample),
        comments  = gsub(",", ";", comments),
        comments  = gsub("[\r\n]", "; ", comments)
        ) |> # cast newSample to char to avoid case_when logical errors
      dplyr::filter(omit == FALSE)

    })


    # preview data table with provided metadata
    output$resultsMetadataView <- DT::renderDataTable({

      resultsMetadata() |>
      dplyr::mutate(
        comments = dplyr::case_when(
          grepl("blk", sample_id, ignore.case = T) & comments == "" ~ "blank",
          grepl("blk", sample_id, ignore.case = T) & comments != "" ~ paste(comments, "blank", sep = "; "),
          TRUE ~ as.character(comments))
        ) |>
      dplyr::mutate(
        newSample = replace(newSample, newSample == "NULL", NA),
        samples   = dplyr::case_when(
          !is.na(newSample) ~ newSample,
          TRUE ~ samples
        )
        ) |>
      dplyr::mutate(
        detection_date = as.character(detection_date),
        detection_time = as.character(detection_time, format = "%H:%M:%S")
        ) |>
      dplyr::select(-omit) |>
      dplyr::select(samples, replicate, comments, sample_id, cup_number, detection_date, detection_time, analyte_name, conc_x_adf_x_mdf)

    },
    selection = "none",
    escape    = FALSE,
    server    = FALSE,
    rownames  = FALSE,
    options   = list(
      bFilter       = 0,
      bLengthChange = FALSE,
      bPaginate     = FALSE,
      bSort         = FALSE
      )
    ) # close output$resultsMetadataView


    # write data to database --------------------------------------------------

    shiny::observeEvent(machineInputs$submit(), {

      # validate sample IDs
      sample_ids_message <- check_sample_ids(resultsMetadata())

      # message and stop if invalid
      if (length(sample_ids_message) != 0) {

        notification_message <- paste(sample_ids_message, collapse = " & ")

        shiny::showNotification(
          ui          = notification_message,
          duration    = 8,
          closeButton = TRUE,
          type        = "error"
        )

      } else {

        # upload raw and results data
        chem_upload <- upload_chemistry(
          this_raw_reactive     = rawReactive(),
          this_results_reactive = resultsMetadata(),
          this_samples_metadata = machineInputs$samples(),
          this_analysis         = tab(),
          this_is_nitrite       = input$nitriteFlag # passed for lachat only
        )

        # reset nitrite flag if uploaded
        if (shiny::isTruthy(chem_upload)) {

          shiny::updateCheckboxInput(
            inputId = "nitriteFlag",
            value   = FALSE
          )

        }

      } # close database operations


      # remove temporary tables

      remove_table(
        schema_name = "stormwater",
        table_name  = "temp_results"
      )

      remove_table(
        schema_name = "stormwater",
        table_name  = "temp_raw"
      )

    }) # close submit data


    # debugging: module level -------------------------------------------------

    # observe(readr::write_csv({ rawReactive() }, "/tmp/lachat_raw.csv"))
    # observe(readr::write_csv({ resultReactive() }, "/tmp/lachat_results_reactive.csv"))
    # observe(readr::write_csv({ machineInputs$samples() }, "/tmp/lachat_metadata.csv"))
    # observe(print({ head(machineInputs$samples()) }))
    # observe(print({ head(rawReactive()) }))
    # observe(print({ head(resultsMetadata()) }))
    # observe(write_csv(machineInputs$samples(), '~/Desktop/machineinputs.csv'))
    # observe(write_csv(rawReactive(), '~/Desktop/rawreactive.csv'))
    # observe(print({ str(resultsMetadata()) }))
    # observe(print({ head(resultReactive()) }))


    # close module lachat ----------------------------------------------------

}) # close module server
} # close module function
