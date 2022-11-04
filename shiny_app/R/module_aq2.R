#' @title Module to faciliate uploading AQ2 data
#'
#' @description The module upload_aq2 facilitates uploading aq2 data. The user
#' attaches the appropriate sample details to uploaded data. Upon execution,
#' the munged data with sample and analysis details is written to
#' stormwater.results upon which, if successful, the imported data are written
#' to stormwater.lachat.
#'
#' @export

# upload UI ---------------------------------------------------------------

upload_aq2UI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(
      shiny::fluidRow(

        shiny::column(id = "leftPanel", 2,
          machineInputUI(ns("samples_for_aq2")) # ns(wrap call to inner mod)
          ), # close the left col

        shiny::column(id = "rightPanel", 10,
          DT::dataTableOutput(ns("resultView")),
          shiny::uiOutput(ns("mergedPreviewDivider")),
          DT::dataTableOutput(ns("resultsMetadataView"))
        ) # close the right col

      ) # close the row
    ) # close the page

  ) # close tagList

} # close aq2 UI


# upload main -------------------------------------------------------------

upload_aq2 <- function(id, tab = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # create listener for adding and deleting records
    # listener <- reactiveValues(dbVersion = 0)

    # call module machineInput: builds sample list & machine file import
    machineInputs <- machineInput("samples_for_aq2")


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

      # import file
      machine_import <- read.csv(file = machineInputs$machineFile()$datapath)

      # check data structure - warning only, does not break workflow
      if (ncol(machine_import) != 10) {

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

      # add run identifier as maxrun
      maxrun                <- as.numeric(run_interpolated_query(interpolatedQuery = "SELECT MAX(run_id) FROM stormwater.results;"))
      machine_import$run_id <- maxrun + 1

      # add a join field
      machine_import <- machine_import |>
      dplyr::mutate(
        idToJoin = toupper(trimws(sample_id)),
        idToJoin = gsub("(\\w+\\.\\w+)(\\s[0-9].+)", "\\1", idToJoin),
        idToJoin = gsub("\\.", "\\_", idToJoin)
      )

      machine_import <- join_sample_metadata(
        this_machine_import  = machine_import,
        this_sample_metadata = machineInputs$samples()
      )

    })


    # results reactive --------------------------------------------------------

    resultReactive <- reactive({

      aq2_results <- rawReactive() |>
      dplyr::filter(!(grepl("c c|cc|standard|tres|digested|control|om|can|apa|roos", sample_id, ignore.case = T)))

      return(aq2_results)

    })


    # add visual separator between dynamic data and preview of data to upload
    output$mergedPreviewDivider <- shiny::renderUI({

      req(machineInputs$machineFile())

      tagList(
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
    ) # close output$rawView


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
      dplyr::select(-omit) |>
      dplyr::select(samples, replicate, comments, sample_id, test, absorbance, date_and_time)

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
          this_analysis         = tab()
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

    # observe(print({ machineInputs$samples() }))
    # observe(write_csv(machineInputs$samples(), "~/Desktop/machineinputs.csv"))
    # observe(write_csv(rawReactive(), "~/Desktop/rawreactive.csv"))
    # observe(print({ rawReactive() }))
    # observe(print({ names(resultsMetadata()) }))
    # observe(print({ str(resultsMetadata()) }))
    # observe(print({ head(resultReactive()) }))
    # observe(print({ samplesSelection() }))


    # close module aq2 ----------------------------------------------------

}) # close module server
} # close module function
