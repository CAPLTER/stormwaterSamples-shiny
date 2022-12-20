#' @title Module to faciliate uploading ICP cation data
#' 
#' @description The module upload_cations facilitates uploading cation data.
#' The user attaches the appropriate sample details to uploaded data. Upon
#' execution, the munged data with sample and analysis details is written to
#' stormwater.results upon which, if successful, the imported data are written
#' to stormwater.icp.
#'
#' @export

# upload UI ---------------------------------------------------------------

upload_cationsUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(

      shiny::fluidRow(
        shiny::column(
          id = "readme_row", width = 12,
          shiny::div(id = "readme_box",
            shiny::strong("CAUTION"),
            "Agilent data must be exported and uploaded as a comma-separated-values (.csv) file"
          ) # close readme div
        )   # close readme column
        ),  # close readme row

      shiny::fluidRow(

        shiny::column(
          id = "leftPanel", 2,
          machineInputUI(ns("samples_for_cations")) # ns(wrap call to inner mod)
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

} # close upload_cationsUI


# upload main -------------------------------------------------------------

upload_cations <- function(id, tab = NULL) {

  shiny::moduleServer(id, function(input, output, session) {

    # call module machineInput: builds sample list & machine file import
    machineInputs <- machineInput("samples_for_cations")


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

    # raw cation data imported from icp output (file)
    rawReactive <- reactive({

      # require file input
      req(machineInputs$machineFile())

      # import file
      if (tools::file_ext(machineInputs$machineFile()$datapath) != "csv") {

        shiny::showNotification(
          ui          = "file must be of type csv",
          duration    = NULL,
          closeButton = TRUE,
          type        = "warning",
          action      = a(href = "javascript:location.reload();", "reload the page")
        )

      } else {

        suppressMessages(

          machine_import <- readr::read_csv(
            file = machineInputs$machineFile()$datapath,
            skip = 2
            ) |>
          janitor::clean_names()

        )

      }


      # check data structure (warning only, does not break workflow)
      expected_names <- c(
        "solution_label",
        "rack_tube",
        "type",
        "date_time",
        "ca_183_944_nm_ppm",
        "ca_315_887_nm_ppm",
        "ca_317_933_nm_ppm",
        "na_588_995_nm_ppm",
        "na_589_592_nm_ppm",
        "y_371_029_nm_ratio",
        "zn_202_548_nm_ppm",
        "zn_206_200_nm_ppm",
        "zn_213_857_nm_ppm"
      )

      if (!all(colnames(machine_import) %in% c(expected_names))) {

        shiny::showNotification(
          ui          = "unexpected data structure: check number and names of columns",
          duration    = NULL,
          closeButton = TRUE,
          type        = "warning"
        )

      }


      # add filename as a variable
      machine_import$filename <- machineInputs$machineFile()$name

      # add run identifier as maxrun
      maxrun                <- as.numeric(run_interpolated_query(interpolatedQuery = "SELECT MAX(run_id) FROM stormwater.results;"))
      machine_import$run_id <- maxrun + 1

      # add a join field
      machine_import <- machine_import |>
      dplyr::mutate(
        idToJoin = toupper(trimws(solution_label)),
        idToJoin = stringr::str_extract(idToJoin, "([0-9]+\\.[0-9]+\\.\\w+)"),
        idToJoin = dplyr::case_when(
          grepl("sample", type, ignore.case = TRUE) ~ gsub("\\.", "\\_", idToJoin),
          TRUE ~ idToJoin
        )
      )

      machine_import <- join_sample_metadata(
        this_machine_import  = machine_import,
        this_sample_metadata = machineInputs$samples()
      )

    })


    # results reactive --------------------------------------------------------

    resultReactive <- reactive({

      cationsResults <- rawReactive() |>
      dplyr::filter(grepl("sample", type, ignore.case = TRUE)) |>
      dplyr::mutate(
        dplyr::across(tidyselect::starts_with("ca_"), ~ stringr::str_remove_all(.x, "[A-z]+|\\s")),
        dplyr::across(tidyselect::starts_with("na_"), ~ stringr::str_remove_all(.x, "[A-z]+|\\s")),
        dplyr::across(tidyselect::starts_with("zn_"), ~ stringr::str_remove_all(.x, "[A-z]+|\\s")),
        dplyr::across(tidyselect::starts_with("y_"),  ~ stringr::str_remove_all(.x, "[A-z]+|\\s")),
        dplyr::across(tidyselect::starts_with("ca_"), as.numeric),
        dplyr::across(tidyselect::starts_with("na_"), as.numeric),
        dplyr::across(tidyselect::starts_with("zn_"), as.numeric),
        dplyr::across(tidyselect::starts_with("y_"),  as.numeric)
      )

      return(cationsResults)

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
          grepl("blk", solution_label, ignore.case = T) & comments == "" ~ "blank",
          grepl("blk", solution_label, ignore.case = T) & comments != "" ~ paste(comments, "blank", sep = "; "),
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
      dplyr::select(
        samples,
        replicate,
        comments,
        solution_label,
        rack_tube,
        date_time,
        tidyselect::starts_with("ca_183"),
        tidyselect::starts_with("na_588"),
        tidyselect::starts_with("zn_202")
      )

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
          this_results_reactive = resultsMetadata() |> dplyr::rename(sample_id = solution_label),
          this_samples_metadata = machineInputs$samples(),
          this_analysis         = tab()
        )

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

    # observe(print({ head(machineInputs$samples()) }))
    # observe(print({ head(rawReactive()) }))
    # observe(print({ head(resultReactive()) }))
    # observe(print({ colnames(rawReactive()) }))
    # observe(readr::write_csv({ resultsMetadata() }, "/tmp/cations_results_metadta.csv"))
    # observe(readr::write_csv({ machineInputs$samples() }, "/tmp/cations_metadata.csv"))
    # observe(readr::write_csv({ resultsMetadata() }, "/tmp/cations_results_metadata.csv"))
    # observe(print({ head(resultsMetadata()) }))
    # observe(print({ head(resultReactive()) }))
    # observe(print({ samplesSelection() }))
    # observe(print({ machineInputs$samples() }))
    # observe(print({ machineInputs$machineFile() }))
    # observe(print({ machineInputs$fileName() }))
    # observe(print({ machineInputs$filePath() }))
    # observe(print({ machineInputs$submit() }))
    # observe(print({ rawReactive() %>% print(n=Inf) }))
    # observe(print({ queryType$default }))
    # observe(print({ input$ReachPatchs_cell_edit }))


    # close module cations ----------------------------------------------------

}) # close module server
} # close module function
