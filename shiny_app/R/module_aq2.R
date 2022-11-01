#' @title Module to faciliate uploading AQ2 data
#'
#' @description The module aq2 facilitates uploading aq2 data. The user
#'   attaches the appropriate sample details to uploaded data. Upon execution,
#'   the munged data with sample and analysis details is written to
#'   stormwater.results upon which, if successful, the imported data are written
#'   to stormwater.lachat.
#'
#' @export

# upload UI ---------------------------------------------------------------

upload_aq2UI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    fluidPage(
      fluidRow(

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


    # helper function for reading input functions; for reasons that are completely
    # unclear, this function only works if included in app.R or function section
    # if a module (i.e., it is loaded but does not seem to work if loaded from
    # helper_shiny_input.R)
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
      aq2Upload <- read.csv(file = machineInputs$machineFile()$datapath)

      # check data structure - warning only, does not break workflow
      if (ncol(aq2Upload) != 10) {

        shiny::showNotification(
          ui          = "unexpected data structure: check number and names of columns",
          duration    = NULL,
          closeButton = TRUE,
          type        = "warning"
        )

      }

      # add filename as a variable
      aq2Upload$filename <- machineInputs$machineFile()$name

      # format column names
      colnames(aq2Upload) <- tolower(colnames(aq2Upload))             # colnames to lowercase
      colnames(aq2Upload) <- gsub("\\.", "\\_", colnames(aq2Upload))  # replace dots with underscores
      colnames(aq2Upload) <- gsub(" ", "\\_", colnames(aq2Upload))    # replace spaces with underscores

      # add run identifier as maxrun
      maxrun           <- as.numeric(run_interpolated_query(interpolatedQuery = "SELECT MAX(run_id) FROM stormwater.results;"))
      aq2Upload$run_id <- maxrun + 1

      # add a join field
      aq2Upload <- aq2Upload |>
        dplyr::mutate(
          idToJoin = toupper(trimws(sample_id)),
          idToJoin = gsub("(\\w+\\.\\w+)(\\s[0-9].+)", "\\1", idToJoin),
          idToJoin = gsub("\\.", "\\_", idToJoin)
        )

      # join aq2 to sample list (if possible sans creating ambiguous samples)
      if (nrow(aq2Upload |> left_join(machineInputs$samples(), by = c("idToJoin" = "bottle"))) > nrow(aq2Upload)) {

        aq2Upload <- aq2Upload |>
        dplyr::mutate(samples = as.character(NA))

        shiny::showNotification(
          ui          = "cannot guess sample IDs, enter all IDs or try narrowing the range of sample choices",
          duration    = NULL,
          closeButton = TRUE,
          type        = "warning"
        )

      } else {

        aq2Upload <- aq2Upload |>
          left_join(machineInputs$samples() |> dplyr::select(-sample_id), by = c("idToJoin" = "bottle"))

      }

      # return modified object
      return(aq2Upload)

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
      mutate(
        newSample = shinyInputOther(FUN = selectInput,
          len = nrow(resultReactive()),
          id = paste0(session$ns("newSample_")),
          choices = c("NULL", machineInputs$samples()$samples),
          width = "220px"),
        omit = shinyInputOther(checkboxInput,
          nrow(resultReactive()),
          id = paste0(session$ns("omit_")),
          value = FALSE,
          width = "20px"),
        replicate = shinyInputOther(FUN = selectInput,
          len = nrow(resultReactive()),
          id = paste0(session$ns("rep_")),
          choices = c(1, 2, 3),
          width = "40px"),
        comments = shinyInputOther(FUN = textInput,
          len = nrow(resultReactive()),
          id = paste0(session$ns("comments_")),
          width = "120px")
        ) |>
      select(samples, newSample, omit, replicate, comments, everything()) |>
      select(-idToJoin, -run_id)
    },
    selection = "none",
    escape = FALSE,
    server = TRUE, # use server-side to accomodate large tables
    options = list(bFilter = 0,
      bLengthChange = F,
      bPaginate = F,
      bSort = F,
      preDrawCallback = JS("function() {
        Shiny.unbindAll(this.api().table().node()); }"),
        drawCallback = JS("function() {
          Shiny.bindAll(this.api().table().node()); } ")
          ),
        rownames = F
      ) # close output$rawView


      # capture file upload and provided data
      resultsMetadata <- reactive({

        resultReactive() |>
          mutate(
            newSample = shinyValue(id = "newSample_",
              len = nrow(resultReactive())),
            omit = shinyValue(id = "omit_",
              len = nrow(resultReactive())),
            replicate = shinyValue(id = "rep_",
              len = nrow(resultReactive())),
            comments = shinyValue(id = "comments_",
              len = nrow(resultReactive()))
            ) |>
        mutate(newSample = as.character(newSample)) |> # cast newSample to char to avoid case_when logical errors
        filter(omit == FALSE)

      })


      # preview data table with provided metadata
      output$resultsMetadataView <- DT::renderDataTable({

        resultsMetadata() |>
          mutate(
            comments = case_when(
              grepl("blk", sample_id, ignore.case = T) & comments == "" ~ "blank",
              grepl("blk", sample_id, ignore.case = T) & comments != "" ~ paste(comments, "blank", sep = "; "),
              TRUE ~ as.character(comments))
            ) |>
        mutate(
          newSample = replace(newSample, newSample == "NULL", NA),
          samples = case_when(
            !is.na(newSample) ~ newSample,
            TRUE ~ samples
          )
          ) |>
        select(-omit) |>
        select(samples, replicate, comments, sample_id, test, absorbance, date_and_time)

      },
      selection = "none",
      escape = FALSE,
      server = FALSE,
      options = list(bFilter = 0,
        bLengthChange = F,
        bPaginate = F,
        bSort = F
        ),
      rownames = F) # close output$resultsMetadataView


      # write data to database --------------------------------------------------

      observeEvent(machineInputs$submit(), {

        # run a series of data validations

        # 1. check if any samples not flagged to omit are missing a sample ID
        if (

          any(
            is.na(
              resultsMetadata() |>
                mutate(
                  newSample = replace(newSample, newSample == "NULL", NA),
                  samples = case_when(
                    !is.na(newSample) ~ newSample,
                    TRUE ~ samples
                  )
                  ) |>
              filter(omit == FALSE) |>
              pull(samples)
            ) # close is.na
          ) # close any

          ) {

          showNotification(ui = "at least one sample missing sample ID or flag to omit",
            duration = NULL,
            closeButton = TRUE,
            type = "error")

          # 2. check for duplicate combinations of: sample ID x replicate x test
          # discounting samples flagged for omit
        } else if (

          anyDuplicated(
            resultsMetadata() |>
              mutate(
                newSample = replace(newSample, newSample == "NULL", NA),
                samples = case_when(
                  !is.na(newSample) ~ newSample,
                  TRUE ~ samples
                )
                ) |>
            filter(omit == FALSE) |>
            select(samples, replicate, test)
          ) # close anyDuplicated

          ) {

          showNotification(ui = "at least one duplicate: sample ID*replicate*omit*test",
            duration = NULL,
            closeButton = TRUE,
            type = "error")

          # else proceed through workflow
        } else {

          # workflow: RAW

          # rename raw and results data for easier reference
          temp_raw <- rawReactive()

          # write temporary table: raw data

          if (dbExistsTable(stormPool, c("stormwater", "temp_raw"))) {

            dbRemoveTable(stormPool, c("stormwater", "temp_raw"))

          }

          dbWriteTable(conn = stormPool,
            name = c("stormwater", "temp_raw"),
            value = temp_raw,
            row.names = F)

          # build raw insert query
          insert_raw_query <- build_insert_raw_query(currentTab = tab())

          # workflow: RESULTS

          # format resultsMetadata() for insert
          temp_results <- format_raw(
            annotatedData = resultsMetadata(),
            sampleMetadata = machineInputs$samples(),
            currentTab = tab()
          )

          # debuggging ----
          # print(temp_results)
          # print(str(temp_results))
          # end

          # write temporary table: results data

          if (dbExistsTable(stormPool, c("stormwater", "temp_results"))) {

            dbRemoveTable(stormPool, c("stormwater", "temp_results"))

          }

          dbWriteTable(conn = stormPool,
            name = c("stormwater", "temp_results"),
            value = temp_results,
            row.names = F)

          # build results insert query
          insert_results_query <- build_insert_results_query(currentTab = tab())


          # begin tryCatch - transaction
          tryCatch({

            poolWithTransaction(stormPool, function(conn) {

              dbExecute(conn,
                insert_raw_query)

              dbExecute(conn,
                insert_results_query)

          })

            showNotification(ui = "successfully uploaded",
              duration = NULL,
              closeButton = TRUE,
              type = "message",
              action = a(href = "javascript:location.reload();", "reload the page"))

          }, warning = function(warn) {

            showNotification(ui = paste("there is a warning:  ", warn),
              duration = NULL,
              closeButton = TRUE,
              type = "warning")

            print(paste("WARNING: ", warn))

          }, error = function(err) {

            showNotification(ui = paste("there was an error:  ", err),
              duration = NULL,
              closeButton = TRUE,
              type = "error")

            print(paste("ERROR: ", err))
            print("ROLLING BACK TRANSACTION")

          }) # close try catch

        } # close if-validations

        # remove temporary tables

        if (dbExistsTable(stormPool, c("stormwater", "temp_raw"))) {

          dbRemoveTable(stormPool, c("stormwater", "temp_raw"))

        }

        if (dbExistsTable(stormPool, c("stormwater", "temp_results"))) {

          dbRemoveTable(stormPool, c("stormwater", "temp_results"))

        }

      }) # close submitData


      # debugging: module level -------------------------------------------------

      # observe(print({ machineInputs$samples() }))
      # observe(write_csv(machineInputs$samples(), "~/Desktop/machineinputs.csv"))
      # observe(write_csv(rawReactive(), "~/Desktop/rawreactive.csv"))
      # observe(print({ rawReactive() }))
      # observe(print({ resultsMetadata() }))
      # observe(print({ str(resultsMetadata()) }))
      # observe(print({ head(resultReactive()) }))
      # observe(print({ samplesSelection() }))


      # close module aq2 ----------------------------------------------------

}) # close module server
} # close module function
