#' @title Module: lachat
#'
#' @description The module lachat facilitates uploading lachat data. The user
#'   attaches the appropriate sample details to uploaded data. Upon execution,
#'   the munged data with sample and analysis details is written to
#'   stormwater.results upon which, if successful, the imported data are written
#'   to stormwater.lachat.
#'
#' @note Cannot discern any difference in functionality when Shiny bind/unbind
#'   statements are included, except that inclusion in resultsMetadata prevents
#'   results from being displayed.

# upload UI ---------------------------------------------------------------

lachatUI <- function(id) {

  ns <- NS(id)

  tagList(
    # tags$script(
    #   HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
    #            Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})")
    # ), # notable stmt
    tags$head(
      tags$style(
        HTML(paste0("#", ns("leftPanel"), "{
                    background: #D3D3D3;
                    color: #484848; }"))
      ) # close tags$style
    ), # close tagss$head
    fluidPage(
      fluidRow(
        column(id = 'leftPanel', 2,
               helpText("important: check this box (first) if data to upload are nitrite (NO2)",
                        style = "font-weight: bold; color: DarkBlue;"),
               checkboxInput(inputId = ns("nitriteFlag"),
                             label = HTML('<p style="color: #800080; font-weight: bold;">nitrite?</p>')),
               machineInputUI(ns("lachatSamples")) # ns(wrap call to inner mod)
        ), # close the left col
        column(id = "rightPanel", 10,
               DT::dataTableOutput(ns("resultView")),
               uiOutput(ns("mergedPreviewDivider")),
               DT::dataTableOutput(ns("resultsMetadataView"))
        ) # close the right col
      ) # close the row
    ) # close the page
  ) # close tagList

} # close lachatUI


# upload main -------------------------------------------------------------

lachat <- function(input, output, session, tab = NULL) {

  # added to facilitate renderUIs
  # ns <- session$ns

  # create listener for adding and deleting records
  # listener <- reactiveValues(dbVersion = 0)

  # call module machineInput: builds sample list & machine file import
  machineInputs <- callModule(module = machineInput,
                              id = "lachatSamples")

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

  # raw cation data imported from icp output (file)
  rawReactive <- reactive({

    # require file input
    req(machineInputs$machineFile())

    # import file and remove rows sans concentration data
    lachatUpload <- read_excel(path = machineInputs$machineFile()$datapath) %>%
      filter(!is.na(`Peak Concentration`))

    # check data structure - warning only, does not break workflow
    if (ncol(lachatUpload) != 28) {

      showNotification(ui = "unexpected data structure: check number and names of columns",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'warning')

    }

    # add filename as a variable
    lachatUpload$filename <- machineInputs$machineFile()$name

    # format column names
    colnames(lachatUpload) <- tolower(colnames(lachatUpload)) # colnames to lowercase
    colnames(lachatUpload) <- gsub("\\.", "\\_", colnames(lachatUpload)) # replace dots with underscores
    colnames(lachatUpload) <- gsub(" ", "\\_", colnames(lachatUpload)) # replace spaces with underscores

    lachatUpload <- lachatUpload %>%
      rename(weight_units = `weight_(units)`)

    # add run identifier as maxrun
    maxrun <- as.numeric(run_interpolated_query(interpolatedQuery = "SELECT MAX(run_id) FROM stormwater.results;"))
    lachatUpload$run_id <- maxrun + 1

    # add a join field
    lachatUpload <- lachatUpload %>%
      mutate(
        idToJoin = toupper(trimws(sample_id)),
        idToJoin = gsub("(\\w+\\.\\w+)(\\s[0-9].+)", "\\1", idToJoin),
        idToJoin = case_when(
          grepl("unknown", sample_type, ignore.case = T) ~ gsub("\\.", "\\_", idToJoin),
          TRUE ~ idToJoin
        )
      )

    # join lachat to sample list (if possible sans creating ambiguous samples)
    if (nrow(lachatUpload %>% left_join(machineInputs$samples(), by = c("idToJoin" = "bottle"))) > nrow(lachatUpload)) {

      lachatUpload <- lachatUpload %>%
        mutate(samples = as.character(NA))

      showNotification(ui = "cannot guess sample IDs, enter all IDs or try narrowing the range of sample choices",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'warning')

    } else {

      lachatUpload <- lachatUpload %>%
        left_join(machineInputs$samples() %>% select(-sample_id), by = c("idToJoin" = "bottle"))

    }

    # return modified object
    return(lachatUpload)

  })


  # results reactive --------------------------------------------------------

  resultReactive <- reactive({

    # session$sendCustomMessage('unbind-DT', 'resultView') # notable stmt

    lachatResults <- rawReactive() %>%
      filter(
        grepl("unknown", sample_type, ignore.case = TRUE),
        !grepl("om|tres|tr", sample_id, ignore.case = TRUE),
      )

    return(lachatResults)

  })

  # add visual separator between dynamic data and preview of data to upload
  output$mergedPreviewDivider <- renderUI({

    req(machineInputs$machineFile())

    tagList(
      br(),
      p("preview data to upload",
        style = "text-align: left; background-color: LightGray; color: black;")
    )

  })


  # render results ----------------------------------------------------------

  output$resultView <- DT::renderDataTable({

    resultReactive() %>%
      mutate(
        newSample = shinyInputOther(FUN = selectInput,
                                    len = nrow(resultReactive()),
                                    id = paste0(session$ns('newSample_')),
                                    choices = c("NULL", machineInputs$samples()$samples),
                                    width = "220px"),
        omit = shinyInputOther(checkboxInput,
                               nrow(resultReactive()),
                               id = paste0(session$ns("omit_")),
                               value = FALSE,
                               width = "20px"),
        replicate = shinyInputOther(FUN = selectInput,
                                    len = nrow(resultReactive()),
                                    id = paste0(session$ns('rep_')),
                                    choices = c(1,2,3),
                                    width = "40px"),
        comments = shinyInputOther(FUN = textInput,
                                   len = nrow(resultReactive()),
                                   id = paste0(session$ns('comments_')),
                                   width = "120px")
      ) %>%
      select(samples, newSample, omit, replicate, comments, everything()) %>%
      select(-idToJoin, -run_id)

  },
  selection = 'none',
  escape = FALSE,
  server = TRUE, # use server-side to accomodate large tables
  options = list(bFilter = 0,
                 bLengthChange = F,
                 bPaginate = F,
                 bSort = F,
                 preDrawCallback = JS('function() {
                           Shiny.unbindAll(this.api().table().node()); }'),
                 drawCallback = JS('function() {
                        Shiny.bindAll(this.api().table().node()); } ')
  ),
  rownames = F) # close output$rawView


  # capture file upload and provided data
  resultsMetadata <- reactive({

    # def'n do not want this here !!
    # session$sendCustomMessage('unbind-DT', 'resultsMetadataView') # notable stmt

    resultReactive() %>%
      mutate(
        newSample = shinyValue(id = "newSample_",
                               len = nrow(resultReactive())),
        omit = shinyValue(id = "omit_",
                          len = nrow(resultReactive())),
        replicate = shinyValue(id = "rep_",
                               len = nrow(resultReactive())),
        comments = shinyValue(id = "comments_",
                              len = nrow(resultReactive()))
      ) %>%
      mutate(newSample = as.character(newSample)) %>% # cast newSample to char to avoid case_when logical errors
      filter(omit == FALSE)

  })


  # preview data table with provided metadata
  output$resultsMetadataView <- DT::renderDataTable({

    resultsMetadata() %>%
      mutate(
        comments = case_when(
          grepl('blk', sample_id, ignore.case = T) & comments == "" ~ 'blank',
          grepl('blk', sample_id, ignore.case = T) & comments != "" ~ paste(comments, 'blank', sep = "; "),
          TRUE ~ as.character(comments))
      ) %>%
      mutate(
        newSample = replace(newSample, newSample == "NULL", NA),
        samples = case_when(
          !is.na(newSample) ~ newSample,
          TRUE ~ samples
        )
      ) %>%
      mutate(
        detection_date = as.character(detection_date),
        detection_time = as.character(detection_time, format = "%H:%M:%S")
      ) %>%
      select(-omit) %>%
      select(samples, replicate, comments, sample_id, cup_number, detection_date, detection_time, analyte_name, conc_x_adf_x_mdf)

  },
  selection = 'none',
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
          resultsMetadata() %>%
          mutate(
            newSample = replace(newSample, newSample == "NULL", NA),
            samples = case_when(
              !is.na(newSample) ~ newSample,
              TRUE ~ samples
            )
          ) %>%
          filter(omit == FALSE) %>%
          pull(samples)
        ) # close is.na
      ) # close any

    ) {

      showNotification(ui = "at least one sample missing sample ID or flag to omit",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')

      # 2. check for duplicate combinations of: sample ID x replicate x analyte
      # discounting samples flagged for omit
    } else if (

      anyDuplicated(
        resultsMetadata() %>%
        mutate(
          newSample = replace(newSample, newSample == "NULL", NA),
          samples = case_when(
            !is.na(newSample) ~ newSample,
            TRUE ~ samples
          )
        ) %>%
        filter(omit == FALSE) %>%
        select(samples, replicate, analyte_name)
      ) # close anyDuplicated

    ) {

      showNotification(ui = "at least one duplicate sample ID x replicate x omit",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')

      # else proceed through workflow
    } else {

      # workflow: RAW

      # rename raw and results data for easier reference
      temp_raw <- rawReactive()

      # write temporary table: raw data

      if (dbExistsTable(stormPool, c('stormwater', 'temp_raw'))) {

        dbRemoveTable(stormPool, c('stormwater', 'temp_raw'))

      }

      dbWriteTable(conn = stormPool,
                   name = c('stormwater', 'temp_raw'),
                   value = temp_raw,
                   row.names = F)

      # build raw insert query
      insert_raw_query <- build_insert_raw_query(currentTab = tab())

      # workflow: RESULTS

      # format resultsMetadata() for insert
      temp_results <- format_raw(annotatedData = resultsMetadata(),
                                 sampleMetadata = machineInputs$samples(),
                                 currentTab = tab(),
                                 nitrite = input$nitriteFlag)

      # debuggging ----
      # print(temp_results)
      # print(str(temp_results))
      # end

      # write temporary table: results data

      if (dbExistsTable(stormPool, c('stormwater', 'temp_results'))) {

        dbRemoveTable(stormPool, c('stormwater', 'temp_results'))

      }

      dbWriteTable(conn = stormPool,
                   name = c('stormwater', 'temp_results'),
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

      }) # close try catch

    } # close if-validations

    # remove temporary tables

    if (dbExistsTable(stormPool, c('stormwater', 'temp_raw'))) {

      dbRemoveTable(stormPool, c('stormwater', 'temp_raw'))

    }

    if (dbExistsTable(stormPool, c('stormwater', 'temp_results'))) {

      dbRemoveTable(stormPool, c('stormwater', 'temp_results'))

    }

  }) # close submitData


  # debugging: module level -------------------------------------------------

  # observe(print({ machineInputs$samples() }))
  # observe(write_csv(machineInputs$samples(), '~/Desktop/machineinputs.csv'))
  # observe(write_csv(rawReactive(), '~/Desktop/rawreactive.csv'))
  # observe(print({ rawReactive() }))
  # observe(print({ resultsMetadata() }))
  # observe(print({ str(resultsMetadata()) }))
  # observe(print({ head(resultReactive()) }))
  # observe(print({ samplesSelection() }))


  # close module lachat ----------------------------------------------------

} # close module::lachat