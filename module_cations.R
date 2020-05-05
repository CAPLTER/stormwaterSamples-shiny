#' @title Module: cations
#' 
#' @description The module cations facilitates uploading cation data. The user
#'   attaches the appropriate sample details to uploaded data. Upon execution,
#'   the munged data with sample and analysis details is written to
#'   stormwater.results upon which, if successful, the imported data are written
#'   to stormwater.icp.
#'   
#' @note Cannot discern any difference in functionality when Shiny bind/unbind
#'   statements are included, except that inclusion in resultsMetadata prevents
#'   results from being displayed.

# cations UI --------------------------------------------------------------

cationsUI <- function(id) {
  
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
                    color: #484848; }")),
        HTML(paste0("#", ns("samplesList"), "{
                    color: #484848;
                    overflow-y: scroll;
                    max-height: 250px;
                    background: ghostwhite;
                    text-align: left;
                    white-space: pre-wrap; }"))
      ) # close tags$style
    ), # close tagss$head
    fluidPage(
      fluidRow( 
        column(id = 'leftPanel', 2,
               helpText("1. identify sample set",
                        style = "text-align: left; color: DarkBlue; font-weight: bold"),
               selectizeInput(inputId = ns("narrowSamplesSite"),
                              "site",
                              choices = siteAbbreviations,
                              selected = NULL,
                              multiple = TRUE),
               selectizeInput(inputId = ns("narrowSampleMonth"),
                              "month",
                              choices = month.abb,
                              selected = NULL,
                              multiple = TRUE),
               selectizeInput(inputId = ns("narrowSampleYear"),
                              "year",
                              choices = lastFiveYears,
                              selected = NULL,
                              multiple = FALSE),
               br(),
               verbatimTextOutput(ns("samplesList")),
               # textOutput(ns("samplesList")),
               br(),
               helpText("2. select file",
                        style = "text-align: left; color: DarkBlue; font-weight: bold"),
               fileInput(inputId = ns("cationFile"),
                         label = NULL,
                         multiple = FALSE),
               helpText("3. submit data for upload",
                        style = "text-align: left; color: DarkBlue; font-weight: bold"),
               actionButton(inputId = ns("submitData"),
                            label = "upload"),
               br()
        ), # close the left col
        column(id = "rightPanel", 10,
               DT::dataTableOutput(ns("resultView")),
               uiOutput(ns("mergedPreviewDivider")),
               DT::dataTableOutput(ns("resultsMetadataView"))
        ) # close the right col
      ) # close the row
    ) # close the page
  ) # close tagList
  
} # close cationsUI 

# cations main ------------------------------------------------------------

# vector of last five year for filtering sample ids
lastFiveYears <- rev(seq(from = as.numeric(format(Sys.Date(),'%Y')) - 5,
                         to = as.numeric(format(Sys.Date(),'%Y')),
                         by = 1))


# main function
cations <- function(input, output, session) {
  
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
  
  # added to facilitate renderUIs
  # ns <- session$ns
  
  # create listener for adding and deleting records
  # listener <- reactiveValues(dbVersion = 0)
  
  
  # build list of sample IDs ------------------------------------------------
  
  # build (reactive) list of bottle IDs for given site, year, and month
  samplesSelection <- reactive({
    
    # session$sendCustomMessage('unbind-DT', 'resultView') # notable stmt
    
    req(
      input$narrowSamplesSite,
      input$narrowSampleMonth,
      input$narrowSampleYear
    )
    
    # convert month abbreviations to integers for query
    monthTibble <- tibble(number = seq(1:12), abbr = month.abb)
    integerMonths <- glue::glue_sql(
      "{monthTibble[monthTibble$abbr %in% c(input$narrowSampleMonth),]$number*}"
    )
    
    # convert site abbreviations to site_id for query
    integerSites <- glue::glue_sql(
      "{sampleSites[sampleSites$abbreviation %in% input$narrowSamplesSite,]$site_id*}"
    )
    
    # base query
    baseQuery <- "
    SELECT 
      sample_id,
      CONCAT(samples.bottle, '_', samples.sample_datetime) AS samples
    FROM stormwater.samples
    WHERE
      samples.site_id IN (?theseSites) AND
      (EXTRACT (MONTH FROM sample_datetime) IN (?theseMonth) AND EXTRACT (YEAR FROM sample_datetime) = ?thisYear)
    ORDER BY
      samples.site_id, 
      samples.sample_datetime;"
    
    # parameterized query
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         theseSites = integerSites,
                                         theseMonth = integerMonths,
                                         thisYear = input$narrowSampleYear
    )
    
    # sample IDs subset from query
    bottleOptions <- run_interpolated_query(parameterizedQuery)
    
    # return list of sample IDs to populate dropdown
    return(bottleOptions)
    
  })
  
  
  # generate a preview of samples available to associate with cation data
  # output$samplesList <- renderText(
  output$samplesList <- renderPrint(
    
    if (nrow(samplesSelection()) == 0) {
      return(NULL)
    } else {
      # samplesSelection()$samples
      writeLines(samplesSelection()$samples)
    }
    
  )
  
  
  # import icp data file ----------------------------------------------------
  
  # raw cation data imported from icp output (file)
  rawReactive <- reactive({
    
    # require file input
    req(input$cationFile)
    
    # import file with params
    cationsUpload <- read_excel(path = input$cationFile$datapath,
                                skip = 4)
    
    # confirm appropriate file structure
    expectedColumnNames <- tolower(c("Ca3158","Ca3179","Ca3933","Na5889","Na5895","Zn2025","Zn2138"))
    
    if (ncol(cationsUpload) != 11 | !all(expectedColumnNames %in% tolower(colnames(cationsUpload)))) {
      
      showNotification(ui = "unexpected data structure: check number and names of columns",
                       duration = NULL,
                       closeButton = TRUE,
                       type = 'error')
      
    }
    
    # add filename as a variable
    cationsUpload$filename <- input$cationFile$name
    
    # format column names
    colnames(cationsUpload) <- tolower(colnames(cationsUpload)) # colnames to lowercase
    colnames(cationsUpload) <- gsub("\\.", "\\_", colnames(cationsUpload)) # replace dots with underscores
    
    # add missing column names
    colnames(cationsUpload)[1] <- "date_analyzed"
    colnames(cationsUpload)[2] <- "temp_out_id"
    colnames(cationsUpload)[3] <- "operator"
    colnames(cationsUpload)[4] <- "icp_id"
    
    # add run identifier as maxrun
    maxrun <- as.numeric(run_interpolated_query(interpolatedQuery = "SELECT MAX(run_id) FROM stormwater.results;"))
    cationsUpload$run_id <- maxrun + 1
    
    # return modified object
    return(cationsUpload) 
    
  })
  
  
  # results reactive --------------------------------------------------------
  
  resultReactive <- reactive({
    
    # session$sendCustomMessage('unbind-DT', 'resultView') # notable stmt
    
    cationsResults <- rawReactive() %>% 
      filter(!grepl('Blank|CalibStd|QC|Tank', temp_out_id, ignore.case = F)) %>% 
      select(-c(ca3158, ca3179, na5895, zn2025, filename))
    
    return(cationsResults)
    
  })
  
  # add visual separator between dynamic data and preview of data to upload
  output$mergedPreviewDivider <- renderUI({
    
    req(input$cationFile)
    
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
        sampleID = shinyInputOther(FUN = selectInput,
                                   len = nrow(resultReactive()),
                                   id = paste0(session$ns('sampID_')),
                                   choices = samplesSelection()$samples,
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
      select(sampleID, omit, replicate, comments, everything())
    
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
        sampleID = shinyValue(id = "sampID_",
                              len = nrow(resultReactive())),
        omit = shinyValue(id = "omit_",
                          len = nrow(resultReactive())),
        replicate = shinyValue(id = "rep_",
                               len = nrow(resultReactive())),
        comments = shinyValue(id = "comments_",
                              len = nrow(resultReactive()))
      ) %>%
      filter(omit == FALSE)
    
  })
  
  # preview data table with provided metadata
  output$resultsMetadataView <- DT::renderDataTable({
    
    resultsMetadata() %>% 
      mutate(
        comments = case_when(
          grepl('blk', temp_out_id, ignore.case = T) & comments == "" ~ 'blank',
          grepl('blk', temp_out_id, ignore.case = T) & comments != "" ~ paste(comments, 'blank', sep = "; "),
          TRUE ~ as.character(comments))
      ) %>% 
      select(-omit) %>% 
      select(sampleID, replicate, comments, everything())
    
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
  
  observeEvent(input$submitData, {
    
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
    insert_raw_cation_query <- build_insert_raw_cation_query()
    
    # workflow: RESULTS
    
    # format resultsMetadata() for insert
    temp_results <- icp_to_rslt(cationDataFormatted = resultsMetadata(),
                                sampleMetadata = samplesSelection())
    
    
    # write temporary table: results data
    
    if (dbExistsTable(stormPool, c('stormwater', 'temp_results'))) {
      
      dbRemoveTable(stormPool, c('stormwater', 'temp_results'))
      
    }
    
    dbWriteTable(conn = stormPool,
                 name = c('stormwater', 'temp_results'),
                 value = temp_results,
                 row.names = F)
    
    # build results insert query
    insert_results_cation_query <- build_insert_results_cation_query()
    
    
    # begin tryCatch - transaction
    tryCatch({
      
      poolWithTransaction(stormPool, function(conn) {
        
        dbExecute(conn,
                  insert_raw_cation_query)
        
        dbExecute(conn,
                  insert_results_cation_query)
        
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
    
    
    # remove temporary tables
    
    if (dbExistsTable(stormPool, c('stormwater', 'temp_raw'))) {
      
      dbRemoveTable(stormPool, c('stormwater', 'temp_raw'))
      
    }
    
    if (dbExistsTable(stormPool, c('stormwater', 'temp_results'))) {
      
      dbRemoveTable(stormPool, c('stormwater', 'temp_results'))
      
    }
    
  }) # close submitData 
  
  
  # debugging: module level -------------------------------------------------
  
  # observe(print({ head(resultReactive()) }))
  # observe(print({ resultsMetadata() }))
  # observe(print({ samplesSelection() }))
  # observe(print({ queryType$default }))
  # observe(print({ input$ReachPatchs_cell_edit }))
  
  
  # close module cations ----------------------------------------------------
  
} # close module::cations