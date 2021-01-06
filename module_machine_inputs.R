#' @title Module: machineInput
#'
#' @description The module machineInput facilitates identifying a set of samples
#'   based on collection site(s) and a date range that can be joined (where
#'   matches exist) to sample IDs in the imported machine output. The action
#'   button triggers an attempt to write data + metadata to the database.
#'   machineInput is basically the left column of the cations (icp), Lachat,
#'   AQ2, and Shimadzu input modules.
#'
#' @note The intitial implementation had the user identify samples for the join
#'   by selecting site(s), year, and month. However, with that approach, there
#'   was no way to disambiguate bottles between discrete storms occuring in the
#'   same month, and could thus not be joined. Though the initial approach of
#'   winnowing by year and month is a bit faster and easier than using start and
#'   end date pickers, having the finer resolution is far more important to help
#'   with joins, potentially eliminating many mouse clicks when joins are not
#'   feasible.
#'
#' @note Cannot discern any difference in functionality when Shiny bind/unbind
#'   statements are included, except that inclusion in resultsMetadata prevents
#'   results from being displayed.
# machine input UI --------------------------------------------------------
machineInputUI <- function(id) {

  ns <- NS(id)

  tagList(
    # tags$script(
    #   HTML("Shiny.addCustomMessageHandler('unbind-DT', function(id) {
    #            Shiny.unbindAll($('#'+id).find('table').DataTable().table().node());})")
    # ), # notable stmt
    tags$head(
      tags$style(
        HTML(paste0("#", ns("samplesList"), "{
                    font-size: 0.85em;
                    color: #484848;
                    overflow-y: scroll;
                    max-height: 250px;
                    background: ghostwhite;
                    text-align: left;
                    white-space: pre-wrap; }"))
      ) # close tags$style
    ), # close tagss$head
    helpText("1. identify sample set",
             style = "text-align: left; color: DarkBlue; font-weight: bold"),
    selectizeInput(inputId = ns("narrowSamplesSite"),
                   "site",
                   choices = siteAbbreviations,
                   selected = NULL,
                   multiple = TRUE),
    dateInput(inputId = ns("startDate"),
              "start:",
              format = "yyyy-mm-dd"),
    dateInput(inputId = ns("endDate"),
              "end:",
              format = "yyyy-mm-dd"),
    br(),
    verbatimTextOutput(ns("samplesList")),
    br(),
    helpText("2. select file",
             style = "text-align: left; color: DarkBlue; font-weight: bold"),
    fileInput(inputId = ns("machineOutputFile"),
              label = NULL,
              multiple = FALSE),
    helpText("3. submit data for upload",
             style = "text-align: left; color: DarkBlue; font-weight: bold"),
    actionButton(inputId = ns("submitData"),
                 label = "upload"),
    br(),
    br(),
    helpText("reference to sites",
             style = "text-align: left; color: Gray; font-weight: bold"),
    markdown("
      | id      |          | site         |
      |--------:|:--------:|:-------------|
      | 9       | ....     | LM           |
      | 10      | ....     | SGC          |
      | 11      | ....     | IBW          |
      | 12      | ....     | ENC          |
      | 13      | ....     | Ave7th       |
      | 14      | ....     | centralNorth |
      | 15      | ....     | centralSouth |
      | 16      | ....     | Price        |
    "),
    br()
  ) # close tagList

} # close machineInputUI
# machine input main ------------------------------------------------------
# main function
machineInput <- function(input, output, session) {

  # build list of sample IDs ------------------------------------------------

  # build (reactive) list of bottle IDs for given site, year, and month
  samplesSelection <- reactive({

    # session$sendCustomMessage('unbind-DT', 'resultView') # notable stmt

    req(
      input$narrowSamplesSite,
      input$startDate,
      input$endDate
    )

    # convert month abbreviations to integers for query
    # monthTibble <- tibble(number = seq(1:12), abbr = month.abb)
    # integerMonths <- glue::glue_sql(
    #   "{monthTibble[monthTibble$abbr %in% c(input$narrowSampleMonth),]$number*}"
    # )

    # convert site abbreviations to site_id for query
    integerSites <- glue::glue_sql(
      "{sampleSites[sampleSites$abbreviation %in% input$narrowSamplesSite,]$site_id*}"
    )

    start <- as.character(as.Date(input$startDate))
    end <- as.character(as.Date(input$endDate))

    # base query
    baseQuery <- "
    SELECT
      sample_id,
      samples.bottle,
      CONCAT(samples.bottle, '_', samples.sample_datetime) AS samples
    FROM stormwater.samples
    WHERE
      samples.site_id IN (?theseSites) AND
      samples.sample_datetime BETWEEN ?thisStart AND ?thisEnd
    ORDER BY
      samples.site_id,
      samples.sample_datetime;"

    # parameterized query
    parameterizedQuery <- sqlInterpolate(ANSI(),
                                         baseQuery,
                                         theseSites = integerSites,
                                         thisStart = start,
                                         thisEnd = end)

    # sample IDs subset from query
    bottleOptions <- run_interpolated_query(parameterizedQuery)

    # return list of sample IDs to populate dropdown
    return(bottleOptions)
    # return(parameterizedQuery)

  })


  # preview list of sample IDs ----------------------------------------------

  output$samplesList <- renderPrint(

    if (nrow(samplesSelection()) == 0) {
      return(NULL)
    } else {
      # samplesSelection()$samples
      writeLines(samplesSelection()$samples)
    }

  )


  # module returns ----------------------------------------------------------

  return(
    list(
      samples = reactive({ samplesSelection() }),
      machineFile = reactive({ input$machineOutputFile }),
      fileName = reactive({ input$machineOutputFile$name }),
      filePath = reactive({ input$machineOutputFile$datapath }),
      submit = reactive({ input$submitData })
    )
  )


  # debugging ---------------------------------------------------------------

  # observe(print({ resultsMetadata() }))
  # observe(print({ samplesSelection() }))


  # close module machineInput -----------------------------------------------

} # close module::machineInput
