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

  ns <- shiny::NS(id)

  shiny::tagList(

    tags$head(
      tags$style(
        HTML(
          paste0("#", ns("samplesList"), "{
            font-size: 0.85em;
            color: #484848;
            overflow-y: scroll;
            overflow-x: scroll;
            max-height: 250px;
            background: ghostwhite;
            text-align: left;
            white-space: pre;
            }"
          )
        )
      )  # close tags$style
      ), # close tags$head

    shiny::wellPanel(

      shiny::helpText(
        "identify sample set",
        style = "text-align: left; color: DarkBlue; font-weight: bold"
        ),
      shiny::selectizeInput(
        inputId  = ns("narrowSamplesSite"),
        label    = "site (LM ~ 9, SGC ~ 10, IBW ~ 11)",
        choices  = siteAbbreviations,
        selected = NULL,
        multiple = TRUE
        ),
      shiny::dateInput(
        inputId = ns("startDate"),
        label   = "start:",
        value   = Sys.Date() - lubridate::weeks(24),
        format  = "yyyy-mm-dd"
        ),
      shiny::dateInput(
        inputId = ns("endDate"),
        label   = "end:",
        format  = "yyyy-mm-dd"
        ),
      shiny::br(),
      shiny::verbatimTextOutput(ns("samplesList")),
      shiny::br(),
      shiny::helpText(
        "import chemistry data",
        style = "text-align: left; color: DarkBlue; font-weight: bold"
        ),
      shiny::fileInput(
        inputId  = ns("machineOutputFile"),
        label    = NULL,
        multiple = FALSE
        ),
      shiny::helpText(
        "submit data for upload",
        style = "text-align: left; color: DarkBlue; font-weight: bold"
        ),
      shiny::actionButton(
        inputId = ns("submitData"),
        label   = "submit data",
        class   = "btn-success",
        style   = "color: #fff;",
        icon    = shiny::icon("plus"),
        width   = "100%"
        ),
      shiny::br()

      ) # close wellPanel
    ) # close tagList

} # close machineInputUI


# machineInput -----------------------------------------------------------------

machineInput <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # build reactive list of bottle IDs for given site, year, month
    samplesSelection <- shiny::reactive({

      req(
        input$narrowSamplesSite,
        input$startDate,
        input$endDate
      )

      # convert site abbreviations to site_id for query
      integer_sites <- glue::glue_sql(
        "{sample_sites[sample_sites$abbreviation %in% input$narrowSamplesSite,]$site_id*}"
      )

      this_start <- as.character(as.Date(input$startDate))
      this_end   <- as.character(as.Date(input$endDate))

      # base query
      parameterized_query <- glue::glue_sql("
        SELECT
          sample_id,
          samples.bottle,
          CONCAT(samples.bottle, E'\t', samples.sample_datetime) AS samples
        FROM stormwater.samples
        WHERE
          samples.site_id IN ({ integer_sites}) AND
          samples.sample_datetime BETWEEN { this_start } AND { this_end }
        ORDER BY
          samples.site_id,
          samples.sample_datetime
        ;
        ",
        .con = DBI::ANSI()
      )

      bottle_options <- run_interpolated_query(parameterized_query)

      return(bottle_options)

    })


    # preview list of sample IDs ----------------------------------------------

    output$samplesList <- shiny::renderPrint(

      if (nrow(samplesSelection()) == 0) {

        return(NULL)

      } else {

        writeLines(samplesSelection()$samples)

      }

    )


    # module returns ----------------------------------------------------------

    return(
      list(
        samples     = shiny::reactive({ samplesSelection() }),
        machineFile = shiny::reactive({ input$machineOutputFile }),
        fileName    = shiny::reactive({ input$machineOutputFile$name }),
        filePath    = shiny::reactive({ input$machineOutputFile$datapath }),
        submit      = shiny::reactive({ input$submitData })
      )
    )


    # debugging ---------------------------------------------------------------

    # observe(print({ resultsMetadata() }))
    # observe(print({ samplesSelection() }))
    # observe(print({ input$startDate }))


    }) # close module server
} # close module function
