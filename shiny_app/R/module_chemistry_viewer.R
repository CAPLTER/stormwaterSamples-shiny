#' @title Module: ChemViewer
#'
#' @description ChemViewer is an R6 class that facilitates a module for viewing
#'   machine-derived stormwater chemistry data. An instance of this class is
#'   generated in global then accessed from app.

ChemViewer <- R6::R6Class("ChemViewer", list(

    # attributes
    id = NULL,

    # initalizer
    initialize = function(id) {

      self$id <- id

    },

    # UI
    ui = function() {

      ns <- NS(self$id)

      fluidPage(
        fluidRow(
          column(
            id = "leftPanel", 2,
            p("filter results",
              style = "text-align: left; background-color: LightGray; color: black;"),
            br(),
            selectizeInput(inputId = ns("analysis"),
              "analysis",
              choices = analysesNames,
              selected = NULL,
              multiple = TRUE),
            selectizeInput(inputId = ns("samplesSite"),
              "site",
              choices = siteAbbreviations,
              selected = NULL,
              multiple = TRUE),
            dateInput(ns("startDate"),
              "start:",
              format = "yyyy-mm-dd"),
            dateInput(ns("endDate"),
              "end:",
              format = "yyyy-mm-dd"),
            actionButton(inputId = ns("filterObservations"),
              label = "filter",
              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
            actionButton(inputId = ns("clearFilterObservations"),
              label = "clear filter",
              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
            br()
            ), # close the left col
          column(
            id = "rightPanel", 10,
            DT::dataTableOutput(ns("dataOutput"))
          ) # close the right col
        ) # close the row
      ) # close the page

    }, # close ui

    # server
    server = function(input, output, session) {

      # queryType: default vs parameterized query for observations
      queryType <- reactiveValues(default = "default")

      # actionButton filterObservations = parameterized query type
      observeEvent(input$filterObservations, {

        queryType$default <- "param"

      })

      # actionButton clearFilterObservations = default query type
      observeEvent(input$clearFilterObservations, {

        queryType$default <- "default"

        updateSelectizeInput(
          session,
          inputId = "analysis",
          label = "analysis",
          choices = analysesNames,
          selected = NULL
        )
        updateSelectizeInput(
          session,
          inputId = "samplesSite",
          label = "site",
          choices = siteAbbreviations,
          selected = NULL
        )
        updateDateInput(
          session,
          inputId = "startDate",
          "start:",
          value = Sys.Date()
        )
        updateDateInput(
          session,
          inputId = "endDate",
          "end:",
          value = Sys.Date()
        )

      })

      chemData <- reactive({

        if (queryType$default == "default") {

          # default params

          integerSites <- glue::glue_sql("{sampleSites$site_id*}")
          integerAnalyses <- glue::glue_sql("{analyses$analysis_id*}")
          start <- as.character(Sys.Date() - lubridate::years(3))
          end <- as.character(Sys.Date())

        } else {

          # user-provided params

          # convert site abbreviations to site_id for query

          if (is.null(input$samplesSite)) {

            integerSites <- glue::glue_sql("{sampleSites$site_id*}")

          } else {

            integerSites <- glue::glue_sql(
              "{sampleSites[sampleSites$abbreviation %in% input$samplesSite,]$site_id*}"
            )

          }

          # convert analysis name(s) to analysis_id for query

          if (is.null(input$analysis))  {

            integerAnalyses <- glue::glue_sql("{analyses$analysis_id*}")

          } else {

            integerAnalyses <- glue::glue_sql(
              "{analyses[analyses$analysis_name %in% input$analysis,]$analysis_id*}"
            )

          }

          # user-provided start- and end-dates
          start <- as.character(input$startDate)
          end <- as.character(input$endDate)

        }

        baseQuery <- "
        SELECT
          sites.abbreviation AS site,
          samples.sample_datetime::TEXT,
          samples.bottle,
          results.replicate,
          analysis.analysis_name,
          results.date_analyzed::TEXT,
          results.concentration,
          samples.comments AS samples_comments,
          results.comments AS results_comments,
          CASE
            WHEN icp_file.source_file IS NOT NULL THEN icp_file.source_file
            WHEN lachat_file.source_file IS NOT NULL THEN lachat_file.source_file
            WHEN aq2_file.source_file IS NOT NULL THEN aq2_file.source_file
            WHEN shimadzu_file.source_file IS NOT NULL THEN shimadzu_file.source_file
            ELSE NULL
          END AS source_file
        FROM stormwater.results
        JOIN stormwater.samples ON (samples.sample_id = results.sample_id)
        JOIN stormwater.sites ON (samples.site_id = sites.site_id)
        JOIN stormwater.analysis ON (results.analysis_id = analysis.analysis_id)
        LEFT JOIN (
          SELECT
            run_id,
            source_file
          FROM stormwater.icp
          WHERE
            source_file IS NOT NULL
          GROUP BY
            run_id,
            source_file
          ) AS icp_file ON (icp_file.run_id = results.run_id)
        LEFT JOIN (
          SELECT
            run_id,
            source_file
          FROM stormwater.lachat
          WHERE
            source_file IS NOT NULL
          GROUP BY
            run_id,
            source_file
          ) AS lachat_file ON (lachat_file.run_id = results.run_id)
        LEFT JOIN (
          SELECT
            run_id,
            source_file
          FROM stormwater.aq2
          WHERE
            source_file IS NOT NULL
          GROUP BY
            run_id,
            source_file
          ) AS aq2_file ON (aq2_file.run_id = results.run_id)
        LEFT JOIN (
          SELECT
            run_id,
            source_file
          FROM stormwater.shimadzu
          WHERE
            source_file IS NOT NULL
          GROUP BY
            run_id,
            source_file
          ) AS shimadzu_file ON (shimadzu_file.run_id = results.run_id)
        WHERE
          samples.sample_datetime BETWEEN ?thisStart AND ?thisEnd AND
          samples.site_id IN (?theseSites) AND
          results.analysis_id IN (?theseAnalyses)
        ORDER BY
          results.run_id DESC,
          results.result_id ASC;"

        # parameterized query
        parameterizedQuery <- sqlInterpolate(
          ANSI(),
          baseQuery,
          theseSites = integerSites,
          theseAnalyses = integerAnalyses,
          thisStart = start,
          thisEnd = end
        )

        # sample IDs subset from query
        queryResult <- run_interpolated_query(parameterizedQuery)


        # return result
        return(queryResult)

      })


      # render discharge data for viewing
      output$dataOutput <- DT::renderDataTable({

        # return empty frame and message if empty set
        if (nrow(chemData()) == 0) {

          results <- tibble(sample_type = NA)

          showNotification(
            ui = "there are not any data that match those criteria",
            duration = 5,
            closeButton = TRUE,
            type = "warning")

        } else {

          results <- chemData()

        }

        return(results)

      },
      selection = "none",
      escape = FALSE,
      server = TRUE,
      options = list(paging = TRUE,
        pageLength = 25,
        ordering = TRUE,
        searching = TRUE),
      rownames = F) # close output$resinDataOutput

    },

    # call
    call = function(input, ouput, session) {

      callModule(self$server, self$id)

    }

) # close public

)  # close R6::ChemViewer

ChemViewer1    <- ChemViewer$new(id = "chem_display")
ChemInventory1 <- ChemInventory$new(id = "chem_inventory")

