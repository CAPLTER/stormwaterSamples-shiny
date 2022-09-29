#' @title Module: ChemInventory
#'
#' @description ChemInventory is an R6 class that facilitates a module for
#'  generating a graphical view of the analysis status of samples at a given
#'  site and date range. An instance of this class is generated in global then
#'  accessed from app.

ChemInventory <- R6::R6Class("ChemInventory", list(

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
            id = "leftPanel", 1,
            align = "left",
            p("select parameters then plot/replot to view results",
              style = "text-align: left; background-color: LightGray; color: black;"),
            p("brush and double-click to zoom",
              style = "text-align: left; background-color: LightGray; color: black;"),
            p("double click empty space in the plot to exit zoom",
              style = "text-align: left; background-color: LightGray; color: black;"),
            br(),
            selectizeInput(
              inputId = ns("sample_site"),
              "site",
              choices = siteAbbreviations,
              selected = NULL,
              multiple = FALSE),
            dateInput(
              inputId = ns("start_date"),
              label = "start:",
              value = as.character(Sys.Date() - lubridate::years(2)),
              format = "yyyy-mm-dd"),
            dateInput(
              inputId = ns("end_date"),
              "end:",
              format = "yyyy-mm-dd"),
            actionButton(
              inputId = ns("filterObservations"),
              label = "plot/replot",
              style = "text-align:center; border-sytle:solid; border-color:#0000ff;"),
            br()
            ), # close the left col
          column(
            id = "rightPanel", 11,
            plotOutput(
              outputId = ns("sample_chem_plot"),
              dblclick = ns("plot1_dblclick"),
              brush = brushOpts(
                id = ns("plot1_brush"),
                resetOnNew = TRUE
              )
            ) # close plot output
          ) # close the right col
        ) # close the row
      ) # close the page

    }, # close ui

    # server
    server = function(input, output, session) {

      sample_chem_date <- eventReactive(input$filterObservations, {

        req(
          input$sample_site,
          input$start_date,
          input$end_date
        )

        validate(
          need(!is.null(input$sample_site), "select a sample site"),
          need(is.Date(input$start_date), "start date must be a valid date"),
          need(is.Date(input$end_date), "end date must be a valid date")
        )

        # user-provided site (converted to site_id (integer))
        integer_site <- glue::glue_sql(
          "{sampleSites[sampleSites$abbreviation %in% input$sample_site,]$site_id*}"
        )

        # user-provided start- and end-dates
        start <- as.character(input$start_date)
        end <- as.character(input$end_date)

        baseQuery <- "
        SELECT
          samples.sample_datetime,
          samples.bottle,
          sites.abbreviation,
          results.concentration,
          analysis.analysis_name
        FROM stormwater.samples
        JOIN stormwater.sites ON (sites.site_id = samples.site_id)
        LEFT JOIN stormwater.results ON (results.sample_id = samples.sample_id)
        JOIN stormwater.analysis ON (analysis.analysis_id = results.analysis_id)
        WHERE
          sites.site_id = ?this_site AND
          (samples.sample_datetime BETWEEN ?this_start AND ?this_end)
        ;"

        # parameterized query
        parameterizedQuery <- sqlInterpolate(
          ANSI(),
          baseQuery,
          this_site = integer_site,
          this_start = start,
          this_end = end
        )

        # sample IDs subset from query
        queryResult <- run_interpolated_query(parameterizedQuery)

        # address empty sample set
        if (nrow(queryResult) == 0) {

          queryResult <- NULL

        } else {

        # add dummy variable to indicate sample is identified
        queryResult <- queryResult %>%
          mutate(has_sample = "_sample_")

        }

        # return result
        return(queryResult)

      })


      # single zoomable plot (on left)
      ranges <- reactiveValues(x = NULL)

      output$sample_chem_plot <- renderPlot({

        validate(
          need(!is.null(sample_chem_date()), "those criteria do not match any data")
        )

        if (!is.null(ranges$x)) {
          ranges$x <- as.POSIXct(ranges$x, , origin = "1970-01-01")
        }

        ggplot(
          data = sample_chem_date(),
          mapping = aes(
            x = sample_datetime,
            y = analysis_name,
            colour = analysis_name)
          ) +
        geom_point() +
        geom_text(
          aes(
            x = sample_datetime,
            y = has_sample,
            label = bottle),
          angle = 285,
          colour = "gray",
          hjust = 0) +
        theme(
          legend.position = "none",
          axis.text.x = element_text(angle = 285, vjust = 0, hjust = 0)
          ) +
        scale_y_discrete(
          limits = c(
            "_sample_",
            "CaD_ICP",
            "NaD_ICP",
            "ZnD_ICP",
            "NO3D_LACHAT",
            "NH4_LACHAT",
            "ClD_LACHAT",
            "PO4D_LACHAT",
            "NO2D_LACHAT",
            "PO4T_AQ2",
            "NO3T_AQ2",
            "DOC_TOC",
            "NO3T_TOC_TN"),
          expand = expansion(mult = c(0.2, 0.1))
          ) +
        # scale_x_datetime(breaks = "1 day", date_minor_breaks = "1 hour") +
        coord_cartesian(xlim = ranges$x)

      })

      # check if there iss a brush on the plot when a double-click happens; if
      # so, zoom to the brush bounds; if not, reset the zoom; only brushing the
      # x-axis in this instance
      observeEvent(input$plot1_dblclick, {
        brush <- input$plot1_brush
        if (!is.null(brush)) {
          ranges$x <- c(brush$xmin, brush$xmax)
          # ranges$y <- c(brush$ymin, brush$ymax)

        } else {
          ranges$x <- NULL
          # ranges$y <- NULL
        }
      })


    }, # close server

    # call
    call = function(input, ouput, session) {

      callModule(self$server, self$id)

    }

) # close public

)  # close R6::ChemInventory
