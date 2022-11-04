#' @title Module: ChemInventory
#'
#' @description ChemInventory is an R6 class that facilitates a module for
#' generating a graphical view of the analysis status of samples at a given
#' site and date range. An instance of this class is generated in global then
#' accessed from app.
#'
#' @export

ChemInventory <- R6::R6Class("ChemInventory", list(

    # attributes
    id = NULL,

    # initalizer
    initialize = function(id) {

      self$id <- id

    },

    # UI
    ui = function() {

      ns <- shiny::NS(self$id)

      shiny::tagList(

        shiny::fluidPage(
          shiny::fluidRow(

            shiny::column(
              id = "readme_row", width = 12,
              shiny::div(id = "readme_box",
                shiny::strong("README"),
                "This is a graphical tool to help identify stormwater chemistry data that have/not been uploaded to the database. Tailor the inventory to the site and date range of interest then queue the desired data with the plot/replot button. Use the mouse to select then zoom (double-click) into a portion of the plot. Double-click on empty space anywhere in the plot to exit the zoomed view."
              ) # close readme div
            )   # close readme column
            ),  # close readme row

          shiny::fluidRow(

            shiny::column(
              id = "leftPanel", 2,

              shiny::wellPanel(
                shiny::selectizeInput(
                  inputId  = ns("sample_site"),
                  label    = "site",
                  choices  = siteAbbreviations,
                  selected = NULL,
                  multiple = FALSE
                  ),
                shiny::dateInput(
                  inputId = ns("start_date"),
                  label   = "start",
                  value   = as.character(Sys.Date() - lubridate::years(1)),
                  format  = "yyyy-mm-dd"
                  ),
                shiny::dateInput(
                  inputId = ns("end_date"),
                  label   = "end",
                  value   = as.character(Sys.Date()),
                  format  = "yyyy-mm-dd"
                  ),
                shiny::actionButton(
                  inputId = ns("filter_obs"),
                  label   = "plot/replot",
                  style   = "color: #6495ed; margin-bottom: 2px;",
                  icon    = shiny::icon("circle-play"),
                  width   = "100%"
                )
              )   # close the wellPanel
              ),  # close the left column

            shiny::column(
              id = "rightPanel", 10,

              shiny::plotOutput(
                outputId = ns("sample_chem_plot"),
                dblclick = ns("plot1_dblclick"),
                brush = brushOpts(
                  id         = ns("plot1_brush"),
                  direction  = c("x"),
                  resetOnNew = TRUE
                )
              ) # close plot output

            ) # close the right col

          ) # close the row
        ) # close the page

      ) # close the tagList

    }, # close ui

    # server
    server = function(input, output, session) {

      sample_chem_date <- shiny::eventReactive(input$filter_obs, {

        req(
          input$sample_site,
          input$start_date,
          input$end_date
        )

        #         validate(
        #           need(!is.null(input$sample_site), "select a sample site"),
        #           need(lubridate::is.Date(input$start_date), "start date must be a valid date"),
        #           need(lubridate::is.Date(input$end_date), "end date must be a valid date")
        #         )

        # user-provided site (converted to site_id (integer))
        integer_site <- glue::glue_sql(
          "{sample_sites[sample_sites$abbreviation %in% input$sample_site,]$site_id*}"
        )

        # user-provided start- and end-dates
        start <- as.character(input$start_date)
        end   <- as.character(input$end_date)

        parameterized_query <- glue::glue_sql("
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
          sites.site_id = { integer_site } AND
          (samples.sample_datetime BETWEEN { start } AND { end })
          ;
          ",
          .con = DBI::ANSI()
        )

        chemistry_data <- run_interpolated_query(parameterized_query)

        if (nrow(chemistry_data) == 0) {

          chemistry_data <- NULL

        } else {

          # add dummy variable to indicate sample is identified
          chemistry_data <- chemistry_data |>
          dplyr::mutate(has_sample = "BOTTLE")

          # change datetime (x-axis) to factor to remove gaps between dates
          chemistry_data$dt_as_factor <- factor(chemistry_data$sample_datetime)
          chemistry_data$dt_as_factor <- forcats::fct_reorder(chemistry_data$dt_as_factor, chemistry_data$sample_datetime)

        }

        return(chemistry_data)

      },
      ignoreNULL = FALSE
      ) # close reactive sample_chem_date


      # reactive for brush zoom
      ranges <- shiny::reactiveValues(x = NULL)


      # render sample_chem_plot
      output$sample_chem_plot <- shiny::renderPlot({

        shiny::validate(
          shiny::need(!is.null(sample_chem_date()), "those criteria do not match any data")
        )

        ggplot2::ggplot(
          data    = sample_chem_date(),
          mapping = ggplot2::aes(
            x      = dt_as_factor,
            y      = analysis_name,
            colour = factor(analysis_name)
          )
          ) +
        ggplot2::xlab("date_time") +
        ggplot2::geom_tile(
          color = "white",
          lwd   = 0.5,
          fill  = "#ADD8E6"
          ) +
        ggplot2::geom_text(
          ggplot2::aes(
            x     = dt_as_factor,
            y     = has_sample,
            label = bottle
            ),
          angle  = 285,
          colour = "gray",
          hjust  = 0
          ) +
        ggplot2::theme(
          legend.position  = "none",
          panel.grid.major = ggplot2::element_blank(),
          axis.text.x      = ggplot2::element_text(
            angle = 285,
            vjust = 0,
            hjust = 0
          )
          ) +
        ggplot2::scale_y_discrete(
          limits = c(
            "BOTTLE",
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
            "NO3T_TOC_TN"
            ),
          expand = ggplot2::expansion(mult = c(0.3, 0.1))
          ) +
        ggplot2::coord_cartesian(xlim = ranges$x)

      })


      # brush zoom
      shiny::observeEvent(input$plot1_dblclick, {

        brush <- input$plot1_brush

        if (!is.null(brush)) {

          ranges$x <- c(brush$xmin, brush$xmax)

        } else {

          ranges$x <- NULL

        }

      })

    }, # close server

    # call
    call = function(input, ouput, session) {

      shiny::callModule(self$server, self$id)

    }

) # close public

)  # close R6::ChemInventory
