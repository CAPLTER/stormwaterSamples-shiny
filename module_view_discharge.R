#' @title Module: viewDischarge
#'
#' @description The module viewDischarge facilitates viewing stormwater
#'   discharge data.

# view discharge UI -------------------------------------------------------

viewDischargeUI <- function(id) {

  ns <- NS(id)

  tagList(
    p(HTML('&emsp;'), "warning: load times for filtered data can be very long",
      style = "text-align: left; background-color: #89cff0; color: black;"),
    tags$head(
      tags$style(
        HTML("#samplesLeftPanel { background: #D3D3D3; color: #484848; }")
      ) # close tags$style
    ), # close tagss$head
    fluidPage(
      fluidRow(
        column(id = 'leftPanel', 2,
               # filter existing
               strong("filter data",
                      style = "text-align: center; color: black"),
               selectizeInput(inputId = ns("viewDischargeSite"),
                              "site",
                              choices = siteAbbreviations,
                              selected = NULL,
                              multiple = FALSE),
               br(),
               dateInput(inputId = ns("viewDischargeStartDate"),
                         "start:",
                         format = "yyyy-mm-dd"),
               dateInput(inputId = ns("viewDischargeEndDate"),
                         "end:",
                         format = "yyyy-mm-dd"),
               actionButton(inputId = ns("filterDischarge"),
                            label = "view data",
                            style = "text-align:center; border-sytle:solid; border-color:#0000ff;")
        ), # close the left col
        column(id = "rightPanel", 10,
               DT::DTOutput(ns("dischargeView"))
        ) # close the right col
      ) # close the row
    ) # close the page
  ) # close tagList

} # close viewDischargeUI


# modify reach patches main -----------------------------------------------

viewDischarge <- function(input, output, session) {

  # queryType: default vs parameterized query for transects
  queryType <- reactiveValues(default = "default")

  # actionButton filterDischarge = parameterized query type
  observeEvent(input$filterDischarge, {

    queryType$default <- "param"

  })

  # query samples data
  dischargeDataReactive <- reactive({

    # add listener for adding and deleting records
    # listenviewDischarge$dbVersion

    if (queryType$default == "default") {

      dischargeData <- query_discharge_default()

    } else {

      # parameters cannot be passed to function directly
      filterStart <- as.character(input$viewDischargeStartDate)
      filterEnd <- as.character(input$viewDischargeEndDate)
      filterSite <- input$viewDischargeSite

      # run query with params
      dischargeData <- query_discharge_site_date(start = filterStart,
                                                 end = filterEnd,
                                                 site = filterSite)

    }

    if (nrow(dischargeData) == 0) {

      dischargeData <- data.frame(
        discharge_id = NA,
        site_id = NA,
        site = as.character("match not found"),
        event_datetime = NA,
        water_height = NA,
        discharge = NA,
        discharge_corrected = NA
      )

    }

    return(dischargeData)

  })

  # render editable table of samples data
  output$dischargeView <- DT::renderDT({

    dischargeDataReactive()

  },
  escape = FALSE,
  selection = "none",
  rownames = FALSE,
  options = list(bFilter = 0,
                 bLengthChange = FALSE,
                 bPaginate = FALSE,
                 bSort = FALSE,
                 autoWidth = TRUE,
                 columnDefs = list(list(width = '100px', targets = c(1)))
  )
  ) # close output$dischargeView


  # debugging: module level -------------------------------------------------

  ############# START debugging
  # observe(print({ queryType }))
  # observe(print({ queryType$default }))
  # observe(print({ input$ReachPatchs_cell_edit }))
  ############# END debugging


  # close module viewDischarge ----------------------------------------------

} # close module viewDischarge
