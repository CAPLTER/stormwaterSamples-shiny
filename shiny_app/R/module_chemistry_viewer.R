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

      ns <- shiny::NS(self$id)

      shiny::fluidPage(
        shiny::fluidRow(

          shiny::column(
            id = "rightPanel", 12,
            DT::dataTableOutput(ns("chemistry_data_view"))
          ) # close the right col

        ) # close the row
      ) # close the page

    }, # close ui

    # server
    server = function(input, output, session) {

      chemistry_data_reactive <- shiny::reactive({

        chemistry_data <- query_chemistry()

        return(chemistry_data)

      })


      # render discharge data for viewing
      output$chemistry_data_view <- DT::renderDataTable({

        chemistry_data_reactive()

      },
      class      = "cell-border stripe",
      filter     = "top",
      extensions = c("FixedHeader", "Buttons"),
      plugins    = c("ellipsis"),
      escape     = FALSE,
      selection  = "none",
      rownames   = FALSE,
      options    = list(
        # bFilter       = 0,
        autoWidth     = FALSE,
        scrollX       = FALSE,
        bLengthChange = FALSE,
        bPaginate     = TRUE,
        bSort         = TRUE,
        autoWidth     = FALSE,
        pageLength    = 50,
        fixedHeader   = FALSE,
        columnDefs    = list(
          list(
            targets   = c(7, 8, 9),
            render    = JS("$.fn.dataTable.render.ellipsis( 25 )")
            ),
          list(
            targets = c(3),
            width   = "50px"
          )
        )
      )
      ) # close table output

    },


    # call
    call = function(input, ouput, session) {

      shiny::callModule(self$server, self$id)

    }

) # close public
) # close R6::ChemViewer

ChemViewer1    <- ChemViewer$new(id = "chem_display")
ChemInventory1 <- ChemInventory$new(id = "chem_inventory")
