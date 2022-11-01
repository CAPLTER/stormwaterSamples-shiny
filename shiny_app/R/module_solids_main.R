#' @title module: stormwater solids inventory
#'
#' @description The solids_main module facilitates viewing, searching, and
#' calling a dialogue box to edit stormwater solid records.
#'
#' @note 
#'
#' @export
#'
# solids_inventoryUI -----------------------------------------------------------

solids_inventoryUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(

      shiny::fluidRow(
        shiny::column(
          id = "readme_row", width = 12,
          shiny::div(id = "readme_box",
            shiny::strong("README"), "The table features stormwater sample data from present to three years prior. Navigate to storms and samples using the Search tool and/or column filters. From the table, solids data can be added, edited, or deleted. Note that a sample must exist in the database to attach solids data. Add solids data for a sample using the ", shiny::icon("plus"), "button. Edit or delete existing solid data records with the ", shiny::icon("pen-to-square"), "or", shiny::icon("trash-can"), "buttons, respectively. Solids data exist as records in the database. As such, new records must be added, and only existing records can be edited or deleted. Samples that have corresponding solids data will feature a number in the `id` column - these records can only be edited or deleted. The `id` column will be blank for samples that do not yet have solids data - only new solids data can be added to these records."
          ) # close readme div
        ) # close readme column
        ), # close readme row

      shiny::fluidRow(
        id = "row_solids_data",
        shiny::column(
          id    = "solids",
          width = 12,
          DT::DTOutput(ns("solids_inventory_view"))
        )  # close column solids_main_panel
        ), # close row row_solids_data

      tags$script(src = "solids_inventory_module.js"),
      tags$script(paste0("solids_inventory_module_js('", ns(''), "')"))

    ) # close the page
  ) # close tagList

} # close solids_mainUI


# solids_inventory ------------------------------------------------------------

solids_inventory <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # added to facilitate renderUIs
    ns <- session$ns


    # query solids data
    solids_inventory_reactive <- reactive({

      # add listener for adding and editing records
      listener_watch("update_solid")

      solids_inventory_queried <- query_solids()

      actions <- purrr::map_chr(solids_inventory_queried$id, function(id_) {
        paste0(
          '<div class="btn-group" style="width: 120px;" role="group" aria-label="Basic example">
            <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
            <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin-left: 5px;"><i class="fa fa-trash-o"></i></button>
          </div>'
        )
        }
      )

      new <- purrr::map_chr(solids_inventory_queried$sample_id, function(id_) {
        paste0(
          '<div class="btn-group" style="width: 50px;" role="group" aria-label="Basic example">
            <button class="btn btn-success btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="Add" id = ', id_, ' style="margin: 0"><i class="fa fa-plus"></i></button>
          </div>'
        )
      }
      )

      solids_inventory_queried <- cbind(
        solids_inventory_queried,
        tibble::tibble("add_new" = new),
        tibble::tibble("edit_delete" = actions)
      )

      return(solids_inventory_queried)

    })


    # render table of data
    output$solids_inventory_view <- DT::renderDT({

      solids_inventory_reactive()

    },
    # class      = "cell-border stripe",
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
          targets   = c(9),
          render    = JS("$.fn.dataTable.render.ellipsis( 50 )")
          ),
        list(
          visible   = FALSE,
          targets   = c(1) # sample_id column
          ),
        list(
          targets   = c(0, 2, 4, 10, 11),
          className = "dt-center"
          ),
        list(
          targets = c(5:8),
          className = "dt-right"
          ),
        list(
          targets = c(0, 2, 5:8, 10),
          width   = "80px"
          ),
        list(
          targets   = c(10, 11),
          orderable = FALSE
        )
      )
    )
    ) # close table output


    # module: edit solid

    this_solid_to_edit <- shiny::eventReactive(input$solid_id_to_edit, {

      this_solid <- query_solid_solid_id(solid_id = input$solid_id_to_edit)

      return(this_solid)

    })

    module_solid_new(
      id             = "edit_solid",
      modal_title    = "edit solid",
      solid_to_edit  = this_solid_to_edit,
      sample_to_edit = function() NULL,
      modal_trigger  = shiny::reactive({input$solid_id_to_edit})
    )


    # module: add solid

    module_solid_new(
      id             = "add_solid",
      modal_title    = "add solid",
      solid_to_edit  = function() NULL,
      sample_to_edit = shiny::reactive({input$sample_id_to_populate}),
      modal_trigger  = shiny::reactive({input$sample_id_to_populate})
    )


    # delete solid

    shiny::observeEvent(input$solid_id_to_delete, {

      if (grepl("na", input$solid_id_to_delete, ignore.case = TRUE)) {

        shiny::showNotification(
          ui          = "solid record does not exist",
          duration    = 5,
          closeButton = TRUE,
          type        = "warning"
        )

      } else {

        tryCatch({

          delete_solid(solid_to_delete = input$solid_id_to_delete)

          listener_trigger("update_solid")

        }, warning = function(warn) {

          shiny::showNotification(
            ui          = paste("warning: ", warn),
            duration    = NULL,
            closeButton = TRUE,
            type        = "warning"
          )

        }, error = function(err) {

          shiny::showNotification(
            ui          = paste("error: ", err),
            duration    = NULL,
            closeButton = TRUE,
            type        = "error"
          )

        }) # close tryCatch

      }

    })


    # debugging: module level -------------------------------------------------

    # observe(print({ solids_inventory_reactive() |> dplyr::select(-actions) |> head() }))
    # observe(print({ input$sample_id_to_populate }))
    # observe(print({ input$solid_id_to_edit }))


  }) # close module server
} # close module function
