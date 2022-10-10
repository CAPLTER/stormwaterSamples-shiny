#' @title module: stormwater samples inventory
#'
#' @description The samples_main module facilitates viewing, searching, and
#' calling a dialogue box to edit stormwater sample records.
#'
#' @note 
#'
#' @export
#'
# samples_inventoryUI ----------------------------------------------------------

samples_inventoryUI <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(

    shiny::fluidPage(

      shiny::fluidRow(
        shiny::column(
          width = 12
          ),
        shiny::column(
          width = 2,
          shiny::actionButton(
            inputId = ns("add_sample"),
            label   = "add sample",
            class   = "btn-success",
            style   = "color: #fff; margin-bottom: 2px;",
            icon    = shiny::icon("plus"),
            width   = "100%"
          )
        )
        ),

      shiny::fluidRow(
        id = "row_samples_data",
        shiny::column(
          id    = "samples",
          width = 12,
          DT::DTOutput(ns("samples_inventory_view"))
        )  # close column samples_main_panel
        ), # close row    row_samples_data

      tags$script(src = "samples_inventory_module.js"),
      tags$script(paste0("samples_inventory_module_js('", ns(''), "')"))

    ) # close the page
  ) # close tagList

} # close samples_mainUI


# samples_inventory ------------------------------------------------------------

samples_inventory <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    # setup

    # added to facilitate renderUIs
    ns <- session$ns


    # query samples data
    samples_inventory_reactive <- reactive({

      samples_inventory_queried <- query_samples()

      actions <- purrr::map_chr(samples_inventory_queried$id, function(id_) {
        paste0(
          '<div class="btn-group" style="width: 120px;" role="group" aria-label="Basic example">
          <button class="btn btn-primary btn-sm edit_btn" data-toggle="tooltip" data-placement="top" title="Edit" id = ', id_, ' style="margin: 0"><i class="fa fa-pencil-square-o"></i></button>
          <button class="btn btn-danger btn-sm delete_btn" data-toggle="tooltip" data-placement="top" title="Delete" id = ', id_, ' style="margin-left: 5px;"><i class="fa fa-trash-o"></i></button>
          </div>'
        )
        }
      )

      samples_inventory_queried <- cbind(
        samples_inventory_queried,
        tibble::tibble("actions" = actions)
      )

      return(samples_inventory_queried)

    })


    # render table of data
    output$samples_inventory_view <- DT::renderDT({

      samples_inventory_reactive()

    },
    # class      = "cell-border stripe",
    filter     = "top",
    extensions = c("FixedHeader", "Buttons"),
    plugins    = c("ellipsis"),
    escape     = FALSE,
    selection  = "none",
    rownames   = FALSE,
    options    = list(
      scrollX       = TRUE,
      # bFilter       = 0,
      bLengthChange = FALSE,
      bPaginate     = TRUE,
      bSort         = TRUE,
      autoWidth     = FALSE,
      pageLength = 50,
      fixedHeader = FALSE,
      columnDefs    = list(
        list(
          targets = c(3),
          render  = JS("$.fn.dataTable.render.ellipsis( 12 )")
          ),
        list(
          visible = FALSE,
          targets = 0
          ),
        list(
          targets   = c(1, 2, 8),
          className = "dt-center"
          ),
        list(
          targets   = c(7),
          className = "dt-right"
          ),
        list(
          targets   = c(8),
          orderable = FALSE
        )
      )
    )
    ) # close table output


    # module: edit sample

    #   this_sample_to_edit <- shiny::eventReactive(input$sample_id_to_edit, {
    # 
    #     this_sample <- query_single_sample(sample_id = input$sample_id_to_edit)
    # 
    #     return(this_sample)
    # 
    #   })
    # 
    #   module_sample_new(
    #     id            = "edit_sample",
    #     modal_title   = "edit sample",
    #     sample_to_edit  = this_sample_to_edit,
    #     survey_id     = survey_id,
    #     modal_trigger = reactive({input$sample_id_to_edit})
    #   )


    # module: add sample

    #   module_sample_new(
    #     id            = "add_sample",
    #     modal_title   = "add sample",
    #     bird_to_edit  = function() NULL,
    #     survey_id     = survey_id,
    #     modal_trigger = reactive({input$add_sample})
    #   )


    # delete sample

    #   shiny::observeEvent(input$sample_id_to_delete, {
    # 
    #     tryCatch({
    # 
    #       delete_sample(sample_to_delete = input$sample_id_to_delete)
    # 
    #       listener_trigger("update_birds")
    # 
    #     }, warning = function(warn) {
    # 
    #       shiny::showNotification(
    #         ui          = paste("warning: ", warn),
    #         duration    = NULL,
    #         closeButton = TRUE,
    #         type        = "warning"
    #       )
    # 
    #     }, error = function(err) {
    # 
    #       shiny::showNotification(
    #         ui          = paste("error: ", err),
    #         duration    = NULL,
    #         closeButton = TRUE,
    #         type        = "error"
    #       )
    # 
    #     }) # close tryCatch
    # 
    #   }) # close delete sample


    # debugging: module level -------------------------------------------------

    # observe(print({ birds_inventory_reactive() |> dplyr::select(-actions) }))


  }) # close module sever
} # close module function
