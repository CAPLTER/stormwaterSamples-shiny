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

#       shiny::fluidRow(
#         shiny::column(
#           width = 12
#           ),
#         shiny::column(
#           width = 2,
#           shiny::actionButton(
#             inputId = ns("add_solid"),
#             label   = "add solid",
#             class   = "btn-success",
#             style   = "color: #fff; margin-bottom: 2px;",
#             icon    = shiny::icon("plus"),
#             width   = "100%"
#           )
#         )
#         ),

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

    # setup

    # added to facilitate renderUIs
    ns <- session$ns


    # query solids data
    solids_inventory_reactive <- reactive({

      # add listener for adding and editing records
      # listener_watch("update_solid")

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
            <button class="btn btn-info btn-sm info_btn" data-toggle="tooltip" data-placement="top" title="Add" id = ', id_, ' style="margin: 0"><i class="fa fa-terminal"></i></button>
          </div>'
        )
        }
      )

      solids_inventory_queried <- cbind(
        solids_inventory_queried,
        tibble::tibble("new" = new),
        tibble::tibble("actions" = actions)
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
      fixedHeader   = FALSE #,
#       columnDefs    = list(
#         list(
#           targets   = c(3),
#           render    = JS("$.fn.dataTable.render.ellipsis( 50 )")
#           ),
#         list(
#           visible   = FALSE,
#           targets   = c(0, 1)
#           ),
#         list(
#           targets   = c(2, 10),
#           className = "dt-center"
#           ),
#         list(
#           targets   = c(5),
#           className = "dt-right"
#           ),
#         list(
#           targets = c(1, 2, 4:7),
#           width   = "100px"
#           ),
#         list(
#           targets   = c(7),
#           orderable = FALSE
#         )
#       )
    )
    ) # close table output


    # module: edit solid

#     this_solid_to_edit <- shiny::eventReactive(input$solid_id_to_edit, {
#     
#       this_solid <- query_single_solid(solid_id = input$solid_id_to_edit)
#     
#       return(this_solid)
#     
#     })
#     
#     module_solid_new(
#       id            = "edit_solid",
#       modal_title   = "add or edit solid",
#       solid_to_edit = this_solid_to_edit,
#       modal_trigger = reactive({input$solid_id_to_edit})
#     )


    # module: add solid

#     module_solid_new(
#       id            = "add_solid",
#       modal_title   = "add solid",
#       solid_to_edit = function() NULL,
#       modal_trigger = reactive({input$add_solid})
#     )


    # delete solid

#     shiny::observeEvent(input$solid_id_to_delete, {
#     
#       tryCatch({
#     
#         delete_solid(solid_to_delete = input$solid_id_to_delete)
#     
#         # listener_trigger("update_solid")
#     
#       }, warning = function(warn) {
#     
#         shiny::showNotification(
#           ui          = paste("warning: ", warn),
#           duration    = NULL,
#           closeButton = TRUE,
#           type        = "warning"
#         )
#     
#       }, error = function(err) {
#     
#         shiny::showNotification(
#           ui          = paste("error: ", err),
#           duration    = NULL,
#           closeButton = TRUE,
#           type        = "error"
#         )
#     
#       }) # close tryCatch
#     
#     }) # close delete solid


    # debugging: module level -------------------------------------------------

    # observe(print({ solids_inventory_reactive() |> dplyr::select(-actions) |> head() }))
    # observe(print({ solids_inventory_reactive() |> dplyr::select(-actions) |> str() }))


  }) # close module server
} # close module function
