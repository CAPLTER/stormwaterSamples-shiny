#' @title module: add new solid
#'
#' @description module_solid_new facilitates adding a new solid record.
#'
#' @note add listener_init("new_solid") in server.R
#'
#' @note add listener_watch("new_solid") in block where data are queried
#' (usually module_solid_main.R)
#'
#' @export

module_solid_new <- function(id, modal_title, solid_to_edit, sample_to_edit, modal_trigger) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # UI -----------------------------------------------------------------------

    shiny::observeEvent(modal_trigger(), {

      hold <- solid_to_edit()

      shiny::showModal(
        shiny::modalDialog(
          shiny::fluidRow(
            shiny::column(
              id = "col_add_solid",
              width = 10,

              shiny::numericInput(
                inputId = ns("new_filter_initial"),
                label   = "filter initial",
                value   = ifelse(is.null(hold), "", as.character(hold$filter_initial)),
                min     = 0,
                max     = NA
                ),
              shiny::numericInput(
                inputId = ns("new_filter_dry"),
                label   = "filter dry",
                value   = ifelse(is.null(hold), "", as.character(hold$filter_dry)),
                min     = 0,
                max     = NA
                ),
              shiny::numericInput(
                inputId = ns("new_volume_filtered"),
                label   = "volume filtered",
                value   = ifelse(is.null(hold), "", as.character(hold$volume_filtered)),
                min     = 0,
                max     = NA
                ),
              shiny::numericInput(
                inputId = ns("new_filter_ashed"),
                label   = "filter_ashed",
                value   = ifelse(is.null(hold), "", as.character(hold$filter_ashed)),
                min     = 0,
                max     = NA
                ),
              shiny::textAreaInput(
                inputId     = ns("new_comments"),
                label       = "comments",
                value       = ifelse(is.null(hold), "", as.character(hold$comments)),
                placeholder = "some settling had occurrred",
                resize      = "vertical"
              )

            ) # close column
            ), # close fluid row
          title = modal_title,
          size = "m",
          footer = list(
            shiny::actionButton(
              inputId = ns("submit"),
              label   = "submit",
              class   = "btn btn-primary",
              style   = "color: white"
              ),
            shiny::modalButton("cancel")
            ), # close footer 
          easyClose = TRUE
        ) # close modal dialog
      ) # close modal
}) # close observeEvent trigger modal


    # validation ---------------------------------------------------------------

    iv <- shinyvalidate::InputValidator$new()

    ## filter_initial
    iv$add_rule("new_filter_initial", function(value) {
      if (!is.na(value) && value < 0) {
        "must be >= 0"
      }
}
    )

    iv$add_rule("new_filter_initial", function(value) {
      if (!is.na(value) && value > 3.0) {
        "must be <= 3"
      }
    }
    )

    ## filter_dry
    iv$add_rule("new_filter_dry", function(value) {
      if (!is.na(value) && value < 0) {
        "must be >= 0"
      }
    }
    )

    iv$add_rule("new_filter_dry", function(value) {
      if (!is.na(value) && value > 3.0) {
        "must be <= 3"
      }
    }
    )

    ## filter_ashed
    iv$add_rule("new_filter_ashed", function(value) {
      if (!is.na(value) && value < 0) {
        "must be >= 0"
      }
    }
    )

    iv$add_rule("new_filter_ashed", function(value) {
      if (!is.na(value) && value > 3.0) {
        "must be <= 3"
      }
    }
    )


    ## volume_filtered
    iv$add_rule("new_volume_filtered", function(value) {
      if (!is.na(value) && value < 0) {
        "must be >= 0"
      }
    }
    )

    iv$add_rule("new_volume_filtered", function(value) {
      if (!is.na(value) && value > 500.0) {
        "must be <= 500"
      }
    }
    )


    # add or update ------------------------------------------------------------

    shiny::observeEvent(input$submit, {

      iv$enable()
      req(iv$is_valid())

      shiny::removeModal()

      if (is.null(solid_to_edit())) {

        this_solid_id   <- NULL
        this_sample_id  <- sample_to_edit()
        this_query_type <- "insert"

      } else {

        this_solid_id   <- solid_to_edit()$id
        this_sample_id  <- NULL
        this_query_type <- "update"

      }

      # call to sql function
      insert_new_or_update_solid(
        filter_initial  = input$new_filter_initial,
        filter_dry      = input$new_filter_dry,
        volume_filtered = input$new_volume_filtered,
        filter_ashed    = input$new_filter_ashed,
        comments        = input$new_comments,
        query_type      = this_query_type,
        sample_id       = this_sample_id,
        solid_id        = this_solid_id
      )

      if (this_query_type == "insert") {

        shiny::updateTextAreaInput(
          inputId     = "new_comments",
          label       = "comments",
          value       = "",
          placeholder = "some settling had occurrred"
        )

      }

      # disable validation
      iv$disable()

      # trigger listener
      listener_trigger("update_solid")

    }) # close observeEvent::submit


    # debugging: module level -------------------------------------------------

    # observe(print(paste0("new_date: ", { input$new_conductivity })))
    # observe(print(paste0("new_time: ", { input$new_time })))
    # observe(print(paste0("new_bottle: ", { input$new_bottle })))
    # observe(print(paste0("new_site: ", { input$new_site })))


}) # close module server
} # close module function
