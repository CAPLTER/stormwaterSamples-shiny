#' @title module: add new or edit existing sample
#'
#' @description module_sample_new facilitates adding a new or editing an
#' existing stormwater samples record.
#'
#' @export

module_sample_new <- function(id, modal_title, sample_to_edit, modal_trigger) {

  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # UI -----------------------------------------------------------------------

    shiny::observeEvent(modal_trigger(), {

      hold <- sample_to_edit()

      shiny::modal(
        shiny::modalDialog(
          shiny::fluidRow(
            shiny::column(
              id = "a",
              width = 10,

              shiny::numericInput(
                inputId = ns("temperature"),
                label = "temperature",
                value = ifelse(is.null(hold), "", as.character(hold$temp)),
                min = 0,
                max = NA
              )


            ) # close column
            ), # close fluid row
          title = modal_title,
          size = "m",
          footer = list(
            shiny::actionButton(
              inputId = ns("submit"),
              label = "submit",
              class = "btn btn-primary",
              style = "color: white"
              ),
            shiny::modalButton("cancel")
            ), # close footer 
          easyClose = TRUE
        ) # close modal dialog
      ) # close modal
}) # close observeEvent tigger modal


    # validation ---------------------------------------------------------------



    # add or update ------------------------------------------------------------



}) # close module server
} # close module function
