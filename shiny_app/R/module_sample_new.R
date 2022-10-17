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

      shiny::showModal(
        shiny::modalDialog(
          shiny::fluidRow(
            shiny::column(
              id = "col_add_sample",
              width = 10,

              shiny::selectInput(
                inputId   = ns("new_site"),
                label     = "site",
                choices   = sample_sites[["abbreviation"]],
                selected  = ifelse(is.null(hold), "", hold$site),
                multiple  = FALSE,
                selectize = FALSE
                ),
              shiny::dateInput(
                inputId = ns("new_date"),
                label   = "date",
                value   = ifelse(
                  test = is.null(hold),
                  yes  = as.character(Sys.Date()),
                  no   = as.character(as.Date(hold$date_time))
                  ),
                format  = "yyyy-mm-dd",
                min     = "2020-01-01",
                max     = Sys.Date()
                ),
              shinyTime::timeInput(
                inputId = ns("new_time"),
                label   = "time",
                value   = strptime(
                  x = ifelse(
                    test = is.null(hold),
                    yes  = "00:00:00",
                    no   = format(strptime(hold$date_time, format = "%Y-%m-%d %H:%M:%S"), "%H:%M:%S")
                    ),
                  format = "%T"
                  ),
                seconds = TRUE
                ),
              shiny::selectInput(
                inputId   = ns("new_bottle"),
                label     = "bottle",
                choices   = bottleList,
                selected  = ifelse(is.null(hold), "", hold$bottle),
                multiple  = FALSE,
                selectize = FALSE
                ),
              shiny::numericInput(
                inputId = ns("new_temperature"),
                label   = "temperature",
                value   = ifelse(is.null(hold), "", as.character(hold$temp)),
                min     = 0,
                max     = NA
                ),
              shiny::numericInput(
                inputId = ns("new_conductivity"),
                label   = "conductivity",
                value   = ifelse(is.null(hold), "", as.character(hold$cond)),
                min     = 0,
                max     = NA
                ),
              shiny::textAreaInput(
                inputId     = ns("new_comments"),
                label       = "comments",
                value       = ifelse(is.null(hold), "", as.character(hold$comments)),
                placeholder = "bottles only partially filled",
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

    iv$add_rule("new_site", shinyvalidate::sv_required())
    iv$add_rule("new_date", shinyvalidate::sv_required())
    iv$add_rule("new_time", shinyvalidate::sv_required())
    iv$add_rule("new_bottle", shinyvalidate::sv_required())

    iv$add_rule("new_date", function(value) {
      if (as.Date(value) > Sys.Date()) {
        "time travel not allowed"
      }
    }
    )

    iv$add_rule("new_temperature", function(value) {
      if (!is.na(value) && value < 0) {
        "must be >= 0"
      }
    }
    )
    iv$add_rule("new_temperature", function(value) {
      if (!is.na(value) && value > 50) {
        "must be <= 50"
      }
    }
    )

    iv$add_rule("new_conductivity", function(value) {
      if (!is.na(value) && value < 0) {
        "must be >= 0"
      }
    }
    )
    iv$add_rule("new_conductivity", function(value) {
      if (!is.na(value) && value > 2) {
        "must be <= 2"
      }
    }
    )


    # update bottle list per site ----------------------------------------------

    shiny::observeEvent(input$new_site, {

      hold <- sample_to_edit()

      bottles_subset <- bottleList[
        grepl(
          pattern     = paste0("^", sample_sites[sample_sites$abbreviation == input$new_site,]$site_id),
          x           = bottleList[["bottles"]],
          ignore.case = TRUE
          ),
        ]

      shiny::updateSelectInput(
        inputId  = "new_bottle",
        choices  = bottles_subset,
        selected = ifelse(is.null(hold), "", hold$bottle)
      )

    })


    # add or update ------------------------------------------------------------

    shiny::observeEvent(input$submit, {

      iv$enable()
      req(iv$is_valid())

      shiny::removeModal()

      if (is.null(sample_to_edit())) {

        this_sample_id   <- NULL
        this_sample_type <- "insert"

      } else {

        this_sample_id   <- sample_to_edit()$id
        this_sample_type <- "update"

      }

      insert_new_or_update_sample(
        site        = input$new_site,
        date        = input$new_date,
        time        = input$new_time,
        comments    = input$new_comments,
        temperature = input$new_temperature,
        conductance = input$new_conductivity,
        bottle      = input$new_bottle,
        survey_type = this_sample_type,
        sample_id   = this_sample_id
      )

      if (this_sample_type == "insert") {

        shiny::updateTextAreaInput(
          inputId     = "new_comments",
          label       = "comments",
          value       = "",
          placeholder = "bottles only partially filled"
        )

      }

      # disable validation
      iv$disable()

      # trigger listener
      listener_trigger("update_sample")

    }) # close observeEvent::submit


    # debugging: module level -------------------------------------------------

    # observe(print(paste0("new_date: ", { input$new_conductivity })))
    # observe(print(paste0("new_time: ", { input$new_time })))
    # observe(print(paste0("new_bottle: ", { input$new_bottle })))
    # observe(print(paste0("new_site: ", { input$new_site })))


}) # close module server
} # close module function
