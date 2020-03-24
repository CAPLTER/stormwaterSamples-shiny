#' @title Add a custom shiny input
#'
#' @description The \code{shinyInput} family of functions allows the user to
#'   create a custom input feature, typically applied to each row of a data
#'   frame or similar object.
#'   \code{shinyInputFlex} is a modified version of shinyInput that
#'   allows for passing any data column name to id instead of literal 'id'
#'   required for shinyInput; this can be useful when the primary key of a
#'   database table is not named id; see app.R#solids for an example of this use.
#'   \code{shinyInputOther} is a modified version of shinyInput that allows for
#'   adding checkboxes, dropdowns, or other intput widgets besides action
#'   buttons. A notable difference from the functionality of \code{shinyInput}
#'   and \code{shinyInputFlex} is that the primary purpose is to add a column to
#'   provide additional input options rather than as a button targeting a
#'   particular table row for a database operation.
#'   \code{shinyValue} Is a helpful function to extract values generated from
#'   \code{shinyInputOther}; however, for reasons that are completely unclear,
#'   this function only works if included in app.R (i.e., it is loaded but does
#'   not seem to work if loaded from helper_shiny_input.R) - included here only
#'   for documentation.
#'
#' @param reactiveObject a reactive (tabular) object to which custom inputs will
#'   be attached
#' @param FUN the input feature to be created (e.g., actionButton)
#' @param len the number of custom inputs to create, typically the number of
#'   rows in the tabular, reactive object
#' @param id the (per shiny) id of the custom input
#'
#' @note The most common (thus far only) use case for the shinyInput function is
#'   to create an actionButton that is attached to each row of a tabular,
#'   reactive data object (e.g., a modify or delete button) that can then be
#'   linked to an observeEvent which then acts on the particular row of the
#'   tabular, reactive object that is selected. As such, additional parameters
#'   (to ...) typically include a label and onclick where the id of the button
#'   is identified in the onclick, and an empty string is passed to the id
#'   argument of the function.

shinyInput <- function(reactiveObject, FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
        ident = as.character(reactiveObject %>% select(id) %>% slice(i:i))
        inputs[i] <- as.character(FUN(paste0(id, ident), ...))
    }
    inputs
}


shinyInputFlex <- function(reactiveObject, FUN, len, id, ...) {
    inputs <- character(len)
    for (i in seq_len(len)) {
        ident = as.character(reactiveObject %>% select(id) %>% slice(i:i))
        inputs[i] <- as.character(FUN(paste0(ident), ...))
    }
    inputs
}


shinyInputOther <- function(FUN, len, id, ...) {
    inputs = character(len)
    for (i in seq_len(len)) {
        inputs[i] = as.character(FUN(paste0(id, i), label = NULL, ...))
    }
    inputs
}


shinyValue <- function(id, len) {
    unlist(lapply(seq_len(len), function(i) {
        value = input[[paste0(id, i)]]
        if (is.null(value)) NA else value
    }))
}
