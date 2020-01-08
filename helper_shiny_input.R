#' @title Add a custom shiny input
#'
#' @description The shinyInput function allows the user to create a custom input
#'   feature, typically applied to each row of a data frame or similar object.
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
#'   
#'   
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