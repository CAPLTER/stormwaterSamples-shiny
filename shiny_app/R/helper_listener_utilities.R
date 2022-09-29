#' @importFrom shiny reactiveVal getDefaultReactiveDomain

listener_init <- function(name, session = shiny::getDefaultReactiveDomain()) {

  session$userData[[name]] <- shiny::reactiveVal(0)

}


listener_trigger <- function(name, session = shiny::getDefaultReactiveDomain()) {

  session$userData[[name]](session$userData[[name]]() + 1)

}


listener_watch <- function(name, session = shiny::getDefaultReactiveDomain()) {

  session$userData[[name]]()

}
