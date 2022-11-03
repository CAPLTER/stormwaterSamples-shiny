server <- function(input, output, session) {

  # file upload -------------------------------------------------------------

  # helper function for reading input functions; for reasons that are completely
  # unclear, this function only works if included in app.R (i.e., it is loaded
  # but does not seem to work if loaded from helper_shiny_input.R)
  shinyValue <- function(id, len) {
    unlist(
      lapply(seq_len(len), function(i) {
        value = input[[paste0(id, i)]]
        if (is.null(value)) NA else value
}
      )
    )
  }


  # listeners ------------------------------------------------------------------

  listener_init("update_sample")
  listener_init("update_solid")


  # establish tab position as input to modules ------------------------------

  tabID <- reactive({ input$tabs })


  # modules --------------------------------------------------------------------

  upload_report("upload_report")                  # upload 6700 sample report
  samples_inventory("samples_inventory")          # manage samples data
  solids_inventory("solids_inventory")            # manage solids data
  upload_discharge("discharge")                   # upload 6700 level data
  upload_lachat("upload_lachat", tab = tabID)     # upload lachat data
  upload_aq2("upload_aq2", tab = tabID)           # upload aw2 data
  upload_shimadzu("upload_shimadzu", tab = tabID) # upload aw2 data

  machineInput("alpha")                       # temporary


  # call to cations module --------------------------------------------------

#   callModule(
#     module = cations,
#     id = "icpCations",
#     tab = tabID
#   )


  # call to aq2 module ---------------------------------------------------

#   callModule(module = aq2,
#     id = "aq2",
#     tab = tabID)


  # call to shimadzu module ---------------------------------------------------

#   callModule(module = shimadzu,
#     id = "shimadzu",
#     tab = tabID)


  # call to chem viewer module ----------------------------------------------

  ChemViewer1$call()

  # call to chem inventory module ----------------------------------------------

  ChemInventory1$call()


  # debugging ---------------------------------------------------------------

  # observe(print({ input$tabs }))
  # observe(print({ solidsDataReactive() }))
  # observe(print({ listenModifySolids$dbVersion }))
  # observe(print({ queryType$default }))
  # observe(print({ input$solidsData_cell_edit }))


  # close server ------------------------------------------------------------

}
