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

  tabID <- shiny::reactive({ input$tabs })


  # modules --------------------------------------------------------------------

  upload_report("upload_report")                  # upload 6700 sample report
  samples_inventory("samples_inventory")          # manage samples data
  solids_inventory("solids_inventory")            # manage solids data
  upload_discharge("discharge")                   # upload 6700 level data
  upload_lachat("upload_lachat", tab = tabID)     # upload lachat data
  upload_aq2("upload_aq2", tab = tabID)           # upload aq2 data
  upload_shimadzu("upload_shimadzu", tab = tabID) # upload shimadzu data
  ChemViewer1$call()                              # chem viewer module
  ChemInventory1$call()                           # chem inventory module


  # debugging ---------------------------------------------------------------

  # observe(print({ input$tabs }))
  # observe(print({ solidsDataReactive() }))
  # observe(print({ listenModifySolids$dbVersion }))
  # observe(print({ queryType$default }))
  # observe(print({ input$solidsData_cell_edit }))


  # close server ------------------------------------------------------------

}
