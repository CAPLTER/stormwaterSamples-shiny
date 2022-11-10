# UI ----------------------------------------------------------------------

ui <- shiny::tagList(

  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

  shiny::navbarPage(
    title = "stormwater",
    id    = "tabs", # use explicit id to access tab position

    # isco tab -----------------------------------------------------------------

    shiny::tabPanel(
      "isco",
      upload_reportUI("upload_report")
      ),


    # samples tab --------------------------------------------------------------

    shiny::tabPanel(
      "samples",
      samples_inventoryUI("samples_inventory")
      ),


    # solids -------------------------------------------------------------------

    shiny::tabPanel(
      "solids",
      solids_inventoryUI("solids_inventory")
      ),


    # discharge ----------------------------------------------------------------

    shiny::tabPanel(
      "discharge",
      upload_dischargeUI("discharge")
      ),


    # cations tab -------------------------------------------------------------

    shiny::tabPanel("cations",
      upload_cationsUI("upload_cations")
      ),


    # lachat tab --------------------------------------------------------------

    shiny::tabPanel("lachat",
      upload_lachatUI("upload_lachat")
      ),


    # aq2 tab --------------------------------------------------------------

    shiny::tabPanel("aq2",
      upload_aq2UI("upload_aq2")
      ),


    # shimadzu tab --------------------------------------------------------------

    shiny::tabPanel("shimadzu",
      upload_shimadzuUI("upload_shimadzu")
      ),


    # chem data view ----------------------------------------------------------

    shiny::tabPanel("chemistry: data viewer",
      ChemViewer1$ui()
      ),


    # chem inventory ----------------------------------------------------------

    shiny::tabPanel("chemistry: data inventory",
      ChemInventory1$ui()
    )

  ) # close navbar/page
)   # close tagList
