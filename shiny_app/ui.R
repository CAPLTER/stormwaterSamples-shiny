
# call global R -----------------------------------------------------------

# source("global.R")


# UI ----------------------------------------------------------------------

ui <- tagList(

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

    tabPanel("cations",
      cationsUI("icpCations")
      ), # close cations tab


    # lachat tab --------------------------------------------------------------

    tabPanel("lachat",
      lachatUI("lachat")
      ), # close lachat tab


    # aq2 tab --------------------------------------------------------------

    tabPanel("aq2",
      aq2UI("aq2")
      ), # close aq2 tab


    # shimadzu tab --------------------------------------------------------------

    tabPanel("shimadzu",
      shimadzuUI("shimadzu")
      ), # close shimadzu tab


    # chem data view ----------------------------------------------------------

    tabPanel("chemistry: data viewer",
      ChemViewer1$ui()
      ), # close 'chemistry: data viewer' tab panel


    # chem inventory ----------------------------------------------------------

    tabPanel("chemistry: data inventory",
      ChemInventory1$ui()
    ) # close 'chemistry: inventory viewer' tab panel

    # closing UI --------------------------------------------------------------

  ) # close navbar/page
) # close tagList
