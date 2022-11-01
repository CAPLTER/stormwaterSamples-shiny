
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
      upload_lachatUI("upload_lachat")
      ), # close lachat tab


    # aq2 tab --------------------------------------------------------------

    tabPanel("aq2",
      upload_aq2UI("upload_aq2")
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

    ,
    tabPanel("machine_input",
      machineInputUI("alpha")
    ) # close 'chemistry: inventory viewer' tab panel

  ) # close navbar/page
) # close tagList
