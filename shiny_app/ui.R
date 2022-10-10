
# README ------------------------------------------------------------------

# Note regarding Javascript: There are several code chunks that have the comment
# 'notable stmt' associated with them. These statements are related to the
# Javascript functionality required for the interactivity of a file upload,
# i.e., adding temp, cond, etc. to the table following a file uplooad. See
# https://stackoverflow.com/questions/40020600/r-shiny-selectedinput-inside-renderdatatable-cells
# and
# https://groups.google.com/forum/#!msg/shiny-discuss/ZUMBGGl1sss/zfcG9c6MBAAJ
# for details.


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

  # isco tab ----------------------------------------------------------------

  shiny::tabPanel(
    "isco",
    upload_reportUI("upload_report")
  ),


  # samples tab -------------------------------------------------------------

  shiny::tabPanel(
    "samples",
    samples_inventoryUI("samples_inventory")
  ),

#   tabPanel("samples",
#     modifySamplesUI("modifySamples")
#     ), # close samples tab

  # discharge upload tab ----------------------------------------------------

  tabPanel("discharge: upload",
    fluidPage(
      fluidRow(
        column(
          id = "leftPanel", 2,
          fileInput("dischargeFile", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",
              "text/comma-separated-values,text/plain",
              ".csv")),
          actionButton("submitDischargeFileUpload",
            "submit data"),
          br(),
          br()
          ), # close the left col
        column(
          id = "dischargeRightPanel", 10,
          strong("Data Preview"),
          hr(),
          tableOutput("levelDataPreview")
        ) # close the right col
      ) # close the row
    ) # close the page
    ), # close discharge: file upload

  # discharge viewing tab ---------------------------------------------------

  tabPanel("discharge",
    viewDischargeUI("viewDischarge")
    ), # close discharge tab

  # solids tab --------------------------------------------------------------

  tabPanel("solids",
    fluidPage(
      fluidRow(
        column(
          id = "leftPanel", 2,
          # filter existing
          strong("filter samples",
            style = "text-align: center; color: black"),
          selectizeInput(inputId = "viewSamplesSolidsSite",
            "site",
            choices = siteAbbreviations,
            selected = NULL,
            multiple = FALSE),
          dateInput(inputId = "viewSamplesSolidsStartDate",
            "start:",
            format = "yyyy-mm-dd"),
          dateInput(inputId = "viewSamplesSolidsEndDate",
            "end:",
            format = "yyyy-mm-dd"),
          actionButton(inputId = "filterSamplesSolids",
            label = "view samples",
            style = "text-align:center; border-sytle:solid; border-color:#0000ff;")
          ), # close the left col
        column(
          id = "rightPanel", 10,
          DT::DTOutput("samplesSolidsDataView"),
          DT::DTOutput("solidsData"),
          uiOutput("addNewSolidUI"),
          div(id = "modifySolidsDiv")
        ) # close the right col
      ) # close the row
    ) # close the page
    ), # close AFDM tab panel

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
