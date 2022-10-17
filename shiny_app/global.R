# libraries ---------------------------------------------------------------

library(shiny)
library(DBI)
library(RPostgreSQL)
library(pool)
library(DT)


# options -----------------------------------------------------------------

options(shiny.maxRequestSize = 30*1024^2) # increase max file upload size
options(shiny.reactlog = FALSE)


# configuration ----------------------------------------------------------------

# configuration from config.yml
this_configuration <- config::get(config = "default")

# database connection
this_pool <- dbPool(
  drv      = RPostgreSQL::PostgreSQL(),
  dbname   = this_configuration$dbname,
  host     = this_configuration$host,
  user     = this_configuration$user,
  password = this_configuration$password
)

onStop(function() {
  poolClose(this_pool)
})


# modules and functions --------------------------------------------------------

source("R/helper_sql_execution.R") # ensure that this is loaded first


# selectors --------------------------------------------------------------------

bottleList <- readr::read_csv(
  file      = "documents/allPossibleBottleCombinations.csv",
  col_names = TRUE
  ) |>
dplyr::filter(grepl("^9|^10_|^11_", bottles, ignore.case = TRUE))


# R6 ---------------------------------------------------------------------------

# ChemViewer1    <- ChemViewer$new(id = "chem_display")
# ChemInventory1 <- ChemInventory$new(id = "chem_inventory")
