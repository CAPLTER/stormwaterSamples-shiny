
# libraries ---------------------------------------------------------------

library(shiny)
library(dplyr)
library(tidyr)
library(readr)
library(DT)
library(stringr)
library(lubridate)
library(DBI)
library(RPostgreSQL)
library(pool)
library(readxl)
library(glue)
library(R6)


# options -----------------------------------------------------------------

options(shiny.maxRequestSize = 30*1024^2) # increase max file upload size
options(shiny.reactlog = FALSE)


# supporting modules, functions, and configurations -----------------------

# config
source('config.R')

# generate list of bottle IDs for manual data entry
bottleList <- read_csv('allPossibleBottleCombinations.csv',
                       col_names = TRUE)

# functions
source('helper_sql_execution.R')
source('helper_query_samples.R')
source('helper_query_sites.R')
source('helper_query_analyses.R')
source('helper_shiny_input.R')
source('helper_query_solids.R')
source('helper_query_discharge.R')
source('helper_insert_results.R')
source('helper_insert_raw.R')
source('helper_format_raw.R')


# modules
source('module_modify_samples.R')
source('module_view_discharge.R')
source('module_cations.R')
source('module_lachat.R')
source('module_machine_inputs.R')
source('module_chemistry_viewer.R')

# generate objects
ChemViewer1 <- ChemViewer$new(id = "chem_display")