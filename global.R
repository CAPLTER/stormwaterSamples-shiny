
# libraries ---------------------------------------------------------------

library(shiny)
library(dplyr)
library(readr)
library(DT)
library(stringr)
library(lubridate)
library(DBI)
library(RPostgreSQL)
library(pool)
library(readxl)
# library(shinyjs)


# options -----------------------------------------------------------------

# increase max file upload size
options(shiny.maxRequestSize=30*1024^2)


# supporting modules, functions, and configurations -----------------------

# config
source('config.R')

# generate list of bottle IDs for manual data entry
bottleList <- read_csv('allPossibleBottleCombinations.csv',
                       col_names = TRUE)

# functions
source('helper_sql_execution.R')
source('helper_query_samples.R')
source('helper_sample_locations.R')
source('helper_shiny_input.R')
source('helper_query_solids.R')
source('helper_query_discharge.R')


# modules
source('module_modify_samples.R')
source('module_view_discharge.R')
source('module_cations.R')