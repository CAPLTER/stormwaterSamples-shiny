
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


# options -----------------------------------------------------------------

# increase max file upload size
options(shiny.maxRequestSize=30*1024^2)


# supporting modules, functions, and configurations -----------------------

# config
source('config.R')

# generate list of bottle IDs for manual data entry
bottleList <- read_csv('allPossibleBottleCombinations.csv',
                       col_names = TRUE)

# function
source('helper_sql_execution.R')