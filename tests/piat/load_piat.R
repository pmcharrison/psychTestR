library(shiny)

library(DT)
library(magrittr)
library(shinyBS)
library(shinyjs)
library(shinythemes)

lapply(list.files("common_functions/", pattern = "*\\.R$", full.names = TRUE), source)

source("tests/piat/functions.R")
source("tests/gold_msi/gold_msi.R")
source("tests/demographics/basic_demographics.R")

params <- new.env()
source("tests/piat/params.R", local = params)
