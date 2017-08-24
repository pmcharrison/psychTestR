library(shiny)

library(DT)
library(magrittr)
library(shinyBS)

lapply(list.files("functions/", pattern = "*\\.R$", full.names = TRUE), source)

source("s4.R")
source("imagery_functions.R")
source("gold_msi.R")

params <- new.env()
source("params.R", local = params)
