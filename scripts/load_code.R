library(shiny)

lapply(list.files("functions/", pattern = "*\\.R$", full.names = TRUE), source)

source("s4.R")

params <- new.env()
source("params.R", local = params)
