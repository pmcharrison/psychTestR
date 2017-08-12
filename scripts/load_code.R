library(shiny)

lapply(list.files("functions/", pattern = "*\\.R$", full.names = TRUE), source)

params <- new.env()
source("params.R", local = params)
