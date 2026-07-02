
args <- commandArgs(trailingOnly = FALSE)
script_path <- sub("--file=", "", args[grep("--file=", args)])
base_dir <- dirname(normalizePath(script_path))

.libPaths(c(file.path(base_dir, "library"), .libPaths()))
Sys.setenv(RSTUDIO_PANDOC = file.path(base_dir, "pandoc"))

library(shiny)
library(shinyjs)
library(meta)
library(readxl)
library(bslib)
library(dplyr)
library(rmarkdown)
library(dosresmeta)
library(rms)

setwd(base_dir)
source(file.path(base_dir, "app_ui.R"))
source(file.path(base_dir, "app_server.R"))

options(shiny.port = 6865, shiny.host = "127.0.0.1")
shiny::shinyApp(ui = app_ui, server = app_server)

