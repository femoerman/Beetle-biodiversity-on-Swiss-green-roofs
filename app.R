#' *****************************************************************************
#' This is a Shiny web application. You can run the application by clicking
#' the 'Run App' button above.
#' You can publish the app to RS Connect through the 'publish'-button.
#' It sits next to the run button.
#' 
#' How to build your own app.
#' * Build your ui in the ui.R script.
#' * Build your server functionality in the server.R script.
#' * Define more complex functions in the scripts provided in the R-folder.
#'
#' Find out more about building applications with Shiny here:
#'
#'    http://shiny.rstudio.com/
#'    
#' *****************************************************************************

# Set the working directory
setwd("~/work/Beetle-biodiversity-on-Swiss-green-roofs")

#Install any necessary packages
source("install.R")

# Run the config script
source("config.R")

# start the app
runApp(getwd())
