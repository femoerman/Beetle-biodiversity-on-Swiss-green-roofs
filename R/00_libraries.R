#' configure here what you need
#' load all needed libraries in this script
#' the following constants allow you to choose easily which ones you need
#' we assume shiny and tidyverse as standard

#---- Configuration
USE_BOOTSTRAP <- TRUE # for bootstrap theme
USE_READXL <- TRUE # to read xlsx/xls without requiring java
USE_WRITEXL <- TRUE # to write xlsx without requiring java
USE_REDCAP_API <- TRUE # to use REDCapR for redcap io
 
# Mandatory core packages ----
library(tidyverse) # the tidyverse: dplyr, ggplot,...
library(shiny) # shiny core
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
#library(rgdal)
library(rjson)
library(httr)
library(writexl)
library(shinyWidgets)


# Mandatory for theme_definition.R - Theme and styling (bootstrap) ----- 
if (USE_BOOTSTRAP) {
  if (!require(thematic)) install.packages('thematic')
  library(thematic) # https://rstudio.github.io/thematic/
  
  if (!require(bslib)) install.packages('bslib')
  library(bslib) # styling of HTML / bootstrap
}

# Optional - Input/Output libraries----
if (USE_READXL) {
  if (!require(readxl)) install.packages('readxl')
  library(readxl) # read xlsx
}
if (USE_WRITEXL) {
  if (!require(writexl)) install.packages('writexl')
  library(writexl) # write xlsx
}
if (USE_REDCAP_API) {
   if (!require(REDCapR)) install.packages('REDCapR')
  library(REDCapR) # library to use to get data from REDCap
}


# Optional - additional plot libraries---- 
#if (!require(ggforce)) install.packages('ggforce')
#library(ggforce) # additional functions for ggplot


# Custom - your libraries----

