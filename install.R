# This file contains packages which should be added to the notebook
# during the build process. It is standard R code which is run during
# the build process and typically comprises a set of `install.packages()`
# commands.
#
# For example, remove the comment from the line below if you wish to
# install the `ggplot2` package.
#
# install.packages('ggplot2')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(shiny)) install.packages('shiny') 
if (!require(rnaturalearth)) install.packages('rnaturalearth') 
if (!require(rnaturalearthdata)) install.packages('rnaturalearthdata') 
if (!require(ggspatial)) install.packages('ggspatial') 
if (!require(rgdal)) install.packages('rgdal') 
if (!require(rjson)) install.packages('rjson') 
if (!require(httr)) install.packages('httr') 
if (!require(writexl)) install.packages('writexl') 
if (!require(shinyWidgets)) install.packages('shinyWidgets') 
if (!require(readxl)) install.packages('readxl') 
if (!require(REDCapR)) install.packages('REDCapR') 
if (!require(thematic)) install.packages('thematic')  


# install.packages("tidyverse") # the tidyverse: dplyr, ggplot,...
# install.packages("shiny") # shiny core
# install.packages("rnaturalearth")
# install.packages("rnaturalearthdata")
# install.packages("ggspatial")
# install.packages("rgdal")
# install.packages("rjson")
# install.packages("httr")
# install.packages("writexl")
# install.packages("shinyWidgets")
# install.packages("readxl")
# install.packages("REDCapR")
# install.packages("thematic")