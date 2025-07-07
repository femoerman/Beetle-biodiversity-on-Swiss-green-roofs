#' run this config script to set the secrets as environment variables
#' run this as well if you are not starting the shiny app, so that all files
#' from the R folder are sourced

CONFIG_SUCCESS <- FALSE

# Set the deployment environment
env.deploy="posit"

#Install any necessary packages
source("install.R")

# Load any secrets
source("secret_keys.R")

#Read the environmental variables, if they don't exist yet
if(env.deploy=="posit"){
  redcap_url <- Sys.getenv("redcap_url")
  redcap_ind_beetles_token <- Sys.getenv("redcap_ind_beetles_token")
  redcap_ind_roofs_token <- Sys.getenv("redcap_ind_roofs_token")
  redcap_obs_beetles_token <- Sys.getenv("redcap_obs_beetles_token")
}
scripts <- paste0("R/", list.files("R/", recursive = FALSE))
lapply(scripts, source)


# LOAD FUNCTIONS FOR TOKENS ----------------------------------------------------
# create a file "secret_keys.R with a function for each of your token,
# at least three: artenliste, daecher, bugs/heuschrecken, etc.
# get_token_<projectname/short> <- function() {
#   api_token <- "your token, REDCap project specific"
#   return(api_token)
# }
#
# seems complicated? These functions hide the way of storing the tokens from
# the rest of your code. If you decide to use an external tool, or Renviron
# you only need to change these functions, and not any other places in your code
# source("secret_token.R")


# SUCCESSFULLY LOADED THE CONFIG -----------------------------------------------
CONFIG_SUCCESS <- TRUE
