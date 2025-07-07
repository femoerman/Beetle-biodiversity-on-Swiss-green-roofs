#' Definition of global constants
#' Convention:
#' * use capitals to show that these variables are meant to be constants

# ROOT FOLDER FOR INPUT AND OUTPUT ---------------------------------------------
ROOT_INPUT <- "data/input/"
ROOT_OUTPUT <- "data/output/"
ROOT_PLOT <- paste0(ROOT_OUTPUT, "plots/")

# DATABASE URL -----------------------------------------------------------------
URL <- "https://redcap.zhaw.ch/api/"


# REDCAP INSTRUMENT NAMES ------------------------------------------------------
INSTRUMENT_INDEX_BEETLES <- "index_beetles"
INSTRUMENT_OBSERVATIONS_BEETLES <- "observations_beetles"
INSTRUMENT_INDEX_ROOFS <- "index_roofs"


# TOKENS -----------------------------------------------------------------------
get_token_observations_beetles <- function() {
  api_token <- Sys.getenv("redcap_obs_beetles_token", unset = NA)
  return(api_token)
}

get_token_index_beetles <- function() {
  api_token <- Sys.getenv("redcap_ind_beetles_token", unset = NA)
  return(api_token)
}

get_token_index_roofs <- function() {
  api_token <- Sys.getenv("redcap_ind_roofs_token", unset = NA)
  return(api_token)
}

