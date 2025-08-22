# Beetle biodiversity on Swiss green roofs

This project contains a dashboard for the Beetle Biodiversity on green roofs in Switzerland.

## Introduction

This is a Renku project - basically a git repository with some
bells and whistles. You'll find we have already created some
useful things like `data` and `notebooks` directories and
a `Dockerfile`.

## How the shiny app works
* The skeleton is based on the jast template, created by Vanessa Klaas.
* The app.R, ui.R and server.R files are in the root directory of the repository.
* The scripts/functions are in the `R` folder.
* The install.R file contains the packages needed to run the app.
* The config file contains the configuration parameters of the app.
* The app can normally directly started from the `app.R` file.
* The app works as follows:
    * The `app.R` first calls the install.R file. Here, any packages that are necessary to run the app, are checked to see if the package is already install, and if not, the package is installed.
    * Next, the `config.R` file is run, to set parameters, and to fetch the environmental variables.
        * If the app is run locally, using a file containing the API tokens, env.deploy needs to be changed to "Rstudio", if the app is run from Renku/a shiny server, it should be set to "posit". This changes the behaviour of reading the API tokens (from file, or as environmental variables).
        * Then, the secrets/secret_keys.R file is sourced, to read the API tokens in Renku.
            * Note that any users that are added in Renku, will need to add this file under the Renku secrets settings for the project, otherwise the app will not work for them.
        * Next, the scripts in the R folder are sourced, to load all the necessary functions. The different scripts are:
            * 00_Libraries.R: loads the libraries needed for the app
            * 00_Library_functions.R: Additional functions (for the REDCap R package and data handling) are defined here.
            * 0_constants.R: Constants used in the app are defined here.
            * 01_redcap_io.R: Functions to read and write data from/to REDCap are defined here.
            * 01_theme_definitions.R: General theme/visual settings are defined here.
            * 02_misc_functions.R: Miscellaneous functions are defined here. Currently doent contain any necessary functions.
            * 10_input_functions.R: Functions to create the input elements of the app are defined here.
            * 20_data_wrangling.R: Functions to wrangle the data are defined here. This file contains the main functions to process the reactive data for the shiny app.
            * 21_data_preparation.R: Functions to prepare lookup tables, as well as for summarized information (e.g. biodiversity indices), and for data tables to upload back to REDCap are defined here.
            * 22_queries.R: Currently does not contain any functions.
            * 23_instrument_manipulation.R: Function definitions that manipulate REDCap instruments.
            * 30_plot_functions.R: Functions to create the plots in the app are defined here.
            * 40_output_functions.R: Functions to create the output elements of the app, including function to write data to REDCap.
            * 99_code_run_once_when_app_starts.R: Code that should be run once when the app starts, e.g. to read in data that does not change during the app session.
* Then, the app is started, and the ui and server files are called. This is what can be done in the app:
    * On the left side, you can filter the data by roof and species, as well as indicate whether data should be cumulative over time, or not, and whether data should be displayed per roof, or per subarea within a roof.
    * You can find sumamrized information for all data selected (summary tab), or see breakdowns by roof (by roof tab), or by species (by species tab), or by both (by observation tab).
    * You can find a list of the roofs (roofs tab), or get a list of the species (species tab).
    * Additionally, you can export the filtered data as a csv file or excel file, in raw format or summarized format.
    * Additionally, you can import the data into REDCap, but here you'll need to provide an API token again, with write permissions. This is for safety reasons, so that not everyone with access to the app should be able to write data to REDCap.
        * Any data that is uploaded this way, will be added as unverified in REDCap, and a notification email will be sent to Franziska, so that she can verify the data.

## How to set up the app in Renku:
* Create a Renku project.
* Under secrets, add a new secret file called `secret_keys.R`, containing the API tokens for REDCap, and containing the URL for the ZHAW REDCap API link(https://redcap.zhaw.ch/api/).
    * Ensure that all users have this file in their secrets, otherwise the app will not work for them. If one user creates it, the secret will also created for the other users, but they will need to add the file/provide the value for that secret.
* Add the code from this repository to the Renku project. This either needs to be from a repository where you have write/reading rights in Renku, or from a public repository.
    * If you have write rights in Renku, you can simply push the code to the repository, using git commands in the terminal in Rstudio.
    * If you do not have write rights in Renku, you can only read in the code, not alter it.:
* Use a launcher with the correct image (currently R-minimal 2025-07-07 13:48:41 GMT+2).
* Start the session, and wait for it to be loaded.
* Then open the terminal, navigate to the repository folder, open the app,R file, and run the app.