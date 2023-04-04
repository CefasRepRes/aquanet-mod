#### Run model command line ####

library(beepr)

# Setup ------------------------------------------------------------------------

# Clear the startup screen
cat('\014')

# Clear objects from the workspace
rm(list = ls())

# Set display options
options(width = 1000)

# Load necessary packages ------------------------------------------------------

# List required packages
pkgs <- c("igraph", "sf", "here", "Matrix", "yaml",
          "doParallel", "doRNG", "data.table", "dplyr")

# Check whether installed and if not install from CRAN
for (i in pkgs){
  print(paste0("Checking: ", i))
  if (!requireNamespace(i, quietly = T)) {
    print(paste0("Installing: ", i))
    install.packages(i)
  }
}

# Load packages
lapply(pkgs, library, character.only = T)

# TODO: add install aquanet when it does not require PAT
library(aquanet) # Functions for aquanet model

# User settings ----------------------------------------------------------------

# Number of cores to be assigned
  # We recommend using half the number of cores available on your device
noCores <- detectCores() / 2

# Load in parameters -----------------------------------------------------------

# Location of model parameters file
model_parameter_filepath <- here::here("data",
                                       "Parameterisation",
                                       "scenarios",
                                       "test.yml")

# Load input parameter file
model_parameters <- yaml::yaml.load_file(model_parameter_filepath)

# Set file paths
source(here::here("code",
                  "01_SetFilePaths.R"))


# Coordinate reference system (CGS)
BNG_crs <- sf::st_crs(model_parameters$epsg)

# Create directories to save results -------------------------------------------

# Define output directories
dirs <- c("outputs" = here::here("outputs"),
          "results" = here::here("outputs", model_parameters$scenario_name),
          "results_batch" = here::here("outputs", model_parameters$scenario_name, "batch_results"),
          "results_full" = here::here("outputs", model_parameters$scenario_name, "full_results"),
          "results_code" = here::here("outputs", model_parameters$scenario_name, "code"))

lapply(dirs, dir.create, showWarnings = F)

# Save code and data for reproducibility ---------------------------------------

# Copy model parameters yaml
file.copy(model_parameter_filepath,
          here::here(dirs[["results_code"]]),
          overwrite = TRUE)

# Get list of coding files
coding_files <- list.files(here::here("code"))

# Copy code and parameter file to new coding folder
file.copy(here::here("code", coding_files),
          here::here(dirs[["results_code"]]),
          overwrite = TRUE)

# Print where results are saved
message(paste("Results are saved in:", dirs[["results"]]))

# Load in live fish movement (LFM) data ----------------------------------------

# Section 30

section_30_movements <- read.csv(section_30_lfm_filename, 
                                 colClasses = "character") 

# Farm-to-farm

farm_to_farm_movements <- read.csv(farm_to_farm_lfm_filename, 
                                   colClasses = "character") 

# Load and run components of AquaNet model --------------------------------

source('code/02_CheckCatchmentSiteRelationships.R') # Don't need to run this if you have no duplicates file already
Tywi
source('code/03_CreateContactNetwork.R')
source('code/04_PrepareModelObjects.R')
source('code/05_CreateRiverContactMatrices.R', local = TRUE)
source('code/06_RunCoreSimulationLoop-Parallel.R', local = TRUE);beep()
