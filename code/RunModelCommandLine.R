#### Run model command line ####

# Setup ------------------------------------------------------------------------

# Clear the startup screen
cat('\014')

# Clear objects from the workspace
rm(list = ls())

# Set display options
options(width = 1000)

# Load necessary packages ------------------------------------------------------

# List required packages
pkgs <- c("igraph", "sf", "here", "Matrix",
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

# IMPORTANT: if you change these settings, save the command line before running.
#            This will ensure the correct code is copied across with the results.

# Scenario name 
  # This is the name that will appear as your output directory. Make it descriptive
scenario_name <- "top_sites_removed_baseline"

# Remove top sites
remove_top_sites <- TRUE # Whether or not to remove the top most connected sites
prop_sites_keep <- 0.95 # If removing most connected sites, which proportion should be kept?

# Data collection period
  # This is the period of time (in days) over which your LFM data was collected
data_collection_period <- 365 * 4 # 2011-2014 = 4 years

# tmax
  # Maximum time (in days) for which each simulation can run
tmax <- 360 * 10

# Initial number of infections
# The default is one
# Adding more would simulate an outbreak resulting from multiple introductions
initial_no_infections <- 1

# Catchment movement control options
# Set to 0, 1 or 2
# 0 = within catchment movements allowed
# 1 = between and within infected catchments allow
# 2 = no movement allowed by any sites within infected catchments
# None = no catchment movement controls whatsoever
catchment_movement_controls <- 0

# Number of simulations to be run
  # Suggest 4 for a test, and 3000 for a full run
noSims <- 3000

# Number of cores to be assigned
  # We recommend using half the number of cores available on your device
noCores <- detectCores() / 2

# Seed number
  # Set the seed associated with pseudo-random number generation
  # This will ensure your results are repeatable across runs
  # Only change if you want to compare repeatability when using different seeds
seedNo <- 123 

# Coordinate reference system (CGS)

BNG_crs <- sf::st_crs(27700) # Number is the EPSG for the British National Grid

# Load in parameters -----------------------------------------------------------

# Set file paths
source(here::here("code",
                  "01_SetFilePaths.R"))

# Load parameter file
parameter_file <- read.csv(file = parameter_filepath, 
                           header = TRUE)

# Put scenario name into rowname
rownames(parameter_file) <- scenario_name
parameter_file$Range <- NULL

# Get names of variable parameters
run_time_parameters <- colnames(parameter_file)

# Check all required setup parameters have been supplied
if (length(run_time_parameters) < 12) {
   print("Not all of the required parameters have been supplied.")
} else { 
   if (length(run_time_parameters) > 12)
      print("An excess of parameters have been supplied.")
}

# Print parameters
print(str(as.list(run_time_parameters)))

# Get parameters as list of numbers

run_time_parameters_list <- unname(parameter_file)

# Create directories to save results -------------------------------------------

# Create outputs file if doesn't exist
dir.create(outputs_filepath, showWarnings = FALSE)

# Get the filepath
save_results_filepath <- paste(outputs_filepath, scenario_name, sep = "/")

# Overall results file
dir.create(file.path(save_results_filepath), showWarnings = FALSE)

# Batch results folder
dir.create(file.path(save_results_filepath, "batch_results"), showWarnings = FALSE)

# Commit results folder
dir.create(file.path(save_results_filepath, "FullDetails"), showWarnings = FALSE)

# Code folder
dir.create(file.path(save_results_filepath, "code"), showWarnings = FALSE)

# Save code and data for reproducibility ---------------------------------------

# Get list of coding files
coding_files <- list.files(file.path("code"))

# Copy code to new coding folder
file.copy(file.path("code", coding_files), 
          file.path(save_results_filepath, "code"),
          overwrite = TRUE)
file.copy(parameter_filepath, 
          file.path(save_results_filepath, "code"),
          overwrite = TRUE)

# Print where results are saved
message(paste("Results are saved in:", save_results_filepath))

# Load in live fish movement (LFM) data ----------------------------------------

# Section 30

section_30_movements <- read.csv(section_30_lfm_filename, 
                                 colClasses = "character") 

# Farm-to-farm

farm_to_farm_movements <- read.csv(farm_to_farm_lfm_filename, 
                                   colClasses = "character") 

# Load and run components of AquaNet model --------------------------------

source('code/02_CheckCatchmentSiteRelationships.R') # Don't need to run this if you have no duplicates file already
Twyi
source('code/03_CreateContactNetwork.R')
source('code/04_PrepareModelObjects.R')
source('code/05_CreateRiverContactMatrices.R', local = TRUE)
source('code/06_RunCoreSimulationLoop-Parallel.R', local = TRUE)
