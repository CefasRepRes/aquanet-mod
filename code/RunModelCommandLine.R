#### Run model command line ####

# Setup ------------------------------------------------------------------------

# Clear the startup screen
cat('\014')

# Clear objects from the workspace
rm(list = ls())

# Set display options
options(width = 1000)

# Load necessary packages ------------------------------------------------------

library(aquanet) # Functions for aquanet model
library(igraph) # Contact network
library(sf) # Working with spatial data
library(here) # Makes writing file paths much easier
library(Matrix) # Creating and dealing with sparse matrices

# Packages for running simulations in parallel
library(doParallel)
library(doRNG)

# Manipulating tables
library(data.table)
library(dplyr)

# User settings ----------------------------------------------------------------

# IMPORTANT: if you change these settings, save the command line before running.
#            This will ensure the correct code is copied across with the results.

# Scenario name 
  # This is the name that will appear as your output directory. Make it descriptive
scenario_name <- "test"

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
catchment_movement_controls <- 0 

# Number of simulations to be run
  # Suggest 4 for a test, and 3000 for a full run
noSims <- 4

# Number of cores to be assigned
  # We recommend using half the number of cores available on your device
noCores <- detectCores() / 2

# Seed number
  # Set the seed associated with pseudo-random number generation
  # This will ensure your results are repeatable across runs
  # Only change if you want to compare repeatability when using different seeds
seedNo <- 123 

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
dir.create(file.path(save_results_filepath, "full_results"), showWarnings = FALSE)

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

# Load and run components of AquaNet model --------------------------------

#source('code/02_CheckCatchmentSiteRelationships.R') # Don't need to run this if you have no duplicates file already
source('code/03_CreateContactNetwork.R')
source('code/04_PrepareModelObjects.R')
source('code/05_CreateRiverContactMatrices.R', local = TRUE)
source('code/06_RunCoreSimulationLoop-Parallel.R', local = TRUE)
