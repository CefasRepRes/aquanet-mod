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

# Set scenario name

scenario_name <- "test"

# Set file paths

source(here::here("code",
                  "SetFilePaths.R"))


# Load in parameters -----------------------------------------------------------

# Load
parameter_file <- read.csv(file = parameter_filepath, 
                           header = TRUE)




# Put scenario name into rowname
rownames(parameter_file) <- scenario_name
parameter_file$Range <- NULL

# Get model setup parameters
river_transmission_parameters <- c("River.Downstream.Current_speed",
                                   "River.Downstream_Transmission_Const",
                                   "River.UpDownstream.Current_speed",
                                   "River.UpDownstream_Transmission_Const")

# Get names of variable parameters
run_time_parameters <- colnames(parameter_file)

# Check all required setup parameters have been supplied
if (length(run_time_parameters) < 12) {
   print("Not all of the required parameters have been supplied.")
} else { 
   if (length(run_time_parameters) > 12)
      print("An excess of parameters have been supplied.")
}

# Clear the startup screen
cat("\014")

# Print parameters
print(str(as.list(run_time_parameters)))
print(str(as.list(river_transmission_parameters)))

# Get parameters as list of numbers

run_time_parameters_list <- unname(parameter_file)

# Load in period of data collection --------------------------------------------

data_collection_period <- 365 * 4 # 2011-2014 = 4 years

# Create directories to save results -------------------------------------------

# Get the filepath
save_results_filepath <- paste(outputs_filepath, scenario_name, sep = "/")

# Overall results file
dir.create(file.path(save_results_filepath), showWarnings = FALSE)

# Batch results folder
dir.create(file.path(save_results_filepath, "batch_results"), showWarnings = FALSE)

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

source('code/CreateContactNetwork.R')
source('code/PrepareModelObjects.R')
source('code/CreateRiverContactMatrices.R', local = TRUE)
source('code/RunCoreSimulationLoop-Parallel.R', local = TRUE)
