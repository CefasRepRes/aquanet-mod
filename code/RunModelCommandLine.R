#### Run model command line ####

library(here) # Makes writing file paths much easier

# Setup -------------------------------------------------------------------

# Clear the startup screen
cat('\014')

# Clear objects from the workspace
rm(list = ls()) # TODO: work out if this is needed and remove if possible

# Detach plyr
detach("package:plyr", unload = TRUE) # TODO: stop using plyr if possible

# Set display options
options(width = 1000)

# Set ocation to save results
  # This should be transportable if using the R project
outputs_filepath <- here::here("outputs")

# Set species
Species <- "Salmon - V3" # This is the name of the folder under 'SQL Files'
                         # TODO: work out if the V3 is necessary

# Set parameter file location
parameter_filepath <- here::here("data", 
                                 "parameterisation",
                                 "SimulationParameters_SimpleCase.csv")
# This is the name of the parameter file

# Set the location of the GIS root directory
gis_filepath <- here::here("data",
                           "EA_Catchments")

# Load in parameters -----------------------------------------------------------

# Load parameter file
parameter_file <- read.csv(file = parameter_filepath, 
                           header = TRUE) 

# Set scenario name

scenario_name <- "full_test"

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

# Full details folder
   # TODO: check if this is actually needed
dir.create(file.path(save_results_filepath, "full_details"), showWarnings = FALSE)

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
