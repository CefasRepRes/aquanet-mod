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

# Set location to save results
  # This should be transportable if using the R project
locationSaveResults.Partial <- here::here("outputs")

# Set species
Species <- "Salmon - V3" # This is the name of the folder under 'SQL Files'
                         # TODO: work out if the V3 is necessary

# Set parameter file location
LocationParameterFile <- here::here("data", 
                                    "parameterisation",
                                    "SimulationParameters_SimpleCase.csv")
# This is the name of the parameter file

# Set the location of the GIS root directory
LocationGISRootDirectory <- here::here("data",
                                       "EA_Catchments")


# Check the correct number of parameters have been supplied ---------------

# TODO: this seems very circular. Remove.

# Load parameter file
ParameterFile <- read.csv(file = LocationParameterFile, 
                          header = TRUE) # TODO: streamline?

# Put scenario name into rowname
   # TODO: figure out a better way of naming the folders according to which scenario is run
rownames(ParameterFile) <- ParameterFile$Range
ParameterFile$Range <- NULL

# Get model setup parameters
modelSetupParameters <- c("River.Downstream.Current_speed",
                          "River.Downstream_Transmission_Const",
                          "River.UpDownstream.Current_speed",
                          "River.UpDownstream_Transmission_Const")

# Remove model setup parameters from list of runtime parameters
   # TODO: arrange so this doesn't have to be done
runTimeParameters <- colnames(ParameterFile)[!colnames(ParameterFile) %in% modelSetupParameters]

# Check what setup parameters have been supplied 
modelSetupParametersSupplied <- colnames(ParameterFile)[colnames(ParameterFile) %in% modelSetupParameters]

# Check all required setup parameters have been supplied
if (length(modelSetupParameters) != length(modelSetupParametersSupplied)) {
  print("Not all of the required parameters have been supplied.")
}


# Get a list of parameters ------------------------------------------------

# TODO:  this does nothing. Remove.

listParameters <- list(ParameterFile, 
                       runTimeParameters, 
                       modelSetupParameters, 
                       locationSaveResults.Partial)

ParameterFile <- listParameters[[1]]
runTimeParameters <- listParameters[[2]]
modelSetupParameters <- listParameters[[3]]
locationSaveResults.Partial <- listParameters[[4]]

range <- row.names(ParameterFile)[1]
Median <- row.names(ParameterFile)[1]


# Run the model -----------------------------------------------------------

# Clear the startup screen
cat("\014")

# Get a list of runtime and setup parameters
   # TODO: update names as these are data frames
ListRunTimeParameters <- as.vector(ParameterFile[c(Median), runTimeParameters])
ListModelSetupParameters <- as.vector(ParameterFile[c(Median), modelSetupParameters])

# Print scenario name
print(c(range))

# Print parameters
print(str(as.list(ListRunTimeParameters)))
print(str(as.list(ListModelSetupParameters)))

# Create directories to save results
   # TODO: change range to scenario name
locationSaveResults <- paste(locationSaveResults.Partial, range, sep = "/") # Create file path
dir.create(file.path(locationSaveResults), showWarnings = FALSE) # Create results folder
dir.create(file.path(locationSaveResults, "Summary"), showWarnings = FALSE) # Create summary folder
dir.create(file.path(locationSaveResults, "FullDetails"), showWarnings = FALSE) # Create full details folder
dir.create(file.path(locationSaveResults, "Code"), showWarnings = FALSE) # Create code folder

# Save code and data for reproducibility
list.coding.files = list.files(file.path("code"))
file.copy(file.path("code", list.coding.files), 
          file.path(locationSaveResults, "Code"),
          overwrite = TRUE)
file.copy(LocationParameterFile, 
          file.path(locationSaveResults, "Code"),
          overwrite = TRUE)

# Print where results are saved
print(locationSaveResults)


# Load and run components of AquaNet model --------------------------------

source('code/CreateContactNetwork.R')
source('code/PrepareModelObjects.R', local = TRUE)
source('code/CreateRiverContactMatrices.R', local = TRUE)
source('code/RunCoreSimulationLoop-Parallel.R', local = TRUE)
