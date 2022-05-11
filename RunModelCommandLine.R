cat('\014')
rm(list=ls())
detach("package:plyr", unload = TRUE)
options(width = 1000)

# This needs to be updated
locationSaveResults.Partial = "C:/Users/DR05/OneDrive - CEFAS/Network Modelling - Temp/Results"

# This is the name of the folder under 'SQL Files'
Species = "Salmon - V3"

# This is the name of the parameter file
LocationParameterFile = "Parameterisation/SimulationParameters_SimpleCase.csv"

# Location of GIS Data
LocationGISRootDirectory = "//weyfilecds/cdp/FC1215_Defra_R_and_D_AAD/Working_Area/Theme 4 biosecurity/James Network Model/GIS Catchment Files/"

# Check the correct number of parameters have been supplied
ParameterFile = read.csv(file = LocationParameterFile, header = TRUE)
rownames(ParameterFile) = ParameterFile$Range
ParameterFile$Range = NULL

modelSetupParameters = c('River.Downstream.Current_speed','River.Downstream_Transmission_Const','River.UpDownstream.Current_speed','River.UpDownstream_Transmission_Const')
runTimeParameters = colnames(ParameterFile)[!colnames(ParameterFile) %in% modelSetupParameters]

modelSetupParametersSupplied = colnames(ParameterFile)[colnames(ParameterFile) %in% modelSetupParameters]

if (length(modelSetupParameters) != length(modelSetupParametersSupplied)) {
  print("Not all of the required parameters have been supplied.")
}

listParameters = list(ParameterFile, runTimeParameters, modelSetupParameters, locationSaveResults.Partial)

ParameterFile = listParameters[[1]]
runTimeParameters = listParameters[[2]]
modelSetupParameters = listParameters[[3]]
locationSaveResults.Partial = listParameters[[4]]

range = row.names(ParameterFile)[1]
Median = row.names(ParameterFile)[1]


cat("\014")
ListRunTimeParameters = as.vector(ParameterFile[c(Median), runTimeParameters])
ListModelSetupParameters = as.vector(ParameterFile[c(Median), modelSetupParameters])

print(c(range))

print(str(as.list(ListRunTimeParameters)))
print(str(as.list(ListModelSetupParameters)))

locationSaveResults = paste(locationSaveResults.Partial, range, sep = "_")
dir.create(file.path(locationSaveResults), showWarnings = FALSE)
dir.create(file.path(locationSaveResults, "Summary"), showWarnings = FALSE)
dir.create(file.path(locationSaveResults, "FullDetails"), showWarnings = FALSE)
dir.create(file.path(locationSaveResults, "Code"), showWarnings = FALSE)
list.coding.files = list.files(file.path("SimulationCode2"))
file.copy(file.path("SimulationCode2", list.coding.files), 
          file.path(locationSaveResults, "Code"),
          overwrite = TRUE)
file.copy(LocationParameterFile, 
          file.path(locationSaveResults, "Code"),
          overwrite = TRUE)

print(locationSaveResults)

source('SimulationCode2/CreateContactNetwork.R')
source('SimulationCode2/PrepareModelObjects.R', local = TRUE)
source('SimulationCode2/CreateRiverContactMatrices.R', local = TRUE)
source('SimulationCode2/RunCoreSimulationLoop-Parallel.R', local = TRUE)