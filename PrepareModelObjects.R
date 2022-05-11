# Clear the workspace of any pre-existing objects
# Clear the console, since it gets a bit slow, if the buffer is too full
listObjectsNotToClear = c('LocationGISRootDirectory','Farm2FarmLFM.fileName','LocationParameterFile', 'ParameterFile', 'runTimeParameters', 'modelSetupParameters', 'runTimeParametersSupplied', 'locationSaveResults.Partial', 'parameterIndex', 'rangeIndex', 'parameter', 'range', 'ListRunTimeParameters', 'ListModelSetupParameters', 'locationSaveResults','siteLocationsWithCatchmentDuplicatesRemoved.fileName','Species')
listObjectsWorkspace = ls()
listObjectsToClear = listObjectsWorkspace[!listObjectsWorkspace %in% listObjectsNotToClear]

rm(list = listObjectsToClear)

# Package for creating the contact network
library(igraph)

# Package for loading geographic datasets
library(rgdal)

# Package for plotting geographical points
library(sp)

# Package for plotting graphs
library(ggplot2)

# Package for creating and dealing with sparse matrices
library(Matrix)

# Packages for running simulations in parallel
library(doParallel)
library(doRNG)

# Packages for manipulating tables
library(plyr)
library(dplyr)
library(reshape2)
library(data.table)

# Package for regular expressions / pattern matching
library(stringr)

# Location where the contact network was saved, as well as the number of days over which data was collected
fileName = "combinedMovements.graph2.xml"
periodDataCollection = 365 * 4

# Import the contact network, in a format specific to the igraph package, and which includes site details
graph_full<-read.graph(file = fileName, format = "graphml")

# Save site information to disk, including enough information to make it possible to infect specific sites within the model
source('SimulationCode2/ListSiteDetailsWithModelID.R')
siteDetailsWithModelID = ListSiteDetailsWithModelID(graph_full, graph.contactp.objects, siteLocationsWithCatchmentDuplicatesRemoved.fileName)
locationSiteDetailsWithModelID = "siteDetailsWithModelID.csv"
write.csv(siteDetailsWithModelID,file= locationSiteDetailsWithModelID, row.names = FALSE)

# Create a matrix which represents the site / catchment relationships
source('SimulationCode2/CreateCatchmentLookupTables.R')

catchmentLayer.fileName = "EA Catchments/catchmnt_50k+TrunkCodes-Filtered-Merged_region.shp"

catchmentLayer.fileName = paste(LocationGISRootDirectory,catchmentLayer.fileName,sep="")
graph.catchment2Site.objects = CreateCatchment2SiteMatrix(graph_full, catchmentLayer.fileName, locationSiteDetailsWithModelID)
TestCatchment2SiteMatrix(graph.catchment2Site.objects)

# Get a list of movements which occur within catchments
graph.withinCatchmentEdges.objects = CreateWithinCatchmentEdgesMatrix(graph_full, graph.catchment2Site.objects, locationSiteDetailsWithModelID)

# Create a matrix which represents the probability of connections being made by Live Fish Movements
source('SimulationCode2/CreateContactPMatrix.R')
graph.contactp.objects = CreateContactPMatrix(graph_full, periodDataCollection)

# Create a histogram showing the number of movements made over a specific contact period
TestContactPMatrix(graph.contactp.objects[[2]],-1)

# Create a histogram showing the estimated probability of contacts being made per a day
TestContactPMatrix(graph.contactp.objects[[3]],1)

# Create a matrix which represents the probability of connections being made by Live Fish Movements without the top 5% source sites
source('SimulationCode2/ControlTopSites.R')
graph.contactpalt.objects = CreateAltContactPMatrix(graph_full, periodDataCollection)

# List those sites which are assumed to represent farms, based on the presence of farm to farm movements
source('SimulationCode2/ListFarmingSites.R')
farm_vector = CreateFarmVector(graph_full)
TestFarmVector(farm_vector, graph.contactp.objects, Farm2FarmLFM.fileName)

smallhatch_vector = CreateSmallHatchVector(graph_full)
largehatch_vector = CreateLargeHatchVector(graph_full)
smallrestock_vector = CreateSmallRestockVector(graph_full)
mediumrestock_vector = CreateMediumRestockVector(graph_full)
largerestock_vector = CreateLargeRestockVector(graph_full)
smalltable_vector = CreateSmallTableVector(graph_full)
mediumtable_vector = CreateMediumTableVector(graph_full)
largetable_vector = CreateLargeTableVector(graph_full)
smallongrow_vector = CreateSmallOngrowVector(graph_full)
mediumongrow_vector = CreateMediumOngrowVector(graph_full)
largeongrow_vector = CreateLargeOngrowVector(graph_full)
smallfish_vector = CreateSmallFishVector(graph_full)
mediumfish_vector = CreateMediumFishVector(graph_full)
largefish_vector = CreateLargeFishVector(graph_full)

source('SimulationCode2/EstimateSite2SiteDistinances.R')
graph.estimateSiteDistances.objects = CreateDistanceMatrix(graph_full, siteLocationsWithCatchmentDuplicatesRemoved.fileName, ListModelSetupParameters)
