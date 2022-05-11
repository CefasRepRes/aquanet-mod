# Package for creating the contact network
library(igraph)

# Package for manipulating tables
library(plyr)

# Package for loading geographic datasets
library(rgdal)

# Package for plotting geographical points
library(sp)

# Package for checking spatial relationships
library(rgeos)

# Location of the Section 30 Records
S30LFM.fileName = paste0('./SQL Files/',Species,'/S30LFM11-14.csv')

# Location of the Farm to Farm Records
Farm2FarmLFM.fileName = paste0('./SQL Files/',Species,'/Farm-farmLFM11-14.csv')

# Location of the EA Catchments
catchmentLayer.fileName = "EA Catchments/catchmnt_50k+TrunkCodes-Filtered-Merged_region.shp"
catchmentLayer.fileName = paste(LocationGISRootDirectory,catchmentLayer.fileName,sep="")

# Location to save the site locations, together with catchment details
siteLocationsWithCatchment.fileName = 'ListSiteLocationsWithCatchment.csv'

# Location of sites which were not located within any catchment
siteLocationsWithoutCatchment.fileName = 'ListSiteLocationsWithoutCatchment.csv'

# Location of sites, following removal of duplicate sites
siteLocationsWithCatchmentDuplicatesRemoved.fileName = 'ListSiteLocationsWithCatchmentNoDuplicates.csv'

# Location to save the contact network
contactNetwork.fileName = 'combinedMovements.graph2.xml'

source('SimulationCode2/CheckCatchmentSiteRelationships.R')
source('SimulationCode2/importSiteData.R')
source('SimulationCode2/graphSiteLevelStats.R')
