#### Create contact network ####


# Load required packges ---------------------------------------------------

library(igraph) # Package for creating the contact network
library(rgdal) # Package for loading geographic datasets 
               # TODO: update to sf/stars/terra
library(sp) # Package for plotting geographical points
library(rgeos) # Package for checking spatial relationships
library(here) # Package for getting filenames
library(dplyr) # Package for manipulating tables
              # TODO: see if this can be removed

# Get file locations ------------------------------------------------------

# Location of the Section 30 Records
section_30_lfm_filename <- here::here("data",
                                      "SQL_Files",
                                      Species,
                                      "S30LFM11-14.csv")

# Location of the Farm to Farm Records
farm_to_farm_lfm_filename <- here::here("data",
                                        "SQL_Files",
                                        Species,
                                        "Farm-farmLFM11-14.csv")

# Location of the EA Catchments
catchment_layer_filename <- here::here("data",
                                      "EA_Catchments",
                                      "catchmnt_50k+TrunkCodes-Filtered-Merged_region.shp")


# Location of sites, following removal of duplicate sites
site_locs_duplicates_removed_filename <- here::here("data",
                                                    "ListSiteLocationsWithCatchmentNoDuplicates.csv")


# File paths to save locations --------------------------------------------

# Location to save the site locations, together with catchment details
sites_with_catchment_filename <- here::here("outputs",
                                            scenario_name,
                                            "ListSiteLocationsWithCatchment.csv")

# Location of sites which were not located within any catchment
sites_without_catchment_filename <- here::here("outputs",
                                      scenario_name,
                                      "ListSiteLocationsWithoutCatchment.csv")
# Location to save the contact network
contact_network_filename <- here::here("outputs",
                                      scenario_name,
                                      "combined_movements_simplified_graph.xml")

# Run sub-scripts ---------------------------------------------------------

source('code/CheckCatchmentSiteRelationships.R')
source('code/importSiteData.R')
#source('code/graphSiteLevelStats.R')
