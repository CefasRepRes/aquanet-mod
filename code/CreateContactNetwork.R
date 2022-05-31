#### Create contact network ####

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

#source('code/CheckCatchmentSiteRelationships.R') # Don't need to run this if you have no duplicates file already
source('code/importSiteData.R')

# Save completed network 

write.graph(combined_movements_simplified, 
            file = contact_network_filename, 
            format = "graphml")
