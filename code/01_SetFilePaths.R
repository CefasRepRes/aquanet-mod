#### Set file paths ####

# Done here to prevent having to modify within each script
# Note: these should all be portable if using the project

# Import data ------------------------------------------------------------------

# Location of the Section 30 Records
section_30_lfm_filename <- here::here("example_data",
                                      "sec_30_movements.csv")

# Location of the Farm to Farm Records
farm_to_farm_lfm_filename <- here::here("example_data",
                                        "farm_movements.csv")

# Site licence types
site_licences_filename <-  here::here("example_data",
                                      "licence_types.csv")

# Location of the CEH catchments
catchment_layer_filename <- here::here("example_data",
                                       "IHU_Catchments",
                                       "ihu_sections.shp")

# Location of river routes downstream table
river_downstream_filename <- here::here("example_data",
                                        "river_network.csv")

# Outputs ----------------------------------------------------------------------

# Outputs folder
outputs_filepath <- here::here("outputs")

# Contact network
contact_network_filename <- here::here("outputs",
                                       model_parameters$scenario_name,
                                       "combined_movements_simplified_graph.xml")

# Location to save the site locations, together with catchment details
sites_with_catchment_filename <- here::here("outputs",
                                            model_parameters$scenario_name,
                                            "ListSiteLocationsWithCatchment.csv")

# Location of sites which were not located within any catchment
sites_without_catchment_filename <- here::here("outputs",
                                               model_parameters$scenario_name,
                                               "ListSiteLocationsWithoutCatchment.csv")

# Location of sites, following removal of duplicate sites
site_locs_duplicates_removed_filename <- here::here("outputs",
                                                    model_parameters$scenario_name,
                                                    "ListSiteLocationsWithCatchmentNoDuplicates.csv")

# Site details with model ID
site_details_with_model_id_filename <- here::here("outputs",
                                                  model_parameters$scenario_name,
                                                  "site_details_with_model_id.csv")