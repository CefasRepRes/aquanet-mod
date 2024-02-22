#### Set file paths ####

# Done here to prevent having to modify within each script
# Note: these should all be portable if using the project

# Import data ------------------------------------------------------------------

# Location of the LFM data
lfm_filename <- here::here("example_data",
                           "lfm_data.csv")

# Location of the production data
production_filename <- here::here("example_data",
                                  "production_data.csv")

# Location of tidal site information
tidal_filename <- here::here("example_data",
                             "tidal_sites.csv")

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

# Location of the categorised sites 
site_categories_fiename <- here::here("outputs",
                                      model_parameters$scenario_name,
                                      "categorisedSites.csv")

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
