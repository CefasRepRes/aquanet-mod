#### Prepare model objects ####

# Called by RunModelCommdandLine

# Clear the workspace and console
   # Do this to prevent R getting too slow

#objects_to_keep <- c("gis_filepath",
                    # "farm_to_farm_lmf_filename",
                     #"run_time_parameters",
                     #"run_time_parameters_list",
                     #"river_transmission_parameters", 
                     #"parameterIndex", 
                     #"dirs",
                     #"site_locs_duplicates_removed_filename",
                     #"site_details_with_model_id_filename",
                     #"model_parameters",
                     #"contact_network_filename",
                     #"catchment_layer_filename",
                     #"river_downstream_filename",
                     #"noCores",
                     #"BNG_crs")

#objects_in_workspace <- ls()
#objects_to_clear <- objects_in_workspace[!objects_in_workspace %in% objects_to_keep]

#rm(list = objects_to_clear)

# Load contact network ---------------------------------------------------------

# Load contact network
graph_full <- read_graph(file = contact_network_filename, format = "graphml")

# Save site information --------------------------------------------------------
# Including enough information to make it possible to infect specific sites within the model

site_details_with_model_id <- aquanet::mergeGraphMetaWithCatchmentLocation(graph = graph_full, 
                                                                           filename_sites_catchments = site_locs_duplicates_removed_filename)

write.csv(site_details_with_model_id,
          file = site_details_with_model_id_filename, 
          row.names = FALSE)

# Get site - catchment relationships -------------------------------------------

catchment_site_matrix <- aquanet::createCatchmentToSiteMatrix(graph = graph_full,
                                                              filename_catchment_layer = catchment_layer_filename,
                                                              crs_epsg = BNG_crs)

# Extract within-catchment movements -------------------------------------------

within_catchment_movements <- aquanet::createWithinCatchmentEdgesMatrix(graph_full)

# Create contact matrix --------------------------------------------------------
   # Gives the probability of a contact being made via live fish movement (LFM)

contact_probability_matrix <- 
   aquanet::createContactProbabilityMatrix(graph_full,
                                           model_parameters$data_collection_period)

# As above, but remove the top % of sites (in terms of number of LFMs)
   # This is required to run some control scenarios and/or to evaluate the impacts of these sites on transmission
contact_probability_matrix_top_sites_removed <- 
   aquanet::createContactProbabilityMatrixTopSitesRemoved(graph_full,
                                                          model_parameters$data_collection_period,
                                                          n_remove = model_parameters$n_top_sites_to_remove)



# Get site categories ----------------------------------------------------------

# Add farm vector
farm_vector <- aquanet::createFarmVector(graph_full)

# Get site to site distances ---------------------------------------------------

site_distances_matrix <- aquanet::createDistanceMatrix(graph_full, 
                                                       site_locs_duplicates_removed_filename,
                                                       crs_epsg = BNG_crs,
                                                       sdm_max_dist = model_parameters$Max_Distance_SDM_Transmission,
                                                       sdm_rate_gamma = model_parameters$Probability_SDM_Transmission,
                                                       sdm_scalar_lambda = model_parameters$Local_Scalar)
