#### Prepare model objects ####

# Called by RunModelCommdandLine

# Clear the workspace and console
   # Do this to prevent R getting too slow

objects_to_keep <- c("gis_filepath",
                     "farm_to_farm_lmf_filename",
                     "parameter_filepath", 
                     "parameter_file",
                     "run_time_parameters",
                     "run_time_parameters_list",
                     "river_transmission_parameters", 
                     "outputs_filepath", 
                     "parameterIndex", 
                     "save_results_filepath",
                     "site_locs_duplicates_removed_filename",
                     "scenario_name",
                     "data_collection_period",
                     "contact_network_filename",
                     "river_downstream_filename",
                     "tmax",
                     "noSims",
                     "noCores",
                     "catchment_movement_controls",
                     "seedNo",
                     "initial_no_infections",
                     "BNG_crs")
objects_in_workspace <- ls()
objects_to_clear <- objects_in_workspace[!objects_in_workspace %in% objects_to_keep]

# rm(list = objects_to_clear)

# Load contact network ---------------------------------------------------------

# Location where the contact network was saved
   # TODO: put in commmand line?


# Load contact network
graph_full <- read.graph(file = contact_network_filename, format = "graphml")

# Save site information --------------------------------------------------------
# Including enough information to make it possible to infect specific sites within the model

# Load ListSiteDetailsWithModelID.R
   # TODO: update with Becca's package
source('code/aquanet_functions/ListSiteDetailsWithModelID.R')

site_details_with_model_id <- ListSiteDetailsWithModelID(graph_full, 
                                                         site_locs_duplicates_removed_filename)

file_path_sites_with_model_id <- here::here("outputs",
                                            paste0(scenario_name, "/"))
write.csv(site_details_with_model_id,
          file = here::here("outputs",
                            scenario_name,
                            "site_details_with_model_id.csv"), 
          row.names = FALSE)

# Get site - catchment relationships -------------------------------------------

# Get file patch of catchment shapefile
file_path_catchments <- here::here("data",
                                   "EA_Catchments",
                                   "catchmnt_50k+TrunkCodes-Filtered-Merged_region.shp")

# Get catchment to site matrix
catchment_site_matrix <- aquanet::createCatchmentToSiteMatrix(graph = graph_full,
                                                              filename_catchment_layer = file_path_catchments,
                                                              crs_epsg = BNG_crs)

# Extract within-catchment movements -------------------------------------------

within_catchment_movements <- aquanet::createWithinCatchmentEdgesMatrix(graph_full)

# Create contact matrix --------------------------------------------------------
   # Gives the probability of a contact being made via live fish movement (LFM)

contact_probability_matrix <- 
   aquanet::createContactProbabilityMatrix(graph_full,
                                           data_collection_period)

# As above, but remove the top 5% of sites (in terms of number of LFMs)
   # This is required to run some control scenarios and/or to evaluate the impacts of these sites on transmission
contact_probability_matrix_top_sites_removed <- 
   aquanet::createContactProbabilityMatrixTopSitesRemoved(graph_full, 
                                                          data_collection_period,
                                                          percentile = 0.95)

# Get site categories ----------------------------------------------------------
   # TODO: use to incorporate economic data
   # TODO: figure out how to carry this forward to get useful economic outputs
   # Returns 1 for sites being that category, and 0 for site not being that category

source('code/aquanet_functions/CreateSiteTypeVector.R')

# Create list of all possible site types
type_list <- c("smallhatch", "largehatch",
               "smallrestock", "mediumrestock", "largerestock",
               "smalltable", "mediumtable", "largetable",
               "smallongrow", "mediumongrow", "largeongrow",
               "smallfish", "mediumfish", "largefish")

# Save site ID
type_vector <- vertex_attr(graph_full, "siteID")
type_vector <- as.numeric(type_vector)

# Get site type vectors
for(i in 1:length(type_list)){
   t_vector <- createSiteTypeVector(graph_full,
                        type_list[i])
   type_vector <- rbind(type_vector, t_vector)
}

# Format type vector
type_vector <- t(type_vector)
colnames(type_vector) <- c("site_code", type_list)

# Add farm vector
farm_vector <- aquanet::createFarmVector(graph_full)
type_vector <- cbind(type_vector, farm_vector)

# Check for uncategorised sites
row_sums <- rowSums(type_vector[, 2:16])
type_vector <- cbind(type_vector, row_sums)

uncategorised_sites <- dplyr::filter(as.data.frame(type_vector), row_sums == 0)
if(nrow(uncategorised_sites) > 0) message(paste("There are", nrow(uncategorised_sites), "sites with no type designated"))

# Save the type vector

write.csv(type_vector, here::here(outputs_filepath,
                                  scenario_name,
                                  "site_types.csv"),
          row.names = F)

# Get site to site distances ---------------------------------------------------

site_distances_matrix <- aquanet::createDistanceMatrix(graph = graph_full, 
                                                       filename_site_catchments = site_locs_duplicates_removed_filename,
                                                       crs_epsg = BNG_crs,
                                                       sdm_max_dist = parameter_file$Max_Distance_River_Transmission,
                                                       sdm_rate_gamma = parameter_file$Probability_River_Transmission,
                                                       sdm_scalar_lambda = parameter_file$Local_Scalar)
