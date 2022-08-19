#### Run core simulation loop parallel ####

# Setup -------------------------------------------------------------------

# Clear the startup screen
cat("\014")

# Display without scientific notation
options(scipen = 999)

# Run model ---------------------------------------------------------------

print(system.time( # Print the system time used by the runSimulations function
  expr = {
    aquanet::runSimulations(
      n_cores = noCores,
      n_sims = noSims,
      seed_num = seedNo,
      clear_results = T,
      tmax = tmax,
      run_time_params = parameter_file,
      non_peak_season_length = "90",
      out_createContactProbabilityMatrix = contact_probability_matrix,
      out_createWithinCatchmentEdges = within_catchment_movements,
      out_createCatchmentToSiteMatrix = catchment_site_matrix,
      out_createRiverDistanceProbabilityMatrix = river_downstream_transmission_matrix,
      out_createDistanceMatrix = site_distances_matrix,
      farm_vector = farm_vector,
      n_states = 42,
      n_initial_infections = initial_no_infections,
      type_catchment_controls = catchment_movement_controls,
      contact_tracing = contact_tracing,
      filepath_results = dirs[["results"]]
    )
  }))

# # Testing ----------------------------------------------------------------------
#
# # # # Set things to their original names
# #
# graph.contactp.objects <- contact_probability_matrix
# 
# 
# # Run model, without any parallel code, which is useful for debugging
# set.seed(123)
# 
# system.time(expr =
#               {simulationCode(graph.contactp.objects = contact_probability_matrix,
#                               graph.contactpalt.objects = contact_probability_matrix_top_sites_removed,
#                               runs = 4,
#                               tmax = tmax,
#                               batchNo = 1,
#                               ListRunTimeParameters = run_time_parameters_list,
#                               graph.withinCatchmentEdges.objects = within_catchment_movements,
#                               graph.catchment2Site.objects = catchment_site_matrix,
#                               graph.riverDistance.objects = river_downstream_transmission_matrix,
#                               graph.estimateSiteDistances.objects = site_distances_matrix,
#                               farm_vector = farm_vector,
#                               associatedSiteControlType = 0,
#                               locationSaveResults = save_results_filepath,
#                               initialNoInfections = 1)})
