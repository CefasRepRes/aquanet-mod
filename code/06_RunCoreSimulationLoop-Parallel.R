#### Run core simulation loop parallel ####

# Setup -------------------------------------------------------------------

# Clear the startup screen
cat("\014")

# Display without scientific notation
options(scipen = 999)

# Run model ---------------------------------------------------------------

# print(system.time( # Print the system time used by the runSimulations function
#   expr = {
    aquanet::runSimulations(
      n_cores = noCores,
      n_sims = model_parameters$noSims,
      seed_num = model_parameters$seedNo,
      clear_results = model_parameters$clear_results,
      tmax = model_parameters$tmax,
      run_time_params = model_parameters, # note contains all probabilities for transmission methods
      non_peak_season_length = model_parameters$non_peak_season_length,
      out_createContactProbabilityMatrix = contact_probability_matrix,
      out_createContactProbabilityMatrixTopSitesRemoved = contact_probability_matrix_top_sites_removed,
      out_createWithinCatchmentEdges = within_catchment_movements,
      out_createCatchmentToSiteMatrix = catchment_site_matrix,
      out_createRiverDistanceProbabilityMatrix = river_downstream_transmission_matrix,
      out_createDistanceMatrix = site_distances_matrix,
      farm_vector = farm_vector,
      n_states = model_parameters$n_states,
      n_initial_infections = model_parameters$initial_no_infections,
      type_catchment_controls = model_parameters$catchment_movement_controls,
      filepath_results = dirs,
      contact_tracing = model_parameters$contact_tracing,
      remove_top_sites = model_parameters$remove_top_sites,
      n_infections_remove_top_sites = model_parameters$n_infections_remove_top_sites,
      disease_controls = model_parameters$disease_controls,
      proportion_cullable = model_parameters$proportion_cullable,
      days_before_catchment_restock = model_parameters$days_before_catchment_restock,
      river_distances_df = river_distances_df
    )
  # }))

# # Testing --------------------------------------------------------------------
# Runs without paralell - useful for debugging
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
#               {simulationCode(runs = model_parameters$noSims,
#                               tmax = model_parameters$tmax,
#                               batch_num = 1,
#                               run_time_params = model_parameters,
#                               non_peak_season_length = model_parameters$non_peak_season_length,
#                               out_createContactProbabilityMatrix = contact_probability_matrix,
#                               out_createContactProbabilityMatrixTopSitesRemoved = contact_probability_matrix_top_sites_removed,
#                               out_createWithinCatchmentEdges = within_catchment_movements,
#                               out_createCatchmentToSiteMatrix = catchment_site_matrix,
#                               out_createRiverDistanceProbabilityMatrix = river_downstream_transmission_matrix,
#                               out_createDistanceMatrix = site_distances_matrix,
#                               farm_vector = farm_vector,
#                               n_states = model_parameters$n_states,
#                               n_initial_infections = model_parameters$initial_no_infections,
#                               type_catchment_controls = model_parameters$catchment_movement_controls,
#                               filepath_results = dirs,
#                               contact_tracing = model_parameters$contact_tracing,
#                               remove_top_sites = model_parameters$remove_top_sites,
#                               n_infections_remove_top_sites = model_parameters$n_infections_remove_top_sites,
#                               disease_controls = model_parameters$disease_controls,
#                               proportion_cullable = model_parameters$proportion_cullable,
#                               days_before_catchment_restock = model_parameters$days_before_catchment_restock,
#                               river_distances_df = river_distances_df)})
