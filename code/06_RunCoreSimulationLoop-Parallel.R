#### Run core simulation loop parallel ####

# Setup -------------------------------------------------------------------

# Clear the startup screen
cat("\014")

# Display without scientific notation
options(scipen = 999)

# Load functions ----------------------------------------------------------

# TODO: replace with Becca's package

source(here::here("code",
                  "loop_functions",
                  "simulation_code.R"))
source(here::here("code",
                  "loop_functions",
                  "run_simulations.R"))

# Run model ---------------------------------------------------------------

print(
  system.time( # Print the system time used by the runSimulations function
    expr = {
      runSimulations( 
        graph.contactp.objects = contact_probability_matrix, 
        graph.contactpalt.objects = contact_probability_matrix_top_sites_removed,
        remove_top_sites = remove_top_sites,
        simulationCode = simulationCode, 
        ListRunTimeParameters = run_time_parameters_list, 
        graph.withinCatchmentEdges.objects = within_catchment_movements, 
        graph.catchment2Site.objects = catchment_site_matrix, 
        graph.riverDistance.objects = river_downstream_transmission_matrix, 
        graph.estimateSiteDistances.objects = site_distances_matrix, 
        farm_vector = farm_vector, 
        noCores = noCores,
        associatedSiteControlType = catchment_movement_controls,
        locationSaveResults = save_results_filepath,
        seedNo = seedNo,
        initialNoInfections = initial_no_infections,
        contact_tracing = contact_tracing)
    }
  )
)

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
