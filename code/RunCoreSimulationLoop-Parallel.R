#### Run core simulation loop parallel ####

library(here)

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
        simulationCode = simulationCode, 
        ListRunTimeParameters = run_time_parameters_list, 
        graph.withinCatchmentEdges.objects = within_catchment_movements, 
        graph.catchment2Site.objects = catchment_site_matrix, 
        graph.riverDistance.objects = river_downstream_transmission_objects, 
        graph.estimateSiteDistances.objects = site_distances_matrix, 
        farm_vector = farm_vector, 
        # Options
        associatedSiteControlType = 0, # 0 = within catchment movements,
                                       # 1 = between and within infected catchments,
                                       # 2 = no movement by any sites within infected catchments
        noCores = 4, # Set to 4 (save some computing power)
        locationSaveResults = save_results_filepath, # File path to save results
        seedNo = 125, # Set the seed associated with pseudo-random number generation
        initialNoInfections = 1) # One initial infection
    }
  )
)

# # Testing ----------------------------------------------------------------------
# 
# # # Set things to their original names
# 
# graph.contactp.objects <- contact_probability_matrix
# 
# 
# # Run model, without any parallel code, which is useful for debugging
# set.seed(123)
# 
# system.time(expr =
#               {simulationCode(graph.contactp.objects = contact_probability_matrix,
#                               runs = 4,
#                               tmax = 3600,
#                               batchNo = 1,
#                               ListRunTimeParameters = run_time_parameters_list,
#                               graph.withinCatchmentEdges.objects = within_catchment_movements,
#                               graph.catchment2Site.objects = catchment_site_matrix,
#                               graph.riverDistance.objects = river_downstream_transmission_objects,
#                               graph.estimateSiteDistances.objects = site_distances_matrix,
#                               farm_vector = farm_vector,
#                               associatedSiteControlType = 0,
#                               locationSaveResults = save_results_filepath,
#                               initialNoInfections = 1)})
