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
        graph.contactp.objects, 
        simulationCode, 
        ListRunTimeParameters, 
        graph.withinCatchmentEdges.objects, 
        graph.catchment2Site.objects, 
        graph.riverDistance.objects, 
        graph.estimateSiteDistances.objects, 
        farm_vector, 
        # Options
        associatedSiteControlType = 0, # 0 = within catchment movements,
                                       # 1 = between and within infected catchments,
                                       # 2 = no movement by any sites within infected catchments
        noCores = 4, # Set to 4 (save some computing power)
        locationSaveResults, # File path to save results
        seedNo = 123, # Set the seed associated with pseudo-random number generation
        initialNoInfections = 1) # One initial infection
    }
  )
)

# Run model, without any parallel code, which is useful for debugging
#set.seed(seedNo)

#system.time(expr = {simulationCode(graph.contactp.objects, 1000, 3600,1, ListRunTimeParameters, graph.withinCatchmentEdges.objects, graph.catchment2Site.objects, graph.riverDistance.objects, graph.estimateSiteDistances.objects, farm_vector, associatedSiteControlType, locationSaveResults, initialNoInfections)})
