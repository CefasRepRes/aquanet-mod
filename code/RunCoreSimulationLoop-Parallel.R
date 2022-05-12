cat("\014")
options(scipen=999)

file.remove('log.txt')


# What type of catchment level controls should be applied?
associatedSiteControlType = 0 # Type 0 allows movements within the same catchment,
# Type 1 allows movements within or between any of the infected catchments
# Type 2 allows no movements by any of the sites within an infected catchment

# Number of cores
noCores = 4

# Set the seed associated with pseudo-random number generation
seedNo = 123

# Set the initial number of infections at the start of the simulation
initialNoInfections = 1

# Run model
print(system.time(expr = {runSimulations(graph.contactp.objects, simulationCode, ListRunTimeParameters, graph.withinCatchmentEdges.objects, graph.catchment2Site.objects, graph.riverDistance.objects, graph.estimateSiteDistances.objects, farm_vector, associatedSiteControlType, noCores, locationSaveResults, seedNo, initialNoInfections)}))

# Run model, without any parallel code, which is useful for debugging
#set.seed(seedNo)

#system.time(expr = {simulationCode(graph.contactp.objects, 1000, 3600,1, ListRunTimeParameters, graph.withinCatchmentEdges.objects, graph.catchment2Site.objects, graph.riverDistance.objects, graph.estimateSiteDistances.objects, farm_vector, associatedSiteControlType, locationSaveResults, initialNoInfections)})
