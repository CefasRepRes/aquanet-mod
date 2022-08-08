runSimulations = function(graph.contactp.objects, graph.contactpalt.objects, remove_top_sites, simulationCode, ListRunTimeParameters, graph.withinCatchmentEdges.objects, graph.catchment2Site.objects, graph.riverDistance.objects, graph.estimateSiteDistances.objects, farm_vector, associatedSiteControlType, noCores, locationSaveResults, seedNo, initialNoInfections) {
  clearResults = function(locationSaveResults) {
    files = list.files(path = locationSaveResults, pattern = "\\.RData$",recursive = TRUE)
    
    if (length(files) != 0) {
      for (i in 1:length(files)) {
        file.remove(paste(locationSaveResults,files[i],sep="/"))
      }
    }
    
  }
  
  clearResults(locationSaveResults)
  
  contactp.length = graph.contactp.objects[[1]]
  
  noSimsPerJob = ceiling(noSims/noCores)
  
  runs <- noSimsPerJob # Reassign to something more sensible for the loop
  tmax <- tmax # This is dumb as all heck but it makes the code work. Otherwise R says it cant find it.
  
  # Assign 12 cores to the cluster, and save all the output to a log file
  Cluster = makeCluster(noCores, outfile = "log.txt")
  
  # Register cluster
  registerDoParallel(Cluster)
  
  overallNoInterations = noSimsPerJob * noCores
  
  print(c(noSims, noSimsPerJob, overallNoInterations))
  
  set.seed(seedNo)
  allruns = foreach(batchNo=1:noCores, .combine=c) %dorng% simulationCode(graph.contactp.objects, graph.contactpalt.objects, remove_top_sites, runs, tmax, batchNo, ListRunTimeParameters, graph.withinCatchmentEdges.objects, graph.catchment2Site.objects, graph.riverDistance.objects, graph.estimateSiteDistances.objects, farm_vector, associatedSiteControlType, locationSaveResults, initialNoInfections)
  
  stopCluster(cl = Cluster)
  
  return(list(noSims, allruns))
}
