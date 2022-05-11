#load("PreparedObjects.RData")
source('SimulationCode2/CreateRiverContactMatrix.R')

riverDownstream.fileName = paste0("CalcRiverDistance/",Species,"/RoutesDownstreamTable.csv")
graph.riverDownstream.objects = CreateRiverDistanceMatrix(riverDownstream.fileName, graph.contactp.objects, ListModelSetupParameters, 1)

TestRiverDistanceMatrix(graph.riverDownstream.objects)
graph.riverDistance.objects = list(graph.riverDownstream.objects, NULL)


source('SimulationCode2/EstimateSite2SiteDistinances.R')
fomite.objects = CreateDistanceMatrix(graph_full, siteLocationsWithCatchmentDuplicatesRemoved.fileName, ListModelSetupParameters)
list.fomite.objects = list(fomite.objects)