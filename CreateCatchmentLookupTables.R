CreateCatchment2SiteMatrix = function(graph_full, catchmentLayer.fileName, locationSiteDetailsWithModelID) {
  # Create a table 'graph.catchment2site.Merged' which maps site to catchment
  # Ensure that the order of sites matches that included within the contact matrix

  graph.catchmentID = get.vertex.attribute(graph = graph_full,name = "catchmentID",index = V(graph_full))
  graph.siteID = get.vertex.attribute(graph = graph_full, name = "siteID",index = V(graph_full))
  
  graph.catchment2site = as.data.frame(cbind(graph.catchmentID, graph.siteID))
  colnames(graph.catchment2site) = c('TRUNK_CODE','siteID')
  
  graph.catchment2site$Order = seq(1,nrow(graph.catchment2site))
  
  catchmentLayer = readOGR(dsn = catchmentLayer.fileName, 
                           layer="catchmnt_50k+TrunkCodes-Filtered-Merged_region")
  
  catchmentLayer.Table = catchmentLayer@data
  
  graph.catchment2site.merged = merge(x = graph.catchment2site,
                                      y = catchmentLayer.Table,
                                      by.x = 'TRUNK_CODE',
                                      by.y = 'TRUNK_CODE',
                                      sort = FALSE,
                                      all.x = TRUE)
  
  graph.catchment2site.merged = graph.catchment2site.merged[order(graph.catchment2site.merged$Order),]
    
  # Transform the catchment2site table into a logical matrix
  # Keep sites in the same order
  graph.catchment2site$TRUNK_CODE = factor(x = graph.catchment2site$TRUNK_CODE,
                                           levels = unique(graph.catchment2site$TRUNK_CODE),
                                           ordered = TRUE)
  
  row.names(graph.catchment2site) = graph.catchment2site$siteID
  
  graph.catchment2site.matrix = model.matrix(~ 0 + TRUNK_CODE,data = graph.catchment2site)
  graph.catchment2site.matrix2 = Matrix(graph.catchment2site.matrix > 0, sparse = TRUE)
  graph.catchment2site.matrix2 = as(object = graph.catchment2site.matrix2, Class = "dgCMatrix")
  
  return(list(graph.catchment2site.merged, graph.catchment2site.matrix2))
}

TestCatchment2SiteMatrix = function(graph.catchment2Site.objects) {
  graph.catchment2site = graph.catchment2Site.objects[[1]][,c('TRUNK_CODE','siteID')]
  graph.catchment2site.merged = graph.catchment2Site.objects[[1]]
  graph.catchment2site.matrix2 = graph.catchment2Site.objects[[2]]
  
  # Check number of sites without a catchment
  print(subset(graph.catchment2site,siteID %in% subset(graph.catchment2site.merged, TRUNK_CODE == "")$siteID))
  
  # Check the matrix has the same relationships as the table
  
  for (i in 1:5) {
    cat("\n\nCatchment to site table\n")
    print(graph.catchment2site[i,])
    cat("\nMatrix's catchment component\n")
    print(graph.catchment2site.matrix2[i,][as.logical(graph.catchment2site.matrix2[i,])])
    cat("\nMatrix's site component\n")
    print(dimnames(graph.catchment2site.matrix2)[[1]][i])
  }
}

CreateWithinCatchmentEdgesMatrix = function(graph_full, graph.catchment2Site.objects, locationSiteDetailsWithModelID) {
  # Create a logical matrix to specify which edges represent movements within a catchment
  graph.withinCatchmentEdges.matrix= get.adjacency(graph_full, attr="withinCatchment", names=TRUE, sparse = TRUE) 
  
  # Store the position of each within catchment movement, as a simple edge table
  edgeList.withinCatchmentEdges.Table = get.edges(graph_full, E(graph_full)[E(graph_full)$withinCatchment])
  
  # Check the list of movements that occur within a catchment against the LFMD, Google, the Catchment Layer, etc
  edgeList.withinCatchmentEdges.Table.bySiteID = cbind(E(graph_full)$scrSiteID[E(graph_full)$withinCatchment],E(graph_full)$recSiteID[E(graph_full)$withinCatchment])
  colnames(edgeList.withinCatchmentEdges.Table.bySiteID) = c('scrSiteID','recSiteID')
  
  return(list(graph.withinCatchmentEdges.matrix,edgeList.withinCatchmentEdges.Table,edgeList.withinCatchmentEdges.Table.bySiteID))
}