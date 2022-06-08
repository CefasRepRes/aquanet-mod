#### Vertex functions ####

vertex.betweenness <- function(contact_network){
  between <- igraph::betweenness(graph = contact_network,
                                 v = V(contact_network))
  network_betweenness <- data.frame(mean = mean(between),
                                    median = median(between),
                                    min = min(between),
                                    max = max(between))
  return(network_betweenness)
}

vertex.closeness <- function(contact_network){
  closen <- igraph::closeness(graph = contact_network,
                                 vids = V(contact_network),
                                mode = "all")
  network_closeness <- data.frame(mean = mean(closen),
                                    median = median(closen),
                                    min = min(closen),
                                    max = max(closen))
  return(network_closeness)
}
