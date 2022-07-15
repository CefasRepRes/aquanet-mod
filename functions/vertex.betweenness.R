vertex.betweenness <- function(contact_network){
  between <- igraph::betweenness(graph = contact_network,
                                 v = V(contact_network))
  network_betweenness <- data.frame(mean = mean(between),
                                    median = median(between),
                                    min = min(between),
                                    max = max(between))
  return(network_betweenness)
}
