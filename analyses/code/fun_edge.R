#### Edge functions ####

edge.betweenness.summary <- function(contact_network){
  between <- igraph::edge.betweenness(graph = contact_network,
                                      e = E(contact_network))
  network_betweenness <- data.frame(mean = mean(between),
                                    median = median(between),
                                    min = min(between),
                                    max = max(between))
  return(network_betweenness)
}
