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
