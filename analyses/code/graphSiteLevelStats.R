# Basic statistics #
####################

Nodes <- vcount(combined_graph2)
Edges <- ecount(combined_graph2)

is.directed(combined_graph2)
graph.density(combined_graph2)
transitivity(combined_graph2)
assortativity.degree(combined_graph2)

# Imports and Exports #
#######################

# Look at the number of fish imported

site_imports <- degree(new_network,
                      mode="in")
mean_site_imports <- mean(site_imports)
median_site_imports <- median(site_imports)
min_site_imports <- min(site_imports)
max_site_imports <- max(site_imports)
quan_95_site_imports <- quantile(site_imports, 0.95)
site_with_most_imports <- which(site_imports == max_site_imports)
hist(site_imports,main="Histogram of Imports",
     xlab="Number of Fish Imported")

# Look at the number of fish exported

MostExported <- degree(combined_graph2,mode="out")
meanME <- mean(MostExported)
medianME <- median(MostExported)
minME <- min(MostExported)
maxME <- max(MostExported)
MENode <- which(MostExported == maxME)
MEHistData <- hist(MostExported, main = "Histogram of Exports", xlab="Number of Fish Exported")

# Vertex Betweenness

vertexBetweenness <- betweenness(graph=combined_graph2,v=V(combined_graph2))

meanVB <- mean(vertexBetweenness)
medianVB <- median(vertexBetweenness)
minVB <- min(vertexBetweenness)
maxVB <- max(vertexBetweenness)


# Vertex Closeness

vertexCloseness <- closeness(graph=combined_graph2,vids=V(combined_graph2),mode='all')
meannVC <- mean(vertexCloseness)
medianVC <- median(vertexCloseness)
minVC <- min(vertexCloseness)
maxVC <- max(vertexCloseness)

# Edge Betweenness

edgeBetweenness <- edge.betweenness(graph=combined_graph2,e=E(combined_graph2))

meanEB <- mean(edgeBetweenness)
medianEB <- median(edgeBetweenness)
minEB <- min(edgeBetweenness)
maxEB <- max(edgeBetweenness)

# Save complete network, and networks where the sites with the top ten betweenness, closeness, eigenvector, in-degree and out-degree values have been excluded

write.graph(combined_graph2, file = contact_network_filename, format = "graphml")
