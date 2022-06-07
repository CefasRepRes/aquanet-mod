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

MostImports <- degree(combined_graph2,mode="in")
meanMI <- mean(MostImports)
medianMI <- median(MostImports)
minMI <- min(MostImports)
maxMI <- max(MostImports)
qaun95 <- quantile(MostImports, 0.95)
MINode <- which(MostImports == maxMI)
MIHistData <- hist(MostImports,main="Histogram of Imports",xlab="Number of Fish Imported")

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
