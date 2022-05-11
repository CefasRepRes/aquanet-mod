# Basic statistics #
####################

Nodes <- vcount(combinedMovements.graph2)
Edges <- ecount(combinedMovements.graph2)

is.directed(combinedMovements.graph2)
graph.density(combinedMovements.graph2)
transitivity(combinedMovements.graph2)
assortativity.degree(combinedMovements.graph2)

# Imports and Exports #
#######################

# Look at the number of fish imported

MostImports <- degree(combinedMovements.graph2,mode="in")
meanMI <- mean(MostImports)
medianMI <- median(MostImports)
minMI <- min(MostImports)
maxMI <- max(MostImports)
qaun95 <- quantile(MostImports, 0.95)
MINode <- which(MostImports == maxMI)
MIHistData <- hist(MostImports,main="Histogram of Imports",xlab="Number of Fish Imported")

# Look at the number of fish exported

MostExported <- degree(combinedMovements.graph2,mode="out")
meanME <- mean(MostExported)
medianME <- median(MostExported)
minME <- min(MostExported)
maxME <- max(MostExported)
MENode <- which(MostExported == maxME)
MEHistData <- hist(MostExported, main = "Histogram of Exports", xlab="Number of Fish Exported")

# Vertex Betweenness

vertexBetweenness <- betweenness(graph=combinedMovements.graph2,v=V(combinedMovements.graph2))

meanVB <- mean(vertexBetweenness)
medianVB <- median(vertexBetweenness)
minVB <- min(vertexBetweenness)
maxVB <- max(vertexBetweenness)


# Vertex Closeness

vertexCloseness <- closeness(graph=combinedMovements.graph2,vids=V(combinedMovements.graph2),mode='all')
meannVC <- mean(vertexCloseness)
medianVC <- median(vertexCloseness)
minVC <- min(vertexCloseness)
maxVC <- max(vertexCloseness)

# Edge Betweenness

edgeBetweenness <- edge.betweenness(graph=combinedMovements.graph2,e=E(combinedMovements.graph2))

meanEB <- mean(edgeBetweenness)
medianEB <- median(edgeBetweenness)
minEB <- min(edgeBetweenness)
maxEB <- max(edgeBetweenness)

# Save complete network, and networks where the sites with the top ten betweenness, closeness, eigenvector, in-degree and out-degree values have been excluded

write.graph(combinedMovements.graph2, file = "combinedMovements.graph2.xml", format = "graphml")
