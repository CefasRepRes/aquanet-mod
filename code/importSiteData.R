#### Import the site data to create the contact network ####

# Load data --------------------------------------------------------------------

# Live fish movements
section30Movements.CSV <- read.csv(section_30_lfm_filename,colClasses="character")
farmMovements.CSV <- read.csv(farm_to_farm_lfm_filename,colClasses="character")

# Import List of Site Locations, and add Catchment Details to Section 30 and Farm to Farm Movements

# Import site locations
ListSiteLocations.CSV <- read.csv(site_locs_duplicates_removed_filename,
                                  stringsAsFactors = FALSE)[,c('siteID','ID','NAME','TRUNK_CODE')]

# Merge with live fish movements
   # Do twice for section 30 and farm to get source and receiving site
section30Movements.CSV <- merge(x = section30Movements.CSV,
                               y = ListSiteLocations.CSV,
                               all.x = TRUE,
                               sort = TRUE,
                               by.x = "SourceSiteID",
                               by.y = "siteID")

section30Movements.CSV <- merge(x = section30Movements.CSV,
                               y = ListSiteLocations.CSV,
                               all.x = TRUE,
                               sort = TRUE,
                               by.x = "ReceivingSiteID",
                               by.y = "siteID",
                               suffixes = c('.Source','.Receiving'))

farmMovements.CSV <- merge(x = farmMovements.CSV,
                          y = ListSiteLocations.CSV,
                          all.x = TRUE,
                          sort = TRUE,
                          by.x = "ScrSiteID",
                          by.y = "siteID")

farmMovements.CSV <- merge(x = farmMovements.CSV,
                          y = ListSiteLocations.CSV,
                          all.x = TRUE,
                          sort = TRUE,
                          by.x = "RecSiteID",
                          by.y = "siteID",
                          suffixes = c('.Source','.Receiving'))

# Remove any records that have not been matched against a catchment

section30Movements.CSV <- subset(section30Movements.CSV, !(is.na(section30Movements.CSV$ID.Source) | is.na(section30Movements.CSV$ID.Receiving)))
farmMovements.CSV <- subset(farmMovements.CSV, !(is.na(farmMovements.CSV$ID.Source) | is.na(farmMovements.CSV$ID.Receiving)))

# Convert date to R compatible format

section30Movements.CSV$ConsentStart <- as.Date(section30Movements.CSV$ConsentStart, format = "%d/%b/%y")
section30Movements.CSV$ConsentStartYear <- as.character(section30Movements.CSV$ConsentStart, format = "%Y")

# Combine the table of source and destination farms and convert the format from an array to a matrix

section30Movements.array <- cbind(section30Movements.CSV[,c('SourceSiteID')],section30Movements.CSV[,c('ReceivingSiteID')])
farmMovements.array <- cbind(farmMovements.CSV[,c('ScrSiteID')],farmMovements.CSV[,c('RecSiteID')])

# TODO: Already a matrix so remove this bit
# section30Movements.matrix <- as.matrix(section30Movements.array)
# farmMovements.matrix <- as.matrix(farmMovements.array)

# Generate igraph --------------------------------------------------------------

# Generate a farmMovements and Section30Movements graph
# TODO: remove if not needed
#section30Movements.graph = graph.edgelist(section30Movements.matrix)
#farmMovements.graph = graph.edgelist(farmMovements.matrix)

# Create a graph which is the union of the Farm to Farm and the Section 30 movements

combinedMovements.array <- rbind(section30Movements.array,farmMovements.array)
combinedMovements.graph <- graph.edgelist(combinedMovements.array)

# Add information to network edges ---------------------------------------------

# Site Details
   # TODO: specify package
   # TODO: figure out a way to streamline this
E(combinedMovements.graph)$scrPersonID <- c(section30Movements.CSV[,c('SourcePersonID')],farmMovements.CSV[,c('SourceFarmPersonID')])
E(combinedMovements.graph)$recPersonID <- c(section30Movements.CSV[,c('ReceivingPersonID')],farmMovements.CSV[,c('DestFarmPersonID')])
E(combinedMovements.graph)$scrCode <- c(section30Movements.CSV[,c('SourceCode')],farmMovements.CSV[,c('Scr_Code')])
E(combinedMovements.graph)$recCode <- c(section30Movements.CSV[,c('ReceivingCode')],farmMovements.CSV[,c('Rec_Code')])
E(combinedMovements.graph)$scrSiteID <- c(section30Movements.CSV[,c('SourceSiteID')],farmMovements.CSV[,c('ScrSiteID')])
E(combinedMovements.graph)$recSiteID <- c(section30Movements.CSV[,c('ReceivingSiteID')],farmMovements.CSV[,c('RecSiteID')])
E(combinedMovements.graph)$scrName <- c(section30Movements.CSV[,c('Source')],farmMovements.CSV[,c('Scr_MainName')])
E(combinedMovements.graph)$recName <- c(section30Movements.CSV[,c('RecWater')],farmMovements.CSV[,c('Rec_MainName')])
E(combinedMovements.graph)$scrCatchmentID <- c(section30Movements.CSV[,c('TRUNK_CODE.Source')],farmMovements.CSV[,c('TRUNK_CODE.Source')])
E(combinedMovements.graph)$recCatchmentID <- c(section30Movements.CSV[,c('TRUNK_CODE.Receiving')],farmMovements.CSV[,c('TRUNK_CODE.Receiving')])
E(combinedMovements.graph)$withinCatchment <- E(combinedMovements.graph)$scrCatchmentID == E(combinedMovements.graph)$recCatchmentID

# Year of movement and unique edge reference

E(combinedMovements.graph)$year <- c(section30Movements.CSV[,c('ConsentStartYear')],farmMovements.CSV[,c('ProdYear')])
E(combinedMovements.graph)$reference <- c(section30Movements.CSV[,c('Refno')],farmMovements.CSV[,c('ProductionID')])

# Calculate the number of movements such that farm2farm movements are taken direct from the database, whilst section30 movements count as one movement each

E(combinedMovements.graph)$movements <- as.numeric(c(rep(1,dim(section30Movements.CSV)[1]),farmMovements.CSV[,c('NumberOfMovementsSource')]))
E(combinedMovements.graph)$movements[E(combinedMovements.graph)$movements == -1] <- 1

# Transfer the attribute information from the edges to the vertices, by checking edge connectivity ----

# Store information on edge connectivity

vertexList <- get.edges(graph = combinedMovements.graph, es = E(combinedMovements.graph))

# Transfer class information associated with edges to the appropriate vertices ----
# TODO: streamline and add package
# Details on the source and receiving water

V(combinedMovements.graph)[vertexList[,1]]$siteID <- E(combinedMovements.graph)$scrSiteID
V(combinedMovements.graph)[vertexList[,2]]$siteID <- E(combinedMovements.graph)$recSiteID

V(combinedMovements.graph)[vertexList[,1]]$PersonID <- E(combinedMovements.graph)$scrPersonID
V(combinedMovements.graph)[vertexList[,2]]$PersonID <- E(combinedMovements.graph)$recPersonID

V(combinedMovements.graph)[vertexList[,1]]$code <- E(combinedMovements.graph)$scrCode
V(combinedMovements.graph)[vertexList[,2]]$code <- E(combinedMovements.graph)$recCode

V(combinedMovements.graph)[vertexList[,1]]$siteName <- E(combinedMovements.graph)$scrName
V(combinedMovements.graph)[vertexList[,2]]$siteName <- E(combinedMovements.graph)$recName

V(combinedMovements.graph)[vertexList[,1]]$catchmentID <- E(combinedMovements.graph)$scrCatchmentID
V(combinedMovements.graph)[vertexList[,2]]$catchmentID <- E(combinedMovements.graph)$recCatchmentID

# Add site category ------------------------------------------------------------

# Load
siteCategory <- read.csv(file = here::here("data",
                                          "Check_License_Types",
                                          "SummaryLicensesPerSitewithfarm.csv"), header = TRUE, stringsAsFactors = FALSE)

# Merge with igraph
uniqueCodeIDs <- unique(x = c(E(combinedMovements.graph)$scrCode,
                              E(combinedMovements.graph)$recCode))
uniqueCodeIDs[!uniqueCodeIDs %in% siteCategory$Code]

siteCategories.ordered <- data.frame(scrCode = E(combinedMovements.graph)$scrCode,
                                    recCode = E(combinedMovements.graph)$recCode,
                                    order = 1:ecount(combinedMovements.graph),
                                    stringsAsFactors = FALSE)

siteCategories.ordered <- merge(x = siteCategories.ordered,
                               y = siteCategory,
                               by.x = c('scrCode'),
                               by.y = c('Code'))

siteCategories.ordered <- merge(x = siteCategories.ordered,
                               y = siteCategory,
                               by.x = c('recCode'),
                               by.y = c('Code'),
                               suffixes = c('.scr','.rec'))

siteCategories.ordered <- siteCategories.ordered[order(siteCategories.ordered$order),]

# Add site type to igraph edge

E(combinedMovements.graph)$scrType <- siteCategories.ordered$Category.scr
E(combinedMovements.graph)$recType <- siteCategories.ordered$Category.rec

# Add true/false site type to edges of igraph
# TODO: add package and streamline
######## Edges that are small hatcheries
E(combinedMovements.graph)$smallhatch.scr <- siteCategories.ordered$SmallHatch.scr
E(combinedMovements.graph)$smalllhatch.rec <-siteCategories.ordered$SmallHatch.rec
############# Edges that are large hatcheries
E(combinedMovements.graph)$largehatch.scr <- siteCategories.ordered$LargeHatch.scr
E(combinedMovements.graph)$largehatch.rec <- siteCategories.ordered$LargeHatch.rec
########### Edges that are small restockers
E(combinedMovements.graph)$smallrestock.scr <- siteCategories.ordered$SmallRestock.scr
E(combinedMovements.graph)$smallrestock.rec <- siteCategories.ordered$SmallRestock.rec
########### Edges that are medium restockers
E(combinedMovements.graph)$mediumrestock.scr <- siteCategories.ordered$MediumRestock.scr
E(combinedMovements.graph)$mediumrestock.rec <- siteCategories.ordered$MediumRestock.rec
########### Edges that are large restockers
E(combinedMovements.graph)$largerestock.scr <- siteCategories.ordered$LargeRestock.scr
E(combinedMovements.graph)$largerestock.rec <- siteCategories.ordered$LargeRestock.rec
##### Edges that are small tables
E(combinedMovements.graph)$smalltable.scr <- siteCategories.ordered$SmallTable.scr
E(combinedMovements.graph)$smalltable.rec <- siteCategories.ordered$SmallTable.rec
##### Edges that are medium tables
E(combinedMovements.graph)$mediumtable.scr <- siteCategories.ordered$MediumTable.scr
E(combinedMovements.graph)$mediumtable.rec <- siteCategories.ordered$MediumTable.rec
##### Edges that are large tables
E(combinedMovements.graph)$largetable.scr <- siteCategories.ordered$LargeTable.scr
E(combinedMovements.graph)$largetable.rec <- siteCategories.ordered$LargeTable.rec
##### Edges that are small ongrowers
E(combinedMovements.graph)$smallongrow.scr <- siteCategories.ordered$SmallOngrow.scr
E(combinedMovements.graph)$smallongrow.rec <- siteCategories.ordered$SmallOngrow.rec
##### Edges that are medium ongrowers
E(combinedMovements.graph)$mediumongrow.scr <- siteCategories.ordered$MediumOngrow.scr
E(combinedMovements.graph)$mediumongrow.rec <- siteCategories.ordered$MediumOngrow.rec
##### Edges that are largeongrowers
E(combinedMovements.graph)$largeongrow.scr <- siteCategories.ordered$LargeOngrow.scr
E(combinedMovements.graph)$largeongrow.rec <- siteCategories.ordered$LargeOngrow.rec
##### Edges that are smallfishing
E(combinedMovements.graph)$smallfish.scr <- siteCategories.ordered$SmallFish.scr
E(combinedMovements.graph)$smallfish.rec <- siteCategories.ordered$SmallFish.rec
##### Edges that are medium fishing
E(combinedMovements.graph)$mediumfish.scr <- siteCategories.ordered$MediumFish.scr
E(combinedMovements.graph)$mediumfish.rec <- siteCategories.ordered$MediumFish.rec
##### Edges that are large fishing
E(combinedMovements.graph)$largefish.scr <- siteCategories.ordered$LargeFish.scr
E(combinedMovements.graph)$largefish.rec <- siteCategories.ordered$LargeFish.rec


# Transfer information on the site's category from edges to vertices -----------
V(combinedMovements.graph)[vertexList[,1]]$type <- E(combinedMovements.graph)$scrType
V(combinedMovements.graph)[vertexList[,2]]$type <- E(combinedMovements.graph)$recType


####Vertices that are small hatcheries
V(combinedMovements.graph)[vertexList[,1]]$smallhatch <- E(combinedMovements.graph)$smallhatch.scr
V(combinedMovements.graph)[vertexList[,2]]$smallhatch <- E(combinedMovements.graph)$smalllhatch.rec
### Vertices that are large hatcheries
V(combinedMovements.graph)[vertexList[,1]]$largehatch <- E(combinedMovements.graph)$largehatch.scr
V(combinedMovements.graph)[vertexList[,2]]$largehatch <- E(combinedMovements.graph)$largehatch.rec
#### Vertices that are small restockers
V(combinedMovements.graph)[vertexList[,1]]$smallrestock <- E(combinedMovements.graph)$smallrestock.scr
V(combinedMovements.graph)[vertexList[,2]]$smallrestock <- E(combinedMovements.graph)$smallrestock.rec
#### Vertices that are mwsium restockers
V(combinedMovements.graph)[vertexList[,1]]$mediumrestock <- E(combinedMovements.graph)$mediumrestock.scr
V(combinedMovements.graph)[vertexList[,2]]$mediumrestock <- E(combinedMovements.graph)$mediumrestock.rec
##### Vertices that are large restockers
V(combinedMovements.graph)[vertexList[,1]]$largerestock <- E(combinedMovements.graph)$largerestock.scr
V(combinedMovements.graph)[vertexList[,2]]$largerestock <- E(combinedMovements.graph)$largerestock.rec
####Vertices that are small tables
V(combinedMovements.graph)[vertexList[,1]]$smalltable <- E(combinedMovements.graph)$smalltable.scr
V(combinedMovements.graph)[vertexList[,2]]$smalltable <- E(combinedMovements.graph)$smalltable.rec
####Vertices that are medium tables
V(combinedMovements.graph)[vertexList[,1]]$mediumtable <- E(combinedMovements.graph)$mediumtable.scr
V(combinedMovements.graph)[vertexList[,2]]$mediumtable <- E(combinedMovements.graph)$mediumtable.rec
####Vertices that are large tables
V(combinedMovements.graph)[vertexList[,1]]$largetable <- E(combinedMovements.graph)$largetable.scr
V(combinedMovements.graph)[vertexList[,2]]$largetable <- E(combinedMovements.graph)$largetable.rec
####Vertices that are small ongrowers
V(combinedMovements.graph)[vertexList[,1]]$smallongrow <- E(combinedMovements.graph)$smallongrow.scr
V(combinedMovements.graph)[vertexList[,2]]$smallongrow <- E(combinedMovements.graph)$smallongrow.rec
####Vertices that are medium ongrowers
V(combinedMovements.graph)[vertexList[,1]]$mediumongrow <- E(combinedMovements.graph)$mediumongrow.scr
V(combinedMovements.graph)[vertexList[,2]]$mediumongrow <- E(combinedMovements.graph)$mediumongrow.rec
####Vertices that are large ongrowers
V(combinedMovements.graph)[vertexList[,1]]$largeongrow <- E(combinedMovements.graph)$largeongrow.scr
V(combinedMovements.graph)[vertexList[,2]]$largeongrow <- E(combinedMovements.graph)$largeongrow.rec
#####Vertices that are small fish
V(combinedMovements.graph)[vertexList[,1]]$smallfish <- E(combinedMovements.graph)$smallfish.scr
V(combinedMovements.graph)[vertexList[,2]]$smallfish <- E(combinedMovements.graph)$smallfish.rec
####Vertices that are medium fish
V(combinedMovements.graph)[vertexList[,1]]$mediumfish <- E(combinedMovements.graph)$mediumfish.scr
V(combinedMovements.graph)[vertexList[,2]]$mediumfish <- E(combinedMovements.graph)$mediumfish.rec
#####Vertices that are large fish
V(combinedMovements.graph)[vertexList[,1]]$largefish <- E(combinedMovements.graph)$largefish.scr
V(combinedMovements.graph)[vertexList[,2]]$largefish <- E(combinedMovements.graph)$largefish.rec

# Remove any foreign sites which have not already been removed, due to lack of location data ----
# or falling outside of one of the UK river catchments

foreign.vertices <- V(combinedMovements.graph)[V(combinedMovements.graph)$type == "Foreign"]

combinedMovements.graph <- delete.vertices(graph = combinedMovements.graph, 
                                           v = foreign.vertices)

# Tidy igraph ------------------------------------------------------------------

# Remove the self loops and multiple edges from the 2010 to 2012 graph
combinedMovements.graph2 <- simplify(combinedMovements.graph, 
                                     remove.multiple=TRUE, 
                                     remove.loops=TRUE,
                                     edge.attr.comb=list(withinCatchment="first",
                                                         catchmentID="first",
                                                         scrCatchmentID="first",
                                                         recCatchmentID="first",
                                                         scrSiteID="first",
                                                         recSiteID="first",
                                                         scrName="first",
                                                         recName="first", 
                                                         year="first",
                                                         reference="concat",
                                                         type="first",
                                                         movements="sum",
                                                         "ignore"))

# Remove any vertices with a node degree of zero (i.e. they are unconnected)
node.degree <- degree(combinedMovements.graph2)
zero.degree.nodes <- node.degree[node.degree == 0]
combinedMovements.graph2 <- delete_vertices(graph = combinedMovements.graph2, 
                                            v = names(zero.degree.nodes))
