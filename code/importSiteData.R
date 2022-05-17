#### Import the site data to create the contact network ####

# Load data --------------------------------------------------------------------

# Live fish movements (LFMs)
section_30_movements <- read.csv(section_30_lfm_filename,
                                 colClasses = "character")
farm_movements <- read.csv(farm_to_farm_lfm_filename,
                           colClasses = "character")

# Import site locations
site_locs_dupes_removed <- read.csv(site_locs_duplicates_removed_filename,
                                    stringsAsFactors = FALSE)[,c('siteID','ID','NAME','TRUNK_CODE')]

# Merge LFMs and site locations

# Section 30 movements - source
section_30_movements <- merge(x = section_30_movements,
                              y = site_locs_dupes_removed,
                              all.x = TRUE,
                              sort = TRUE,
                              by.x = "SourceSiteID",
                              by.y = "siteID")

# Section 30 movements - receiving
section_30_movements <- merge(x = section_30_movements,
                              y = site_locs_dupes_removed,
                              all.x = TRUE,
                              sort = TRUE,
                              by.x = "ReceivingSiteID",
                              by.y = "siteID",
                              suffixes = c('.Source','.Receiving'))

# Farm to farm movements - source
farm_movements <- merge(x = farm_movements,
                        y = site_locs_dupes_removed,
                        all.x = TRUE,
                        sort = TRUE,
                        by.x = "ScrSiteID",
                        by.y = "siteID")

# Farm to farm movements - receiving
farm_movements <- merge(x = farm_movements,
                        y = site_locs_dupes_removed,
                        all.x = TRUE,
                        sort = TRUE,
                        by.x = "RecSiteID",
                        by.y = "siteID",
                        suffixes = c('.Source','.Receiving'))

# Remove any records that have not been matched against a catchment

section_30_movements <- section_30_movements %>%
  dplyr::filter(!is.na(ID.Source)) %>%
  dplyr::filter(!is.na(ID.Receiving))

farm_movements <- farm_movements %>%
  dplyr::filter(!is.na(ID.Source)) %>%
  dplyr::filter(!is.na(ID.Receiving))

# Convert date to R compatible format for section 30 records

section_30_movements$ConsentStart <- as.Date(section_30_movements$ConsentStart, 
                                             format = "%d/%m/%Y")
section_30_movements$ConsentStartYear <- as.character(section_30_movements$ConsentStart, 
                                                      format = "%Y")

# Combine section 30 and farm LFMs into a single data frame --------------------

combined_movements <- data.frame(scrSiteID = c(section_30_movements$SourceSiteID,
                                               farm_movements$ScrSiteID),
                                 recSiteID = c(section_30_movements$ReceivingSiteID,
                                               farm_movements$RecSiteID),
                                 scrPersonID = c(section_30_movements$SourcePersonID,
                                                 farm_movements$ScrPersonID),
                                 recPersonID = c(section_30_movements$ReceivingPersonID,
                                                 farm_movements$DestFarmPersonID),
                                 scrCode = c(section_30_movements$SourceCode,
                                             farm_movements$Scr_Code),
                                 recCode = c(section_30_movements$ReceivingCode,
                                             farm_movements$Rec_Code),
                                 scrCatchmentID = c(section_30_movements$TRUNK_CODE.Source,
                                                    farm_movements$TRUNK_CODE.Source),
                                 recCatchmentID = c(section_30_movements$TRUNK_CODE.Receiving,
                                                    farm_movements$TRUNK_CODE.Receiving),
                                 year = c(section_30_movements$ConsentStartYear,
                                          farm_movements$ProdYear),
                                 reference = c(section_30_movements$Refno,
                                               farm_movements$ProductionID))

# Generate igraph of all LFMs --------------------------------------------------

# Combine section 30 and farm LFMs
combined_movement_ids <- combined_movements %>% dplyr::select(scrSiteID,
                                                              recSiteID)
combined_movement_ids <- as.matrix(combined_movement_ids)

# Create igraph
combined_movements_graph <- igraph::graph.edgelist(combined_movement_ids)

# Add information to network edges ---------------------------------------------

# Site Details
   # TODO: figure out a way to streamline this - loops in igraph are hard

# Person ID
igraph::E(combined_movements_graph)$scrPersonID <- combined_movements$scrPersonID
igraph::E(combined_movements_graph)$recPersonID <- combined_movements$recPersonID

# Code
igraph::E(combined_movements_graph)$scrCode <- combined_movements$scrCode
igraph::E(combined_movements_graph)$recCode <- combined_movements$recCode

# Site ID
igraph::E(combined_movements_graph)$scrSiteID <- combined_movements$scrSiteID
igraph::E(combined_movements_graph)$recSiteID <- combined_movements$recSiteID

# Catchment ID
igraph::E(combined_movements_graph)$scrCatchmentID <- combined_movements$scrCatchmentID
igraph::E(combined_movements_graph)$recCatchmentID <- combined_movements$recCatchmentID

# Within catchment movements (logical)
igraph::E(combined_movements_graph)$withinCatchment <- 
  igraph::E(combined_movements_graph)$scrCatchmentID == igraph::E(combined_movements_graph)$recCatchmentID

# Year of movement
igraph::E(combined_movements_graph)$year <- combined_movements$year

# Unique reference
igraph::E(combined_movements_graph)$reference <- combined_movements$reference

# Number of movements
   # Farm to farm = take from database
   # Section 30 = always counts as one
igraph::E(combined_movements_graph)$movements <- as.numeric(c(rep(1, dim(section_30_movements)[1]),
                                                              farm_movements[,c('NumberOfMovementsSource')]))
igraph::E(combined_movements_graph)$movements[E(combined_movements_graph)$movements == -1] <- 1

# Transfer attributes from edge to vertices ------------------------------------

# Store information on edge connectivity

vertex_matrix <- igraph::get.edges(graph = combined_movements_graph, 
                                   es = E(combined_movements_graph))

# TODO: streamline

# Source ID
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$siteID <- 
  igraph::E(combined_movements_graph)$scrSiteID
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$siteID <- 
  igraph::E(combined_movements_graph)$recSiteID

# Person ID
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$PersonID <- 
  E(combined_movements_graph)$scrPersonID
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$PersonID <- 
  igraph::E(combined_movements_graph)$recPersonID

# Code
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$code <- 
  E(combined_movements_graph)$scrCode
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$code <- 
  igraph::E(combined_movements_graph)$recCode

# Catchment ID
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$catchmentID <- 
  igraph::E(combined_movements_graph)$scrCatchmentID
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$catchmentID <-
  igraph::E(combined_movements_graph)$recCatchmentID

combinedMovements.graph <- combined_movements_graph
vertexList <- vertex_matrix

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

