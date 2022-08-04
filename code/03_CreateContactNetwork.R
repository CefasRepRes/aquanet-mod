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

# Get names of columns from combined_movements dataframe and add edges to igraph
columns <- colnames(combined_movements)

for (i in columns) {
  combined_movements_graph <- set_edge_attr(graph = combined_movements_graph,
                                            name = i,
                                            value = combined_movements[[i]])
}

# Within catchment movements (logical)
igraph::E(combined_movements_graph)$withinCatchment <- 
  igraph::E(combined_movements_graph)$scrCatchmentID == igraph::E(combined_movements_graph)$recCatchmentID

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

#combinedMovements.graph <- combined_movements_graph
#vertexList <- vertex_matrix

# Add site category ------------------------------------------------------------

# Load
site_category <- read.csv(file = here::here("data",
                                            "Check_License_Types",
                                            "SummaryLicensesPerSitewithfarm.csv"), 
                          header = TRUE, stringsAsFactors = FALSE)

# Check if there are any sites without a category assigned 
unique_code_ids <- unique(x = c(igraph::E(combined_movements_graph)$scrCode,
                              igraph::E(combined_movements_graph)$recCode))

sites_no_category <- unique_code_ids[!unique_code_ids %in% site_category$Code]

if(length(sites_no_category) != 0) warning(paste(
  length(sites_no_category), "sites do not have a category assigned"))

# Create a data frame of site categories

site_categories_ordered <- data.frame(scrCode = igraph::E(combined_movements_graph)$scrCode,
                                      recCode = igraph::E(combined_movements_graph)$recCode,
                                      order = 1:igraph::ecount(combined_movements_graph),
                                      stringsAsFactors = FALSE)

# Merge with site categories
   # Do twice for source and receiving
site_categories_ordered <- merge(x = site_categories_ordered,
                                 y = site_category,
                                 by.x = c('scrCode'),
                                 by.y = c('Code'))

site_categories_ordered <- merge(x = site_categories_ordered,
                                 y = site_category,
                                 by.x = c('recCode'),
                                 by.y = c('Code'),
                                 suffixes = c('.scr','.rec'))

site_categories_ordered <- site_categories_ordered[order(site_categories_ordered$order),]

# Add site type to igraph edge -------------------------------------------------

# Add as categorical
igraph::E(combined_movements_graph)$scrType <- site_categories_ordered$Category.scr
igraph::E(combined_movements_graph)$recType <- site_categories_ordered$Category.rec

# Get site types
type_columns <- colnames(site_categories_ordered)

# Filter those that start with small medium or large
type_columns <- sort(type_columns[grepl("^Small.|^Medium.|^Large.", type_columns)])

# Add site type to edges of igraph
for (type in type_columns) {
  combined_movements_graph <- set_edge_attr(graph = combined_movements_graph,
                                            name = tolower(type), #lowercase name
                                            value = site_categories_ordered[[type]])
}


# Transfer information on the site's category from edges to vertices -----------

# As categorical

igraph::V(combined_movements_graph)[vertex_matrix[,1]]$type <- igraph::E(combined_movements_graph)$scrType
igraph::V(combined_movements_graph)[vertex_matrix[,2]]$type <- igraph::E(combined_movements_graph)$recType

# As logical

# Small hatchery
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$smallhatch <- igraph::E(combined_movements_graph)$smallhatch.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$smallhatch <- igraph::E(combined_movements_graph)$smalllhatch.rec

# Large hatchery
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$largehatch <- igraph::E(combined_movements_graph)$largehatch.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$largehatch <- igraph::E(combined_movements_graph)$largehatch.rec

# Small restockers
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$smallrestock <- igraph::E(combined_movements_graph)$smallrestock.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$smallrestock <- igraph::E(combined_movements_graph)$smallrestock.rec

# Medium restockers
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$mediumrestock <- igraph::E(combined_movements_graph)$mediumrestock.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$mediumrestock <- igraph::E(combined_movements_graph)$mediumrestock.rec

# Large restockers
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$largerestock <- igraph::E(combined_movements_graph)$largerestock.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$largerestock <- igraph::E(combined_movements_graph)$largerestock.rec

# Small table
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$smalltable <- igraph::E(combined_movements_graph)$smalltable.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$smalltable <- igraph::E(combined_movements_graph)$smalltable.rec

# Medium table
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$mediumtable <- igraph::E(combined_movements_graph)$mediumtable.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$mediumtable <- igraph::E(combined_movements_graph)$mediumtable.rec

# Large table
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$largetable <- igraph::E(combined_movements_graph)$largetable.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$largetable <- igraph::E(combined_movements_graph)$largetable.rec

# Small ongrowers

igraph::V(combined_movements_graph)[vertex_matrix[,1]]$smallongrow <- igraph::E(combined_movements_graph)$smallongrow.scr
igraph::V(combined_movements_graph)[vertex_matrix[,2]]$smallongrow <- igraph::E(combined_movements_graph)$smallongrow.rec

# Medium ongrowers
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$mediumongrow <- igraph::E(combined_movements_graph)$mediumongrow.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$mediumongrow <- igraph::E(combined_movements_graph)$mediumongrow.rec

# Large ongrowers
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$largeongrow <- igraph::E(combined_movements_graph)$largeongrow.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$largeongrow <- igraph::E(combined_movements_graph)$largeongrow.rec

# Small fish
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$smallfish <- igraph::E(combined_movements_graph)$smallfish.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$smallfish <- igraph::E(combined_movements_graph)$smallfish.rec

# Medium fish
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$mediumfish <- igraph::E(combined_movements_graph)$mediumfish.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$mediumfish <- igraph::E(combined_movements_graph)$mediumfish.rec

# Large fish
igraph::V(combined_movements_graph)[vertex_matrix[, 1]]$largefish <- igraph::E(combined_movements_graph)$largefish.scr
igraph::V(combined_movements_graph)[vertex_matrix[, 2]]$largefish <- igraph::E(combined_movements_graph)$largefish.rec

# Remove any foreign sites -----------------------------------------------------

foreign_sites <- igraph::V(combined_movements_graph)[igraph::V(combined_movements_graph)$type == "Foreign"]

combined_movements_graph <- igraph::delete.vertices(graph = combined_movements_graph, 
                                                    v = foreign_sites)
if(length(foreign_sites) != 0) message(paste("Removed", length(foreign_sites),
                                             "foreign sites from contact network"))

# Tidy igraph ------------------------------------------------------------------

# Remove the self loops and multiple edges from the 2010 to 2012 graph
combined_movements_simplified <- igraph::simplify(combined_movements_graph, 
                                          remove.multiple = TRUE, 
                                          remove.loops = TRUE,
                                          edge.attr.comb = list(withinCatchment = "first",
                                                                catchmentID = "first",
                                                                scrCatchmentID = "first",
                                                                recCatchmentID = "first",
                                                                scrSiteID = "first",
                                                                recSiteID = "first",
                                                                year = "first",
                                                                reference = "concat",
                                                                type = "first",
                                                                movements = "sum",
                                                                "ignore"))

# Remove any vertices with a node degree of zero (i.e. unconnected vertices)

node_degree <- igraph::degree(combined_movements_simplified)
unconnected_nodes <- node_degree[node_degree == 0]

# Add message about unconnected nodes
if(length(unconnected_nodes) != 0) message(paste("Removed", length(unconnected_nodes),
                                                 "unconnected sites from contact network"))

combined_movements_simplified <- igraph::delete_vertices(graph = combined_movements_simplified, 
                                                         v = names(unconnected_nodes))

# Save graph 

write.graph(combined_movements_simplified, 
            file = contact_network_filename, 
            format = "graphml")