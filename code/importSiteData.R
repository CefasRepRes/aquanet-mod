#### Import site data ####

# Called by Create contact network

# Compile live fish movements ---------------------------------------------

# Read Section30 and Farm to Farm Movements

section_30_movements <- read.csv(section_30_lfm_filename, 
                             colClasses = "character")

farm_movements <- read.csv(farm_to_farm_lfm_filename, 
                           colClasses="character")

# Import catchment details

site_locations <- read.csv(site_locs_duplicates_removed_filename,
                           stringsAsFactors = FALSE)[,c('siteID','ID','NAME','TRUNK_CODE')]

# Add catchment details to section 30 movements
  # Do twice to assign for source and recieving sites

section_30_movements <- merge(x = section_30_movements,
                          y = site_locations,
                          all.x = TRUE,
                          sort = TRUE,
                          by.x = "SourceSiteID",
                          by.y = "siteID")

section_30_movements <- merge(x = section_30_movements,
                          y = site_locations,
                          all.x = TRUE,
                          sort = TRUE,
                          by.x = "ReceivingSiteID",
                          by.y = "siteID",
                          suffixes = c('.Source','.Receiving'))

# Add catchment details to farm to farm movements
   # Do twice to assign for source and receiving sites

farm_movements <- merge(x = farm_movements,
                        y = site_locations,
                        all.x = TRUE,
                        sort = TRUE,
                        by.x = "ScrSiteID",
                        by.y = "siteID")

farm_movements <- merge(x = farm_movements,
                        y = site_locations,
                        all.x = TRUE,
                        sort = TRUE,
                        by.x = "RecSiteID",
                        by.y = "siteID",
                        suffixes = c('.Source','.Receiving'))


# Data cleaning -----------------------------------------------------------

# Remove any records that have not been matched against a catchment
section_30_movements <- section_30_movements %>% 
  filter(!is.na(ID.Source)) %>% 
  filter(!is.na(ID.Receiving))

farm_movements <- farm_movements %>%
  filter(!is.na(ID.Source)) %>%
  filter(!is.na(ID.Receiving))

# Convert dates of section 30 movements into an R compatible format
section_30_movements$ConsentStart <- as.Date(section_30_movements$ConsentStart, 
                                             format = "%d/%b/%y")
section_30_movements$ConsentStartYear <- as.character(section_30_movements$ConsentStart, 
                                                      format = "%Y")

# Combine sources and receiving sites into matrices

section_30_movements_matrix <- as.matrix(cbind(section_30_movements$SourceSiteID,
                                     section_30_movements$ReceivingSiteID))

farm_movements_matrix <- as.matrix(cbind(farm_movements$ScrSiteID,
                                         farm_movements$RecSiteID))


# Generate igraphs --------------------------------------------------------

# Section 30
section_30_graph <- igraph::graph.edgelist(section_30_movements_matrix)

# Farms
farm_movements_graph <- igraph::graph.edgelist(farm_movements_matrix)

# Combined
combined_matrix <- rbind(section_30_movements_matrix,
                         farm_movements_matrix)
combined_graph <- igraph::graph.edgelist(combined_matrix)


# Add information to network edges ----------------------------------------

# Source person ID
igraph::E(combined_graph)$source_person_id <- c(section_30_movements$SourcePersonID,
                                                farm_movements$SourceFarmPersonID)

# Receiving person ID
igraph::E(combined_graph)$receive_person_id <- c(section_30_movements$ReceivingPersonID,
                                                 farm_movements$DestFarmPersonID)

#Source code
igraph::E(combined_graph)$source_code <- c(section_30_movements$SourceCode,
                                           farm_movements$Scr_Code)

# Receiving code
igraph::E(combined_graph)$receive_code <- c(section_30_movements$ReceivingCode,
                                           farm_movements$Rec_Code)

# Source site id
igraph::E(combined_graph)$source_site_id <- c(section_30_movements$SourceSiteID,
                                              farm_movements$RecSiteID)

# Receiving site id
igraph::E(combined_graph)$receive_site_id <- c(section_30_movements$ReceivingSiteID,
                                               farm_movements$RecSiteID)

# Source and receiver name
   # TODO: see if we can remove to anonymise
E(combined_graph)$scrName <- c(section_30_movements[,c('Source')],farm_movements[,c('Scr_MainName')])
E(combined_graph)$recName <- c(section_30_movements[,c('RecWater')],farm_movements[,c('Rec_MainName')])

# Source catchment ID
igraph::E(combined_graph)$source_catchment_id <- c(section_30_movements$TRUNK_CODE.Source,
                                                  farm_movements$TRUNK_CODE.Source)

# Receiving catchment ID
igraph::E(combined_graph)$receive_catchment_id <- c(section_30_movements$TRUNK_CODE.Receiving,
                                                   farm_movements$TRUNK_CODE.Receiving)

# Note where movements are within catchment
igraph::E(combined_graph)$within_catchment <- 
  E(combined_graph)$source_catchment_id == E(combined_graph)$receive_catchment_id

# Year of movement
igraph::E(combined_graph)$year <- c(section_30_movements$ConsentStartYear,
                                    farm_movements$ProdYear)

# Unique edge reference
igraph::E(combined_graph)$reference <- c(section_30_movements$Refno,
                                         farm_movements$ProductionID)

# Calculate the number of movements
   # such that farm2farm movements are taken direct from the database, 
   # whilst section30 movements count as one movement each

igraph::E(combined_graph)$movements <- as.numeric(c(rep(1,dim(section_30_movements)[1]),farm_movements[,c('NumberOfMovementsSource')]))
igraph::E(combined_graph)$movements[E(combined_graph)$movements == -1] <- 1

# Transfer the attribute information from the edges to the vertices ------------
   # by checking edge connectivity

# Store information on edge connectivity
vertex_matrix <- igraph::get.edges(graph = combined_graph, es = E(combined_graph))
  # vertex_matrix[, 1] = source, vertex_matrix[, 2] = receivers

# Transfer source ID
igraph::V(combined_graph)[vertex_matrix[, 1]]$siteID <- E(combined_graph)$source_site_id
# Transfer receiver ID
igraph::V(combined_graph)[vertex_matrix[, 2]]$siteID <- E(combined_graph)$receive_site_id

# Transfer source person ID
igraph::V(combined_graph)[vertex_matrix[, 1]]$PersonID <- E(combined_graph)$source_person_id
# Transfer receiver person ID
igraph::V(combined_graph)[vertex_matrix[, 2]]$PersonID <- E(combined_graph)$receive_person_id

# Transfer source code
igraph::V(combined_graph)[vertex_matrix[, 1]]$code = E(combined_graph)$source_code
#Transfer receiver code
igraph::V(combined_graph)[vertex_matrix[, 2]]$code = E(combined_graph)$receive_code

# Transfer source and receiver name
  # TODO: See if this is really necessary
igraph::V(combined_graph)[vertex_matrix[,1]]$siteName <- E(combined_graph)$scrName
igraph::V(combined_graph)[vertex_matrix[,2]]$siteName <- E(combined_graph)$recName

# Transfer source catchment ID
igraph::V(combined_graph)[vertex_matrix[, 1]]$catchment_id <- E(combined_graph)$source_catchment_id
igraph::V(combined_graph)[vertex_matrix[, 2]]$catchment_id <- E(combined_graph)$receive_catchment_id

# Import sites category --------------------------------------------------------

# Import
site_category <- read.csv(here::here("data",
                                     "Check_License_Types",
                                     "SummaryLicensesPerSitewithfarm.csv"),
                          header = TRUE, stringsAsFactors = FALSE)

# Get unique site codes from graph
unique_code_ids <- unique(x = c(igraph::E(combined_graph)$source_code,
                                igraph::E(combined_graph)$receive_code))

# Check for any which aren't present in site category
missing <- unique_code_ids[!unique_code_ids %in% site_category$Code]
if(length(missing != 0)) warning("Some sites do not have a category assigned. Check SummaryLicensesPerSitewithfarm.csv")

# Order a data frame of site categories  ---------------------------------------

# Create dat frame
site_categories_ordered <- data.frame(source_code = E(combined_graph)$source_code,
                                      receive_code = E(combined_graph)$receive_code,
                                      order = 1:ecount(combined_graph), # ecound = edge count
                                      stringsAsFactors = FALSE)

# Merge with source type 
site_categories_ordered <- merge(x = site_categories_ordered,
                                 y = site_category,
                                 by.x = c("source_code"),
                                 by.y = c("Code"))

# Merge with receiver type
site_categories_ordered <- merge(x = site_categories_ordered,
                               y = site_category,
                               by.x = c("receive_code"),
                               by.y = c("Code"),
                               suffixes = c('_source','_receive'))

# Order by site categories
site_categories_ordered <- site_categories_ordered[order(site_categories_ordered$order),]
str(site_categories_ordered)

igraph::E(combined_graph)$source_type <- site_categories_ordered$Category_source
igraph::E(combined_graph)$receive_type <- site_categories_ordered$Category_receive

# Assign site type -------------------------------------------------------------

# TODO: figure out how to do this in a more efficient manner
  # Hold up is indexing edge attributes in a loop

# Create vector of all possible site types
site_type <- data.frame(type = c("SmallHatch", "LargeHatch",
               "SmallRestock", "MediumRestock", "LargeRestock",
               "SmallTable", "MediumTable", "LargeTable",
               "SmallOngrow", "MediumOngrow", "LargeOngrow",
               "SmallFish", "MediumFish", "LargeFish"))

# Small hatcheries
igraph::E(combined_graph)$small_hatch_source <- site_categories_ordered$SmallHatch_source
igraph::E(combined_graph)$small_hatch_receive <- site_categories_ordered$SmallHatch_rec

# Large hatcheries
igraph::E(combined_graph)$large_hatch_source <- site_categories_ordered$LargeHatch_source
igraph::E(combined_graph)$large_hatch_receive <- site_categories_ordered$LargeHatch_receive

# Small restockers
igraph::E(combined_graph)$small_restock_source <- site_categories_ordered$SmallRestock_source
igraph::E(combined_graph)$small_restock_receive <- site_categories_ordered$SmallRestock_receive

# Medium restockers
igraph::E(combined_graph)$medium_restock_source <- site_categories_ordered$MediumRestock_source
igraph::E(combined_graph)$medium_restock_receive <- site_categories_ordered$MediumRestock_receive

# Large restockers
igraph::E(combined_graph)$large_restock_source <- site_categories_ordered$LargeRestock_source
igraph::E(combined_graph)$large_restock_receive <- site_categories_ordered$LargeRestock_receive

# Small tables
igraph::E(combined_graph)$small_table_source <- site_categories_ordered$SmallTable_source
igraph::E(combined_graph)$small_table_receive <- site_categories_ordered$SmallTable_receive

# Medium tables
igraph::E(combined_graph)$medium_table_source <- site_categories_ordered$MediumTable_source
igraph::E(combined_graph)$medium_table_receive <- site_categories_ordered$MediumTable_receive

# Large tables
igraph::E(combined_graph)$large_table_source <- site_categories_ordered$LargeTable_source
igraph::E(combined_graph)$large_table_receive <- site_categories_ordered$LargeTable_receive

# Small ongrowers
igraph::E(combined_graph)$small_ongrow_source <- site_categories_ordered$SmallOngrow_source
igraph::E(combined_graph)$small_ongrow_receive <- site_categories_ordered$SmallOngrow_receive

# Medium ongrowers
igraph::E(combined_graph)$medium_ongrow_source <- site_categories_ordered$MediumOngrow_source
igraph::E(combined_graph)$medium_ongrow_receive <- site_categories_ordered$MediumOngrow_receive

# Large ongrowers
igraph::E(combined_graph)$large_ongrow_source <- site_categories_ordered$LargeOngrow_source
igraph::E(combined_graph)$large_ongrow_receive <- site_categories_ordered$LargeOngrow_receive

# Small fishing
igraph::E(combined_graph)$small_fish_source <- site_categories_ordered$SmallFish_source
igraph::E(combined_graph)$small_fish_receive <- site_categories_ordered$SmallFish_receive

# Medium fishing
igraph::E(combined_graph)$medium_fish_source <- site_categories_ordered$MediumFish_source
igraph::E(combined_graph)$medium_fish_receive <- site_categories_ordered$MediumFish_receive

# Large fishing
igraph::E(combined_graph)$large_fish_source <- site_categories_ordered$LargeFish_source
igraph::E(combined_graph)$large_fish_receive <- site_categories_ordered$LargeFish_receive

# Tranfer site type to vertices ------------------------------------------------
# TODO: again, figure out a way to make this more efficient

igraph::V(combined_graph)[vertex_matrix[, 1]]$type = igraph::E(combined_graph)$source_type
igraph::V(combined_graph)[vertex_matrix[, 2]]$type = igraph::E(combined_graph)$receive_type

# Small hatcheries
igraph::V(combined_graph)[vertex_matrix[, 1]]$small_hatch = igraph::E(combined_graph)$small_hatch_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$small_hatch = igraph::E(combined_graph)$small_hatch_receive
# Large hatcheries
igraph::V(combined_graph)[vertex_matrix[, 1]]$large_hatch = igraph::E(combined_graph)$large_hatch_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$large_hatch = igraph::E(combined_graph)$large_hatch_receive

# Small restockers
igraph::V(combined_graph)[vertex_matrix[, 1]]$small_restock = igraph::E(combined_graph)$small_restock_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$small_restock = igraph::E(combined_graph)$small_restock_receive
# Medium restockers
igraph::V(combined_graph)[vertex_matrix[, 1]]$medium_restock = igraph::E(combined_graph)$medium_restock_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$medium_restock = igraph::E(combined_graph)$medium_restock_receive
# Large restockers
igraph::V(combined_graph)[vertex_matrix[, 1]]$large_restock = igraph::E(combined_graph)$large_restock_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$large_restock = igraph::E(combined_graph)$large_restock_receive

# Small tables
igraph::V(combined_graph)[vertex_matrix[, 1]]$small_table = igraph::E(combined_graph)$small_table_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$small_table = igraph::E(combined_graph)$small_table_receive
# Medium tables
igraph::V(combined_graph)[vertex_matrix[, 1]]$medium_table = igraph::E(combined_graph)$medium_table_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$medium_table = igraph::E(combined_graph)$medium_table_receive
# Large tables
igraph::V(combined_graph)[vertex_matrix[, 1]]$large_table = igraph::E(combined_graph)$large_table_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$large_table = igraph::E(combined_graph)$large_table_receive

# Small ongrowers
igraph::V(combined_graph)[vertex_matrix[, 1]]$small_ongrow = igraph::E(combined_graph)$small_ongrow_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$small_ongrow = igraph::E(combined_graph)$small_ongrow_receive
# Medium ongrowers
igraph::V(combined_graph)[vertex_matrix[, 1]]$medium_ongrow = igraph::E(combined_graph)$medium_ongrow_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$medium_ongrow = igraph::E(combined_graph)$medium_ongrow_receive
# Large ongrowers
igraph::V(combined_graph)[vertex_matrix[, 1]]$large_ongrow = igraph::E(combined_graph)$large_ongrow_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$large_ongrow = igraph::E(combined_graph)$large_ongrow_receive

# Small fish
igraph::V(combined_graph)[vertex_matrix[, 1]]$small_fish = igraph::E(combined_graph)$small_fish_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$small_fish = igraph::E(combined_graph)$small_fish_receive
# Medium fish
igraph::V(combined_graph)[vertex_matrix[, 1]]$medium_fish = igraph::E(combined_graph)$medium_fish_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$medium_fish = igraph::E(combined_graph)$medium_fish_receive
# Large fish
igraph::V(combined_graph)[vertex_matrix[, 1]]$large_fish = igraph::E(combined_graph)$large_fish_source
igraph::V(combined_graph)[vertex_matrix[, 2]]$large_fish = igraph::E(combined_graph)$large_fish_receive


# Sometimes the same site has multiple records in the person table
# If so, use the last movement record which mentions a given site
# This should ensure that sites are classified based on an associated person record from farm to farm, rather than S30, movement records
# It is assumed that any cases where person records refer to the same site are due to issues with the less well curated S30 database (though I did check, for this specific dataset)
# It is also assumed that S30 records refer to sites with a lower risk profile (which, again, I did check, for this specific dataset)

# The below code seems circular, so have commented out for now - Sarah
# E(combined_graph)$scrType = V(combined_graph)[vertexList[,1]]$type
# E(combined_graph)$recType = V(combined_graph)[vertexList[,2]]$type
# 
# #########
# E(combined_graph)$smallhatch.scr = V(combined_graph)[vertexList[,1]]$smallhatch
# E(combined_graph)$smallhatch.rec = V(combined_graph)[vertexList[,2]]$smallhatch
# 
# E(combined_graph)$largehatch.scr = V(combined_graph)[vertexList[,1]]$largehatch
# E(combined_graph)$largehatch.rec = V(combined_graph)[vertexList[,2]]$largehatch
# 
# E(combined_graph)$smallrestock.scr = V(combined_graph)[vertexList[,1]]$smallrestock
# E(combined_graph)$smallrestock.rec = V(combined_graph)[vertexList[,2]]$smallrestock
# 
# E(combined_graph)$mediumrestock.scr = V(combined_graph)[vertexList[,1]]$mediumrestock
# E(combined_graph)$mediumrestock.rec = V(combined_graph)[vertexList[,2]]$mediumrestock
# 
# E(combined_graph)$largerestock.scr = V(combined_graph)[vertexList[,1]]$largerestock
# E(combined_graph)$largerestock.rec = V(combined_graph)[vertexList[,2]]$largerestock
# 
# E(combined_graph)$smalltable.scr = V(combined_graph)[vertexList[,1]]$smalltable
# E(combined_graph)$smalltable.rec = V(combined_graph)[vertexList[,2]]$smalltable
# 
# E(combined_graph)$mediumtable.scr = V(combined_graph)[vertexList[,1]]$mediumtable
# E(combined_graph)$mediumtable.rec = V(combined_graph)[vertexList[,2]]$mediumtable
# 
# E(combined_graph)$largetable.scr = V(combined_graph)[vertexList[,1]]$largetable
# E(combined_graph)$largetable.rec = V(combined_graph)[vertexList[,2]]$largetable
# 
# E(combined_graph)$smallongrow.scr = V(combined_graph)[vertexList[,1]]$smallongrow
# E(combined_graph)$smallongrow.rec = V(combined_graph)[vertexList[,2]]$smallongrow
# 
# E(combined_graph)$mediumongrow.scr = V(combined_graph)[vertexList[,1]]$mediumongrow
# E(combined_graph)$mediumongrow.rec = V(combined_graph)[vertexList[,2]]$mediumongrow
# 
# E(combined_graph)$largeongrow.scr = V(combined_graph)[vertexList[,1]]$largeongrow
# E(combined_graph)$largeongrow.rec = V(combined_graph)[vertexList[,2]]$largeongrow
# 
# E(combined_graph)$smallfish.scr = V(combined_graph)[vertexList[,1]]$smallfish
# E(combined_graph)$smallfish.rec = V(combined_graph)[vertexList[,2]]$smallfish
# 
# E(combined_graph)$mediumfish.scr = V(combined_graph)[vertexList[,1]]$mediumfish
# E(combined_graph)$mediumfish.rec = V(combined_graph)[vertexList[,2]]$mediumfish
# 
# E(combined_graph)$largefish.scr = V(combined_graph)[vertexList[,1]]$largefish
# E(combined_graph)$largefish.rec = V(combined_graph)[vertexList[,2]]$largefish

# Remove any foreign sites -----------------------------------------------------

foreign_sites <- igraph::V(combined_graph)[V(combined_graph)$type == "Foreign"]
if(length(foreign_sites) != 0) warning("Some foreign sites are present within the data")

combined_graph <- igraph::delete.vertices(graph = combined_graph, 
                                          v = foreign_sites)

# Remove the self loops and multiple edges from the 2010 to 2012 graph
combined_graph_simplified <- igraph::simplify(combined_graph, 
                                              remove.multiple = TRUE, # Remove multiple edges
                                              remove.loops = TRUE, # Remove loop edges
                                              edge.attr.comb = list(
                                                within_catchment = "first",
                                                catchment_id = "first",
                                                source_catchment_id = "first",
                                                receive_catchment_id = "first",
                                                source_site_id = "first",
                                                receive_site_id = "first",
                                                scrName = "first",
                                                recName = "first", 
                                                year = "first",
                                                reference = "concat",
                                                type = "first",
                                                movements = "sum",
                                                "ignore"))

# Remove any vertices with a node degree of zero (i.e. they are unconnected) ----

node_degree <- degree(combined_graph_simplified)
node_degree_zero <- node_degree[node_degree == 0]
combined_graph_simplified <- delete_vertices(graph = combined_graph_simplified,
                                             v = names(node_degree_zero))
