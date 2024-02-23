#### Import the site data to create the contact network ####

# Load data --------------------------------------------------------------------

# Import site locations
site_locs_dupes_removed <- read.csv(site_locs_duplicates_removed_filename,
                                    stringsAsFactors = FALSE)[,c("siteID",
                                                                 "S_ID",
                                                                 "RIVER", 
                                                                 "tidal")]

# Merge LFMs and site locations

# Farm to farm movements - source
lfms <- merge(x = lfm_data,
              y = site_locs_dupes_removed,
              all.x = TRUE,
              sort = TRUE,
              by.x = "Src_Code",
              by.y = "siteID")

# Farm to farm movements - receiving
lfms <- merge(x = lfms,
              y = site_locs_dupes_removed,
              all.x = TRUE,
              sort = TRUE,
              by.x = "Dest_Code",
              by.y = "siteID",
              suffixes = c('.Source','.Receiving'))

# Remove any records that have not been matched against a catchment ------------
  # Retain record of those movements

lfms_no_catchment <- lfms[is.na(S_ID.Source) |
                            is.na(S_ID.Receiving)]

lfms <- lfms[!is.na(S_ID.Source)][!is.na(S_ID.Receiving)]

message(paste("There are", nrow(lfms_no_catchment), "LFMs with no catchment assigned."))

# Convert date to R compatible format ------------------------------------------

lfms$MovementDate <- as.Date(lfms$MovementDate, 
                             format = "%d/%m/%Y")
lfms$MovementYear <- as.character(lfms$MovementDate, 
                                  format = "%Y")

# Organise data frame ----------------------------------------------------------

combined_movements <- lfms[, .(Src_Code,
                               Dest_Code,
                               S_ID.Source,
                               S_ID.Receiving,
                               MovementYear,
                               ConsignmentID,
                               tidal.Source,
                               tidal.Receiving)]
colnames(combined_movements) <- c("scrSiteID",
                                  "recSiteID",
                                  "scrCatchmentID",
                                  "recCatchmentID",
                                  "year",
                                  "reference",
                                  "scrTidal",
                                  "recTidal")

# Generate igraph of all LFMs --------------------------------------------------

# Combine source and receiving LFMs
combined_movement_ids <- combined_movements[, .(scrSiteID, recSiteID)]
combined_movement_ids <- as.matrix(combined_movement_ids)

# Create igraph
combined_movements_graph <- igraph::graph.edgelist(combined_movement_ids)

# Add information to network edges ---------------------------------------------

# Get names of columns from combined_movements dataframe and add edges to igraph
columns <- colnames(combined_movements)

for (i in columns) {
  combined_movements_graph <- igraph::set_edge_attr(graph = combined_movements_graph,
                                                    name = i,
                                                    value = combined_movements[[i]])
}

# Within catchment movements (logical)
igraph::E(combined_movements_graph)$withinCatchment <- 
  igraph::E(combined_movements_graph)$scrCatchmentID == igraph::E(combined_movements_graph)$recCatchmentID

# Number of movements
  # TODO: decide whether to do this by consignment or by number of fish
igraph::E(combined_movements_graph)$movements <- 1

# Transfer attributes from edge to vertices ------------------------------------

# Store information on edge connectivity
vertex_matrix <- igraph::get.edges(graph = combined_movements_graph, 
                                   es = igraph::E(combined_movements_graph))

# Define names of vertices to add
edge_columns <- c("siteID", "CatchmentID", "tidal")

for (col in edge_columns) {
  # Make two names title case as per original code
  if (col == "siteID") {
    edgeName <- gsub("(^[[:alpha:]])", "\\U\\1", col, perl=TRUE)
  } else {
    edgeName <- col
  }
  
  # Sources - set vertex attribute
  igraph::vertex_attr(combined_movements_graph,
                      name = col,
                      index = igraph::V(combined_movements_graph)[vertex_matrix[ , 1]]) <-
    combined_movements[[paste0("scr", edgeName)]]
  
  # Receivers - set vertex attribute
  igraph::vertex_attr(combined_movements_graph,
                      name = col,
                      index = igraph::V(combined_movements_graph)[vertex_matrix[ , 2]]) <-
    combined_movements[[paste0("rec", edgeName)]]
}

# Create a data frame of site IDs
sites_ordered <- data.frame(scrCode = igraph::E(combined_movements_graph)$scrSiteID,
                           recCode = igraph::E(combined_movements_graph)$recSiteID,
                           order = 1:igraph::ecount(combined_movements_graph),
                           stringsAsFactors = FALSE)

# Add site country -------------------------------------------------------------

# Get country
site_countries <- data.frame(siteID = c(lfms$Src_Code, lfms$Dest_Code),
                             country = c(lfms$Src_Country, lfms$Dest_Country)) %>%
  unique() 

# Correct GBE
site_countries$country <- gsub("GBE", "England", site_countries$country)

# Correct those missing countries
for(i in 1:nrow(site_countries)){
  if(!(site_countries$country[i] %in% c("England", "Wales", "Scotland"))){
    site_countries$country[i] <- readline(prompt = paste("Enter the correct country for site", site_countries$siteID[i], ": "))
  }
}

# Merge with site categories
   # Do twice for source and receiving
site_countries_ordered <- merge(x = sites_ordered,
                                 y = site_countries,
                                 by.x = c('scrCode'),
                                 by.y = c('siteID'))

site_countries_ordered <- merge(x = site_countries_ordered,
                                 y = site_countries,
                                 by.x = c('recCode'),
                                 by.y = c('siteID'),
                                 suffixes = c('.scr','.rec'))

site_countries_ordered <- site_countries_ordered[order(site_countries_ordered$order),]

# Add site country to igraph edge 
igraph::E(combined_movements_graph)$scrCountry <- site_countries_ordered$country.scr
igraph::E(combined_movements_graph)$recCountry <- site_countries_ordered$country.rec

# Move to vertices
igraph::V(combined_movements_graph)[vertex_matrix[,1]]$country <- igraph::E(combined_movements_graph)$scrCountry
igraph::V(combined_movements_graph)[vertex_matrix[,2]]$country <- igraph::E(combined_movements_graph)$recCountry

# Get farm vector --------------------------------------------------------------

# Get production data
production_data <- read.csv(production_filename) %>% data.table()

# Get site categories
categorisedSites(lfm_data = lfm_data,
                 production_data = production_data,
                 scenario_name = model_parameters$scenario_name)
categorised_sites <- read.csv(here::here("outputs",
                                         model_parameters$scenario_name,
                                         "categorisedSites.csv"))

# Merge with site categories
  # Do twice: source and receiving
site_types_ordered <- merge(x = site_countries_ordered,
                                y = categorised_sites,
                                by.x = c('scrCode'),
                                by.y = c('Code'))
site_types_ordered <- merge(x = site_types_ordered,
                            y = categorised_sites,
                            by.x = c('recCode'),
                            by.y = c('Code'),
                            suffixes = c(".scr", ".rec"))

site_types_ordered <- site_types_ordered[order(site_types_ordered$order),]

# Add site type to igraph edge 
igraph::E(combined_movements_graph)$scrType <- site_types_ordered$Category.scr
igraph::E(combined_movements_graph)$recType <- site_types_ordered$Category.rec

# Move to vertices
igraph::V(combined_movements_graph)[vertex_matrix[,1]]$type <- igraph::E(combined_movements_graph)$scrType
igraph::V(combined_movements_graph)[vertex_matrix[,2]]$type <- igraph::E(combined_movements_graph)$recType


# Define names of vertices to add ----------------------------------------------
type_columns_uniq <- c("country", "type")

for (col in type_columns_uniq) {

  # Sources - set vertex attribute
  igraph::vertex_attr(combined_movements_graph,
                      name = paste0(tolower(col)),
                      index = igraph::V(combined_movements_graph)[vertex_matrix[ , 1]]) <-
    site_countries_ordered[[paste0(col, ".scr")]]

  # Receivers - set vertex attribute
  igraph::vertex_attr(combined_movements_graph,
                      name = paste0(tolower(col)),
                      index = igraph::V(combined_movements_graph)[vertex_matrix[ , 2]]) <-
    site_countries_ordered[[paste0(col, ".rec")]]
}

# Remove any Scottish sites ----------------------------------------------------

scottish_sites <- igraph::V(combined_movements_graph)[igraph::V(combined_movements_graph)$country == "Scotland"]

combined_movements_graph <- igraph::delete.vertices(graph = combined_movements_graph, 
                                                    v = scottish_sites)
if(length(scottish_sites) != 0) message(paste("Removed", length(scottish_sites),
                                             "Scottish sites from contact network"))

# Tidy igraph ------------------------------------------------------------------

# Remove any self loops and multiple edges 
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
                                                                scrTidal = "first",
                                                                recTidal = "first",
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
