#### Check catchment site relationships ####

# Set up British National Grid CRS ---------------------------------------------

BNG_crs <- sf::st_crs(27700) # Number is the EPSG for the British National Grid

# Create a data frame of all sites, including duplicates -----------------------

# Site ID
siteID <- c(section_30_movements$SourceSiteID,
            farm_to_farm_movements$ScrSiteID,
            section_30_movements$ReceivingSiteID,
            farm_to_farm_movements$RecSiteID)

# Eastings
easting <- as.integer(c(section_30_movements$SourceEasting,
                        farm_to_farm_movements$ScrEasting,
                        section_30_movements$RecEasting,
                        farm_to_farm_movements$RecEasting))

# Northings
northing <- as.integer(c(section_30_movements$SourceNorthing,
                         farm_to_farm_movements$ScrNorthing,
                         section_30_movements$RecNorthing,
                         farm_to_farm_movements$RecNorthing))

# Get site data frame
site_data_frame <- data.frame(siteID,
                              easting,
                              northing,
                              stringsAsFactors = FALSE)

sites_unique <- plyr::ddply(site_data_frame,
                            ~siteID + easting + northing,
                            summarise,
                            noOccurances = length(siteID))

duplicate_sites <- plyr::ddply(sites_unique,
                               ~siteID,
                               summarise,
                               frequency = length(siteID))

# Warn of and remove sites without location -----------------------------------

print(c("Number of sites with no location: ",
        nrow(sites_unique %>% filter(is.na(easting)))))

sites_unique <- sites_unique %>% filter(!is.na(easting))

# Assign the correct spatial projection to the table ---------------------------

sites_unique <- sf::st_as_sf(sites_unique,
                             coords = c("easting", "northing"),
                             crs = BNG_crs)

# Open the polygon file defining each catchment in England and Wales -----------

# Open the catchment layer shapefile
catchment_outlines <- sf::read_sf(dsn = catchment_layer_filename,
                               layer = sub(pattern = "(.*)\\..*$",
                                           replacement = "\\1",
                                           basename(catchment_layer_filename)))

# Transform to British National Grid
catchment_outlines_BNG <- sf::st_transform(catchment_outlines, crs = BNG_crs)

# Match sites to catchments -----------------------------------------------

# Join the sites to the catchment that they are located within
sites_within_catchment_id <- sf::st_join(x = sites_unique,
                                         y = catchment_outlines_BNG,
                                         join = st_within,
                                         sparse = TRUE, # Returns a sparse matrix (data frame in this case)
                                         remove = FALSE) # Keep entries which are outside of any catchment

# Get sites without a catchment
sites_without_catchment_id <- dplyr::filter(sites_within_catchment_id, is.na(FEATURE))

# Remove sites without a catchment
sites_within_catchment_id <- dplyr::filter(sites_within_catchment_id, !is.na(FEATURE))

# Tidy the table of site to catchment relationships ----------------------------

# Parse out the geometry (eastings and northings)
sites_with_catchment <- sites_within_catchment_id %>% 
  dplyr::mutate(easting = sf::st_coordinates(.)[, 1],
                northing = sf::st_coordinates(.)[, 2])

# Return to a data frame format
sites_with_catchment <- sf::st_set_geometry(sites_with_catchment, NULL)

# Select only those variables we are interested in
sites_with_catchment <- dplyr::select(sites_with_catchment,
                                      siteID, noOccurances,
                                      ID, FEATURE,
                                      NAME, AREA_HA,
                                      TRUNK_CODE,
                                      easting, northing)

# Incorporate information on whether site feeds river water into its facilities ----

# Get river sources
river_source <- cbind(farm_to_farm_movements[,c('ScrSiteID','Scr_RiverSourceUsedSince2010')],
                      farm_to_farm_movements[,c('RecSiteID','Rec_RiverSourceUsedSince2010')])
colnames(river_source) <- c("SiteID.Source",
                            "RiverSourceUsedSince2010.Source",
                            "SiteID.Dest",
                            "RiverSourceUsedSince2010.Dest")

# Get table of sites and if they've used a river since 2010
river_source_long <- data.frame(RiverSourceUsedSince2010 = unlist(river_source[,c("RiverSourceUsedSince2010.Source",
                                                                                  "RiverSourceUsedSince2010.Dest")]),
                                siteID = unlist(river_source[,c("SiteID.Source","SiteID.Dest")]),
                                stringsAsFactors = FALSE,
                                row.names = NULL)
# Get unique values
river_source_unique <- unique(river_source_long)

# Save the catchment site relationships -----------------------------------

# Join to river source data       
sites_with_catchment <- merge(x = sites_with_catchment,
                              y = river_source_unique,
                              by = c("siteID"),
                              all.x = TRUE)

# Deal with duplicates ---------------------------------------------------------

# Extract duplicated sites - used T and F to get both occurrences of duplication
duplicates <- rbind(filter(sites_with_catchment, duplicated(siteID, fromLast = T)),
                    filter(sites_with_catchment, duplicated(siteID, fromLast = F)))

# Get list of duplicated ids
dupe_site_id <- unique(duplicates$siteID)

# Loop over, removing duplicates
    
organised_dupes <- data.frame()
for(i in 1:length(dupe_site_id)){
  pair <- dplyr::filter(duplicates, siteID == duplicates$siteID[i])
  
  # If in the same catchment, give easting/northing preference to 
  # the site with the highest number of occurrences
  if(pair$NAME[1] == pair$NAME[2] && pair$noOccurances[1] >= pair$noOccurances[2]){
    pair_merged <- pair[1, ]
    pair_merged$noOccurances <- pair$noOccurances[1] + pair$noOccurances[2]
  } else if(pair$NAME[1] == pair$NAME[2] && pair$noOccurances[2] > pair$noOccurances[1]){
    pair_merged <- pair[2, ]
    pair_merged$noOccurances <- pair$noOccurances[1] + pair$noOccurances[2]
  } else {
    
    # If the catchments don't match, prompt the user to enter the correct catchment name
        # TODO: maybe configure this so it gives the user options
    catchment <- readline(prompt = paste("Enter correct river catchment name for site", 
                                         duplicates$siteID[i], 
                                         ": "))
    pair_merged <- dplyr::filter(pair, NAME == catchment)
  }
  organised_dupes <- rbind(organised_dupes, pair_merged)
}

# Combine with non-duplicate sites
sites_unique <- rbind(dplyr::filter(sites_with_catchment, !(siteID %in% dupe_site_id)),
                      organised_dupes)

# Save the catchment site relationships
write.csv(sites_with_catchment, file = sites_with_catchment_filename)

# Save the catchment site relationships with duplicates removed
write.csv(sites_unique, file = site_locs_duplicates_removed_filename)

# Save a table of sites which were not included within any catchment
write.csv(sites_without_catchment_id, file = sites_without_catchment_filename)
