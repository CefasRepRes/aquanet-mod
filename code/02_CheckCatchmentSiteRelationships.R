#### Check catchment site relationships ####

# Create a data frame of all sites, including duplicates -----------------------

# Site ID
siteID <- c(lfm_data$Src_Code,
            lfm_data$Dest_Code)

## Get eastings and northings ==================================================

# Get unique NGR site combinations
site_locations <- data.frame(siteID = siteID,
                             NGR = c(lfm_data$Src_NGR, lfm_data$Dest_NGR)) %>%
  unique() %>%
  data.table()

# Remove and warn of sites with no location
print(paste0("Number of sites with no location: ",
        nrow(site_locations[NGR %in% c("", " ")])))

site_locations <- site_locations[NGR != ""][NGR != " "]

# Get eastings and northings
site_coords <- rnrfa::osg_parse(site_locations$NGR)

# Get site data frame
site_data_frame <- data.frame(siteID = site_locations$siteID,
                              easting = site_coords$easting,
                              northing = site_coords$northing,
                              stringsAsFactors = FALSE)

# Temporary save
fwrite(site_data_frame, here::here("data",
                                   "LFMs",
                                   "modified",
                                   "salmonid_sites.csv"))

## Tally number of occurrences =================================================

lfm_data <- data.table(lfm_data)
src_tally <- lfm_data[, .N, by = Src_Code]
dest_tally <- lfm_data[, .N, by = Dest_Code]
lfm_tally <- merge(src_tally,
                   dest_tally,
                   by.x = "Src_Code",
                   by.y = "Dest_Code",
                   all = T)
lfm_tally[is.na(lfm_tally)] <- 0
lfm_tally$noOccurrences <- lfm_tally$N.x + lfm_tally$N.y

## Combine all =================================================================

sites_unique <- merge(site_data_frame,
                      lfm_tally,
                      by.x = "siteID",
                      by.y = "Src_Code",
                      all.x = T)

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
sites_without_catchment_id <- dplyr::filter(sites_within_catchment_id, is.na(RIVER))

print(c("Number of sites with no catchment: ",
        nrow(sites_without_catchment_id)))

# Remove sites without a catchment
sites_within_catchment_id <- dplyr::filter(sites_within_catchment_id, !is.na(RIVER))

# Tidy the table of site to catchment relationships ----------------------------

# Parse out the geometry (eastings and northings)
sites_with_catchment <- sites_within_catchment_id %>% 
  dplyr::mutate(easting = sf::st_coordinates(.)[, 1],
                northing = sf::st_coordinates(.)[, 2])

# Return to a data frame format
sites_with_catchment <- sf::st_set_geometry(sites_with_catchment, NULL)

# Select only those variables we are interested in
sites_with_catchment <- dplyr::select(sites_with_catchment,
                                      siteID, noOccurrences,
                                      S_ID, RIVER,
                                      S_AREA_KM2, 
                                      easting, northing)

# Incorporate information on whether site feeds river water into its facilities ----

# # Get river sources
# river_source <- cbind(farm_to_farm_movements[,c('ScrSiteID','Scr_RiverSourceUsedSince2010')],
#                       farm_to_farm_movements[,c('RecSiteID','Rec_RiverSourceUsedSince2010')])
# colnames(river_source) <- c("SiteID.Source",
#                             "RiverSourceUsedSince2010.Source",
#                             "SiteID.Dest",
#                             "RiverSourceUsedSince2010.Dest")
# 
# # Get table of sites and if they've used a river since 2010
# river_source_long <- data.frame(RiverSourceUsedSince2010 = unlist(river_source[,c("RiverSourceUsedSince2010.Source",
#                                                                                   "RiverSourceUsedSince2010.Dest")]),
#                                 siteID = unlist(river_source[,c("SiteID.Source","SiteID.Dest")]),
#                                 stringsAsFactors = FALSE,
#                                 row.names = NULL)
# # Get unique values
# river_source_unique <- unique(river_source_long)
# 
# # Save the catchment site relationships -----------------------------------
# 
# # Join to river source data       
# sites_with_catchment <- merge(x = sites_with_catchment,
#                               y = river_source_unique,
#                               by = c("siteID"),
#                               all.x = TRUE)

# Deal with duplicates ---------------------------------------------------------

# Extract duplicated sites - used T and F to get both occurrences of duplication
duplicates <- rbind(filter(sites_with_catchment, duplicated(siteID, fromLast = T)),
                    filter(sites_with_catchment, duplicated(siteID, fromLast = F)))

# Get list of duplicated ids
dupe_site_id <- unique(duplicates$siteID)

# Loop over, removing duplicates
organised_dupes <- data.frame()

# Only run if duplicates are present
if(nrow(duplicates) > 0){
  organised_dupes <- data.frame()
  for(i in 1:length(dupe_site_id)){
    pair <- dplyr::filter(duplicates, siteID == duplicates$siteID[i])
    # Only run if duplicates are present
    
    pair <- dplyr::filter(duplicates, siteID == duplicates$siteID[i])
    # If in the same catchment, give easting/northing preference to 
    # the site with the highest number of occurrences
    if(pair$RIVER[1] == pair$RIVER[2] && pair$noOccurrences[1] >= pair$noOccurrences[2]){
      pair_merged <- pair[1, ]
      pair_merged$noOccurrences <- pair$noOccurrences[1] + pair$noOccurrences[2]
    } else if(pair$RIVER[1] == pair$RIVER[2] && pair$noOccurrences[2] > pair$noOccurrences[1]){
      pair_merged <- pair[2, ]
      pair_merged$noOccurrences <- pair$noOccurrences[1] + pair$noOccurrences[2]
    } else {
      # If the catchments don't match, prompt the user to enter the correct catchment name
      catchment <- readline(prompt = paste("Enter correct river catchment name for site", 
                                           duplicates$siteID[i], 
                                           ": "))
      options <- duplicates %>% dplyr::filter(siteID == duplicates$siteID[i]) %>%
        dplyr::select(RIVER)
      options <- as.list(options)
      if(!(catchment %in% options[[1]])){
        stop(paste("The catchment you entered is incorrect or may be misspelled. Your options are:"),
             options)
      }
      pair_merged <- dplyr::filter(pair, RIVER == catchment)
    }
    
    # Combine with non-duplicate sites
    sites_unique <- rbind(dplyr::filter(sites_with_catchment, !(siteID %in% dupe_site_id)),
                          organised_dupes)
  }
} else {
  # If no duplicates, same as with catchment
  sites_unique <- sites_with_catchment
}

# Save the catchment site relationships
write.csv(sites_with_catchment, file = sites_with_catchment_filename)

# Save the catchment site relationships with duplicates removed
write.csv(sites_unique, file = site_locs_duplicates_removed_filename)

# Save a table of sites which were not included within any catchment
write.csv(sites_without_catchment_id, file = sites_without_catchment_filename)
