#### Check catchment site relationships ####

# Setup -------------------------------------------------------------------

# Define the British National Grid Referencing System, using Proj4 notation
britishNationalGrid <- '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'

# Get LFM records
section_30_movements <- read.csv(section_30_lfm_filename,
                                 colClasses = "character")
farm_to_farm_movements <- read.csv(farm_to_farm_lfm_filename,
                                   colClasses = "character")

# Create a data frame of all sites, including dupes -----------------------------

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

coordinates(sites_unique) <- c('easting','northing')
proj4string(sites_unique) <- CRS(britishNationalGrid)

# Open the polygon file defining each catchment in England and Wales -----------

# Open shapefile
catchment_outlines <- readOGR(dsn = catchment_layer_filename, 
                              layer = "catchmnt_50k+TrunkCodes-Filtered-Merged_region")
# Transform to BNG
catchment_outlines_BNG <- spTransform(catchment_outlines, 
                                      CRS(britishNationalGrid))


# Match sites to catchments -----------------------------------------------

# Identify the catchment each site is located within
sites_within_catchment_id <- gWithin(sites_unique,
                                     catchment_outlines_BNG, 
                                     byid = TRUE, 
                                     returnDense = FALSE)
# Identify sites missing catchments
sites_within_catchment_id[sapply(sites_within_catchment_id, is.null)] <- NA
# Unlist
sites_within_catchment_id <- unlist(sites_within_catchment_id)
# Get sites without a catchment
sites_without_catchment_id <- sites_unique[is.na(sites_within_catchment_id),]
sites_without_catchment_logical <- is.na(sites_within_catchment_id)
# Remove sites without a catchment
sites_within_catchment_id <- sites_within_catchment_id[!is.na(sites_within_catchment_id)]

# Create a table of site to catchment relationships ----------------------------

sites_with_catchment <- cbind(sites_unique@data[!sites_without_catchment_logical,],
                              sites_unique@coords[!sites_without_catchment_logical,],
                              catchment_outlines_BNG@data[sites_within_catchment_id,
                                                          c('ID','FEATURE','NAME','AREA_HA','TRUNK_CODE')])

# Create a table of sites without catchments -----------------------------------

sites_without_catchment <- (nrow(sites_with_catchment)+1):(nrow(sites_with_catchment)+sum(sites_without_catchment_logical))

# Add details

if (length(sites_without_catchment) != 0) {
  sites_without_catchment <- cbind(sites_unique@data[sites_without_catchment_logical,],
                                   sites_unique@coords[sites_without_catchment_logical,])
}

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
# Save the catchment site relationships
write.csv(sites_with_catchment, file = sites_with_catchment_filename)
# Save a list of sites which were not included within any catchment
write.csv(sites_without_catchment, file = sites_without_catchment_filename)