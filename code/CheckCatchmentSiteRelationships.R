# Define the British National Grid Referencing System, using Proj4 notation
britishNationalGrid = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'

# Open the section 30 records, and the record of movements between farms
section30Movements.CSV <- read.csv(S30LFM.fileName,colClasses="character")
farmMovements.CSV <- read.csv(Farm2FarmLFM.fileName,colClasses="character")

# Create a list of each site, as well as each site's location, including duplicates
siteID = c(section30Movements.CSV[,c('SourceSiteID')],farmMovements.CSV[,c('ScrSiteID')],
           section30Movements.CSV[,c('ReceivingSiteID')],farmMovements.CSV[,c('RecSiteID')])

easting = as.integer(c(section30Movements.CSV[,c('SourceEasting')],farmMovements.CSV[,c('ScrEasting')],
                       section30Movements.CSV[,c('RecEasting')],farmMovements.CSV[,c('RecEasting')]))

northing = as.integer(c(section30Movements.CSV[,c('SourceNorthing')],farmMovements.CSV[,c('ScrNorthing')],
                        section30Movements.CSV[,c('RecNorthing')],farmMovements.CSV[,c('RecNorthing')]))

# Create a list of sites, and exclude duplicates, as well as sites where the location is undefined
listSites = data.frame(siteID,easting,northing,
                       stringsAsFactors = FALSE)

listSites.unique = ddply(listSites,
                         ~siteID + easting + northing,
                         summarise,
                         noOccurances = length(siteID))

identifyDuplicates = ddply(listSites.unique,
                           ~siteID,
                           summarise,
                           frequency = length(siteID))

print(c("Number of sites with no location: ",
        nrow(subset(listSites.unique, is.na(listSites.unique$easting)))))

listSites.unique = subset(listSites.unique, !is.na(listSites.unique$easting))

# Assign the correct spatial projection to the table
coordinates(listSites.unique) = c('easting','northing')
proj4string(listSites.unique) = CRS(britishNationalGrid)

# Open the polygon file, defining each catchment in England and Wales
catchment.Outlines = readOGR(dsn=catchmentLayer.fileName, 
                             layer = "catchmnt_50k+TrunkCodes-Filtered-Merged_region")
catchment.Outlines.Transformed = spTransform(catchment.Outlines, CRS(britishNationalGrid))

# Identify the catchment each site is located within, or, where applicable, specify if a site is not located within any catchment
sitesWithinCatchments.catchmentID = gWithin(listSites.unique,catchment.Outlines.Transformed, byid = TRUE, returnDense = FALSE)
sitesWithinCatchments.catchmentID[sapply(sitesWithinCatchments.catchmentID, is.null)] = NA
sitesWithinCatchments.catchmentID = unlist(sitesWithinCatchments.catchmentID)
sitesNotFoundWithinCatchments.byID = listSites.unique[is.na(sitesWithinCatchments.catchmentID),]
sitesNotFoundWithinCatchments.logical = is.na(sitesWithinCatchments.catchmentID)
sitesWithinCatchments.catchmentID.removeNAs = sitesWithinCatchments.catchmentID[!is.na(sitesWithinCatchments.catchmentID)]

# Create a table of site to catchment relationships
listSites.withCatchmentDetails = 
  cbind(listSites.unique@data[!sitesNotFoundWithinCatchments.logical,],
        listSites.unique@coords[!sitesNotFoundWithinCatchments.logical,],
        catchment.Outlines.Transformed@data[sitesWithinCatchments.catchmentID.removeNAs,c('ID','FEATURE','NAME','AREA_HA','TRUNK_CODE')])

# Create a table of those sites which were not found to be located within any catchment
sitesWithNoCatchment = (nrow(listSites.withCatchmentDetails)+1):(nrow(listSites.withCatchmentDetails)+sum(sitesNotFoundWithinCatchments.logical))

if (length(sitesWithNoCatchment) != 0) {
  listSites.withNoCatchmentDetails = cbind(listSites.unique@data[sitesNotFoundWithinCatchments.logical,],
                                           listSites.unique@coords[sitesNotFoundWithinCatchments.logical,])
}

# Incorperate information on whether site feed river water into it's facilities

riverSource = cbind(farmMovements.CSV[,c('ScrSiteID','Scr_RiverSourceUsedSince2010')],farmMovements.CSV[,c('RecSiteID','Rec_RiverSourceUsedSince2010')])
colnames(riverSource) = c("SiteID.Source","RiverSourceUsedSince2010.Source","SiteID.Dest","RiverSourceUsedSince2010.Dest")

riverSource.long = data.frame(RiverSourceUsedSince2010 = unlist(riverSource[,c("RiverSourceUsedSince2010.Source","RiverSourceUsedSince2010.Dest")]),
                                  siteID = unlist(riverSource[,c("SiteID.Source","SiteID.Dest")]),
                                  stringsAsFactors = FALSE,
                                  row.names = NULL)

riverSource.unique = unique(riverSource.long)
       
listSites.withCatchmentDetails = merge(x = listSites.withCatchmentDetails,
                                       y = riverSource.unique,
                                       by = c("siteID"),
                                       all.x = TRUE)

                                                          

# Save the catchment site relationships
write.csv(listSites.withCatchmentDetails, file = siteLocationsWithCatchment.fileName)

# Save a list of sites which were not included within any catchment
write.csv(listSites.withNoCatchmentDetails, file = siteLocationsWithoutCatchment.fileName)
