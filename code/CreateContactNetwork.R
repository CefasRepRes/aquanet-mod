#### Create contact network ####

# Run sub-scripts ---------------------------------------------------------

#source('code/CheckCatchmentSiteRelationships.R') # Don't need to run this if you have no duplicates file already
source('code/importSiteData.R')

# Save completed network 

write.graph(combined_movements_simplified, 
            file = contact_network_filename, 
            format = "graphml")
