#### Create river contact matrices ####

# Get downstream transmission probability matrix -------------------------------

river_downstream_transmission_matrix <- 
  aquanet::createRiverDistanceProbabilityMatrix(
    filepath_river_distances = river_downstream_filename,
    out_createContactProbabilityMatrix = contact_probability_matrix,
    max_dist = parameter_file$Max_Distance_River_Transmission,
    p1km = parameter_file$Probability_River_Transmission)


river_downstream_transmission_objects <- list(river_downstream_transmission_matrix, 
                                              NULL)

# Get fomite transmission probability matrix -----------------------------------

fomite_transmission_matrix <- site_distances_matrix

fomite_transmission_objects <- list(fomite_transmission_matrix)
