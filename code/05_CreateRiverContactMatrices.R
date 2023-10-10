#### Create river contact matrices ####

# Get downstream transmission probability matrix -------------------------------

river_distances_df <- 
  aquanet::loadRiverDistances(filepath_river_distances = river_downstream_filename,
                              out_createContactProbabilityMatrix = contact_probability_matrix)

river_downstream_transmission_matrix <- 
  aquanet::createRiverDistanceProbabilityMatrix(
    river_distances = river_distances_df,
    out_createContactProbabilityMatrix = contact_probability_matrix,
    max_dist = model_parameters$Max_Distance_River_Transmission,
    p1km = model_parameters$Probability_River_Transmission)
