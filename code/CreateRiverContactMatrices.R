#### Create river contact matrices ####

# Load packages ----------------------------------------------------------------

library(aquanet)

# Get downstream transmission probability matrix -------------------------------

river_downstream_filename <- here::here("data",
                                        "CalcRiverDistance",
                                        Species,
                                        "RoutesDownstreamTable.csv")

river_downstream_transmission_matrix <- aquanet::createRiverDistanceProbabilityMatrix(river_downstream_filename,
                                                                                     contact_probability_matrix)


river_downstream_transmission_objects <- list(river_downstream_transmission_matrix, 
                                              NULL)

# Get fomite transmission probability matrix -----------------------------------

fomite_transmission_matrix <- site_distances_matrix

fomite_transmission_objects <- list(fomite_transmission_matrix)
