#### Scenario analyses ####

library(aquanet)

# Baseline ---------------------------------------------------------------------

# Load summary
  # May get over allocation warning - you can ignore
baseline <- aquanet::loadResultsSummary("baseline")

# Calculate epidemic duration
baseline_duration <- aquanet::epidemicDuration(baseline)

# Calculate epidemic size
baseline_size <- aquanet::epidemicSize(baseline)

# No catchment controls --------------------------------------------------------

# Load summary
  # May get over allocation warning - you can ignore
no_catchment_controls <- aquanet::loadResultsSummary("no_catchment_controls")

# Calculate epidemic duration
no_catchment_controls_duration <- aquanet::epidemicDuration(no_catchment_controls)

# Calculate epidemic size
no_catchment_controls_size <- aquanet::epidemicSize(no_catchment_controls)
