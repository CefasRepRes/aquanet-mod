#### Scenario analyses ####

library(aquanet)

# Baseline ---------------------------------------------------------------------

# Load summary
  # May get over allocation warning - you can ignore
baseline <- aquanet::loadResultsSummary("test_data_results/baseline")

# Calculate epidemic duration
baseline_duration <- aquanet::epidemicDuration(baseline, summary = T)
print(baseline_duration)

# Calculate epidemic size
baseline_size <- aquanet::epidemicSize(baseline, summary = T)
print(baseline_size)

# No catchment controls --------------------------------------------------------

# Load summary
  # May get over allocation warning - you can ignore
no_catchment_controls <- aquanet::loadResultsSummary("test_data_results/no_catchment_controls")

# Calculate epidemic duration
no_catchment_controls_duration <- aquanet::epidemicDuration(no_catchment_controls, summary = T)
print(no_catchment_controls_duration)

# Calculate epidemic size
no_catchment_controls_size <- aquanet::epidemicSize(no_catchment_controls, summary = T)
print(no_catchment_controls_size)
