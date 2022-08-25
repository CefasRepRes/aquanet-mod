#### Assign economics via site type ####

library(here)
library(dplyr)
library(beepr)
library(data.table)
library(aquanet)

# Load and process outputs -----------------------------------------------------

loadResultsFullSiteType("test")

# Process results - this will take a little while
time_summary <- timePerStage("test");beep()

# Load in economic costing -----------------------------------------------------

cull_cost <- read.csv(here::here("data",
                                 "Economics",
                                 "cull_costs.csv"))

daily_cost <- read.csv(here::here("data",
                                  "Economics",
                                  "daily_costs.csv"))

# Calculate daily costs --------------------------------------------------------

# Filter out non-farms
# We don't have any duration cost data for fisheries
time_summary_farms <- dplyr::filter(time_summary, farm_vector == 1)
max(time_summary_farms$sim_no)

# Get a list of site types
site_types <- cull_cost$site_type

# Fallow
fallow_costs <- stateCosts(data = time_summary_farms,
                           state = "fallow",
                           site_types = site_types)

# No management
no_manage_costs <- stateCosts(data = time_summary_farms,
                              state = "no_manage",
                              site_types = site_types)

# Contact tracing
contact_trace_cost <- stateCosts(data = time_summary_farms,
                                 state = "contact_trace",
                                 site_types = site_types)

# Catchment controls
catchment_controls <- stateCosts(data = time_summary_farms,
                                 state = "catchment_control",
                                 site_types = site_types)

 # Combine into single daily cost data frame
simulation_daily_costs <- fallow_costs %>% 
  dplyr::full_join(no_manage_costs, by = "sim_no") %>% 
  dplyr::full_join(contact_trace_cost, by = "sim_no") %>%
  dplyr::full_join(catchment_controls, by = "sim_no")

# Add in cull costs ------------------------------------------------------------

# Load function
source(here::here("functions",
                  "cull.cost.R"))

full_cull_cost_sim <- cull.cost(data = time_summary,
                                cull_cost = cull_cost)

# Make into single data frame
full_outbreak_costs <- simulation_daily_costs %>% dplyr::full_join(full_cull_cost_sim,
                                                                   by = c("simNo" = "sim"))

# Calculate total outbreak cost
full_outbreak_costs$total_outbreak_cost <- rowSums(full_outbreak_costs[, -1],
                                                   na.rm = T)
# Save
write.csv(full_outbreak_costs,
          here::here("outputs",
                     "full_run_for_economics",
                     "economics",
                     "full_outbreak_costs.csv"),
          row.names = F)

# Get summary statistics -------------------------------------------------------

# Loop over functions
functions <- c("mean", "sd", "min", "max")
outbreak_summary <- data.frame()
for(i in 1:length(functions)){
  sum <- apply(full_outbreak_costs, 2, functions[i], na.rm = T)
  outbreak_summary <- rbind(outbreak_summary, sum)
}

# Tidy up data frame
outbreak_summary <- as.data.frame(t(outbreak_summary))
colnames(outbreak_summary) <- functions
rownames(outbreak_summary) <- 1:nrow(outbreak_summary)
outbreak_summary$cost_component <- colnames(full_outbreak_costs)
outbreak_summary <- outbreak_summary[-1, ] # Remove simNo

# Save
write.csv(outbreak_summary,
          here::here("outputs",
                     "full_run_for_economics",
                     "economics",
                     "summary_outbreak_costs.csv"),
          row.names = T)
