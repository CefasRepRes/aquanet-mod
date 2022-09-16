#### Assign economics via site type ####

library(here)
library(dplyr)
library(beepr)
library(data.table)
library(aquanet)

# Select scenario name
scenario_name <- "no_contact_tracing"

# Load and process outputs -----------------------------------------------------

# Load results
full_results <- loadResultsFullSiteType(scenario_name)

# Process results - this will take a little while
time_summary <- timePerStage(scenario_name);beep()

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

# Filter out fisheries
  # To do cull cost
time_summary_non_farms <- dplyr::filter(time_summary, farm_vector == 0)

# Get a list of site types
site_types <- cull_cost$site_type

# Fallow
fallow_costs <- stateCosts(data = time_summary_farms,
                           state = "fallow",
                           site_types = site_types);beep()

# No management
no_manage_costs <- stateCosts(data = time_summary_farms,
                              state = "no_manage",
                              site_types = site_types);beep()

# Contact tracing
contact_trace_cost <- stateCosts(data = time_summary_farms,
                                 state = "contact_trace",
                                 site_types = site_types);beep()

# Catchment controls
catchment_controls <- stateCosts(data = time_summary_farms,
                                 state = "catchment_control",
                                 site_types = site_types);beep()

 # Combine into single daily cost data frame
simulation_daily_costs <- fallow_costs[["summary_state_costs"]] %>% 
  dplyr::full_join(no_manage_costs[["summary_state_costs"]], by = "sim_no") %>% 
  dplyr::full_join(contact_trace_cost[["summary_state_costs"]], by = "sim_no") %>%
  dplyr::full_join(catchment_controls[["summary_state_costs"]], by = "sim_no")

# Add in cull costs ------------------------------------------------------------

full_cull_cost_sim <- cullCost(farm_data = time_summary_farms,
                               non_farm_data = time_summary_non_farms,
                               cull_cost = cull_cost,
                               site_types = site_types);beep()

# Make into single data frame
full_outbreak_costs <- simulation_daily_costs %>% dplyr::full_join(full_cull_cost_sim,
                                                                   by = "sim_no")

# Calculate total outbreak cost
full_outbreak_costs$total_outbreak_cost <- rowSums(full_outbreak_costs[, -1],
                                                   na.rm = T)
# Replace NA with 0
  # Important to get correct stats
all_sims <- data.frame(sim_no = 1:3000)
full_outbreak_costs <- merge(all_sims,
                             full_outbreak_costs,
                             all = TRUE)
missing_sims <- dplyr::filter(full_outbreak_costs, is.na(total_outbreak_cost))
missing_sims <- missing_sims %>% dplyr::filter(!(sim_no %in% full_results$sim_no))
write.csv(missing_sims,
          here::here("outputs",
                     scenario_name,
                     "economics",
                     "missing_simulations.csv"),
          row.names = T)
full_outbreak_costs[is.na(full_outbreak_costs)] <- 0

# Save
write.csv(full_outbreak_costs,
          here::here("outputs",
                     scenario_name,
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
                     scenario_name,
                     "economics",
                     "summary_outbreak_costs.csv"),
          row.names = T)
