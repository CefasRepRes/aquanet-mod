#### Assign economics via site type ####

library(here)
library(dplyr)
library(beepr)
library(data.table)

# Load and process outputs -----------------------------------------------------

# Source function
source(here::here("functions",
                  "time.per.stage.R"))

# Process results - this will take a little while
time_summary <- time.per.stage("full_run_for_economics");beep()

# Load results
load(here::here("outputs",
                "full_run_for_economics",
                "economics",
                "full_run_for_economics-site-time-summary.Rdata"))
time_summary <- sims_site_time_summary

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
max(time_summary_farms$simNo)

# Load function
source(here::here("functions",
                  "state.costs.R"))

# Get a list of site types
site_types <- cull_cost$site_type

# Fallow
fallow_costs <- state.costs(data = time_summary_farms,
                            state = "fallow",
                            state_codes = c(4, 14, 24, 34),
                            site_types = site_types)

# No management
  # TODO: find out if this should only exist in the no controls scenario
no_manage_costs <- state.costs(data = time_summary_farms,
                               state = "no_manage",
                               state_codes = 10,
                               site_types = site_types)

# Contact tracing
contact_trace_cost <- state.costs(data = time_summary_farms,
                                  state = "contact_trace",
                                  state_codes = c(1, 11, 21, 31,
                                                 7, 17, 27, 37),
                                  site_types = site_types)

# Catchment controls
catchment_controls <- state.costs(data = time_summary_farms,
                                  state = "catchment_control",
                                  state_codes = c(20, 21, 22, 23, 24,
                                                  25, 26, 27, 28, 29),
                                  site_types = site_types)

# Combine into single daily cost data frame
simulation_daily_costs <- fallow_costs %>% 
  dplyr::full_join(no_manage_costs, by = "simNo") %>% 
  dplyr::full_join(contact_trace_cost, by = "simNo") %>%
  dplyr::full_join(catchment_controls, by = "simNo")

# Add in cull costs ------------------------------------------------------------

# When a site enters a fallow state (34) enter a one-off cost
fallow <- dplyr::filter(time_summary, state %in% c(14, 34))
sims <- unique(fallow$simNo)

# Loop over simulations and add the one-off fallow cost, depending on site type
full_cull_cost <- data.table()
system.time({
for(k in 1:length(sims)){
for(i in 1:length(site_types)){
  type <- site_types[[i]]
  fallow <- data.table(fallow)
  fallow_by_sim <- fallow[simNo == sims[k]]
  #filter_cull_cost <- dplyr::filter(cull_cost, site_type == type)
  cull_cost <- data.table(cull_cost)
  filter_cull_cost <- cull_cost[site_type == type]
  # Separate costs to the farm and the FHI (competent authority)
  farm_cull_cost <- sum(fallow_by_sim[, ..type]) * filter_cull_cost$cull_cost_farm
  fhi_cull_cost <- sum(fallow_by_sim[, ..type]) * filter_cull_cost$cull_cost_fhi
  # Combine into single data frame
  comb_cull_cost <- data.table(cull_cost_farm = farm_cull_cost,
                               cull_cost_fhi = fhi_cull_cost,
                               sim = k)
  # Save results
  full_cull_cost <- rbind(full_cull_cost, comb_cull_cost)
}
  print(sims[k])
  }})

# Summarise per simulation
full_cull_cost_sim <- full_cull_cost %>% 
  group_by(sim) %>% 
  summarise(cull_cost_farm = sum(cull_cost_farm, na.rm = T),
            cull_cost_fhi = sum(cull_cost_fhi, na.rm = T))

full_cull_cost_sim <- full_cull_cost[, by = sim,
                                     .(total_cull_cost_farm = sum(cull_cost_farm, na.rm = T),
                                       total_cull_cost_fhi = sum(cull_cost_fhi, na.rm = T))]

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
rownames(outbreak_summary) <- colnames(full_outbreak_costs)
outbreak_summary <- outbreak_summary[-1, ] # Remove simNo

# Save
write.csv(outbreak_summary,
          here::here("outputs",
                     "full_run_for_economics",
                     "economics",
                     "summary_outbreak_costs.csv"),
          row.names = T)
