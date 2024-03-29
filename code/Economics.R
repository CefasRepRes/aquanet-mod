#### Assign economics via site type ####

library(here)
library(magrittr)
library(aquanet)
library(arrow)
library(data.table)
library(dplyr)

# define scenario name to analyse
scenario_name <- "baseline"

# Create economics folder ------------------------------------------------------

economics_dir <- here::here("outputs", scenario_name, 
                            "economics")
if (!dir.exists(economics_dir)) {
  dir.create(economics_dir)
}

# Epidemic size, duration and peak ---------------------------------------------

## Load summary ================================================================

# May get over allocation warning - you can ignore
batch_res <- aquanet::loadResultsSummary(scenario_name)

## Duration ====================================================================

# Summary
duration_summary <- aquanet::epidemicDuration(batch_res, summary = T)
write.csv(duration_summary, paste0(economics_dir, "/", scenario_name, "_duration_summary.csv"),
          row.names = F)

# Full
duration_full <- aquanet::epidemicDuration(batch_res, summary = F)
write.csv(duration_full, paste0(economics_dir, "/", scenario_name, "_duration_full.csv"),
          row.names = F)

## Size ====================================================================

# Summary
size_summary <- aquanet::epidemicSize(batch_res, summary = T)
write.csv(size_summary, paste0(economics_dir, "/", scenario_name, "_size_summary.csv"),
          row.names = F)

# Full
size_full <- aquanet::epidemicSize(batch_res, summary = F)
write.csv(size_full, paste0(economics_dir, "/", scenario_name, "_size_full.csv"),
          row.names = F)

## Peak ========================================================================

peak <- aquanet::epidemicPeak(batch_res)
write.csv(peak, paste0(economics_dir, "/", scenario_name, "_peak.csv"),
          row.names = F)

# Load and process outputs -----------------------------------------------------

# # Load and process results
if(file.exists(paste0(economics_dir, "/", scenario_name, "-details-condensed.parquet"))){
  time_summary <- read_parquet(paste0(economics_dir, "/", scenario_name, "-details-condensed.parquet"))
} else {
  time_summary <- aquanet::importAndCondense(scenario_name = scenario_name)
}

# Check mechanisms of transmission ---------------------------------------------

trans_mech <- aquanet::transmissionRouteProportions(scenario_name)
write.csv(trans_mech, paste0(economics_dir, "/", scenario_name, "_trans_mech.csv"),
          row.names = F)

# Load in economic costing -----------------------------------------------------

cull_cost <- read.csv(here::here("data",
                                 "Economics",
                                 "cull_costs.csv"))

daily_cost <- read.csv(here::here("data",
                                  "Economics",
                                  "daily_costs.csv"))

ca_cost <- read.csv(here::here("data",
                               "Economics",
                               "ca_costs.csv"))

# Calculate daily costs --------------------------------------------------------

# Filter out non-farms
# We don't have any duration cost data for fisheries
time_summary_farms <- time_summary[farm_vector == 1]

# Filter out fisheries
# To do cull cost
time_summary_non_farms <- time_summary[farm_vector == 0]

# Get a list of site types
site_types <- cull_cost$site_type

# Fallow
fallow_costs <- aquanet::stateCosts(data = time_summary_farms,
                                    state = "fallow",
                                    site_types = site_types)

# No management
no_manage_costs <- aquanet::stateCosts(data = time_summary_farms,
                                       state = "no_manage",
                                       site_types = site_types)

# Contact tracing
contact_trace_cost <- aquanet::stateCosts(data = time_summary_farms,
                                          state = "contact_trace",
                                          site_types = site_types)

# Catchment controls
catchment_controls <- aquanet::stateCosts(data = time_summary_farms,
                                          state = "catchment_control",
                                          site_types = site_types)

# Combine into single daily cost data frame
summary_simulation_daily_costs <- fallow_costs[["summary_state_costs"]] %>% 
  merge(no_manage_costs[["summary_state_costs"]], all = TRUE, by = "sim_no") %>%
  merge(contact_trace_cost[["summary_state_costs"]], all = TRUE, by = "sim_no") %>%
  merge(catchment_controls[["summary_state_costs"]], all = TRUE, by = "sim_no")

full_simulation_daily_costs <- fallow_costs[["full_state_costs"]] %>% 
  merge(no_manage_costs[["full_state_costs"]], all = TRUE, by = c("sim_no", "site_types")) %>%
  merge(contact_trace_cost[["full_state_costs"]], all = TRUE, by = c("sim_no", "site_types")) %>%
  merge(catchment_controls[["full_state_costs"]], all = TRUE, by = c("sim_no", "site_types"))

# Add in cull costs ------------------------------------------------------------

full_cull_cost_sim <- aquanet::cullCost(farm_data = time_summary_farms,
                                        non_farm_data = time_summary_non_farms,
                                        cull_cost = cull_cost,
                                        site_types = site_types)

cull_cost_farm_type <- full_cull_cost_sim[["cull_cost_farm_by_type"]]
full_cull_cost_sim <- full_cull_cost_sim[["cull_cost_by_sim"]]

# Combine cost breakdown by site type ------------------------------------------

# Join together
full_cost_by_type <- full_simulation_daily_costs %>%
  merge(cull_cost_farm_type, all = TRUE, by = c("sim_no", "site_types"))

# Remove durations
remove <- c("fallow_total_duration",
            "no_manage_total_duration",
            "contact_trace_total_duration",
            "catchment_control_total_duration")
full_cost_by_type <- full_cost_by_type[, !..remove]

# Replace NAs with 0s
full_cost_by_type[is.na(full_cost_by_type)] <- 0

# Save
write.csv(full_cost_by_type, paste0(economics_dir, "/", scenario_name, "_cost_by_site_type.csv"),
          row.names = F)

# Add in CA costs -------------------------------------------------------------

## Contact sampling ============================================================

if(!(scenario_name %like% "no_controls" |
     scenario_name %like% "no_contact_tracing")){
  ca_cost <- data.table(ca_cost)
  contact_sampling_cost <- dplyr::count(time_summary[trans_type == 12], # trans_type = 12 -> contact detection
                                        by = sim_no) 
  contact_sampling_cost$n <- contact_sampling_cost$n * ca_cost[cost_type == "site_contact_sampling"]$cost
  data.table::setnames(contact_sampling_cost, old = "by", new = "sim_no")
  data.table::setnames(contact_sampling_cost, old = "n", new = "ca_contact_sampling")
} else {
  contact_sampling_cost <- data.frame(sim_no = 1:3000,
                                      ca_contact_sampling = 0)
}

## Catchment costs =============================================================

if(!(scenario_name %like% "no_controls" |
     scenario_name %like% "no_catchment_controls")){
  batch_res <- data.table(batch_res)
  ca_cost <- data.table(ca_cost)
  batch_res <- batch_res[sim_no != 0]
  catchment_cost <- batch_res[, .(ca_catchment = max(no_controlled_catchments)), 
                              by = "sim_no"] 
  catchment_cost$ca_catchment <- catchment_cost$ca_catchment * ca_cost[cost_type == "catchment"]$cost
} else {
  catchment_cost <- data.frame(sim_no = 1:3000,
                               ca_catchment = 0)
}

## Ancillary costs =============================================================

if(!(scenario_name %like% "no_controls")){
  ancillary_cost <- data.frame(sim_no = 1:3000,
                               ca_ancilliary = ca_cost[cost_type == "outbreak_ancillary"]$cost)
} else {
  ancillary_cost <- data.frame(sim_no = 1:3000,
                               ca_ancilliary = 0)
}

## Join together ===============================================================

ca_non_cull_costs <- contact_sampling_cost %>%
  merge(catchment_cost, all = TRUE, by = "sim_no") %>%
  merge(ancillary_cost, all = TRUE, by = "sim_no")

# Combine all costs ------------------------------------------------------------

# Make into single data frame
full_epidemic_costs <- summary_simulation_daily_costs %>% 
  merge(full_cull_cost_sim, all = TRUE, by = "sim_no") %>%
  merge(ca_non_cull_costs, all = TRUE, by = "sim_no")

# Calculate total epidemic cost
full_epidemic_costs$total_epidemic_cost <- rowSums(full_epidemic_costs[, -c("sim_no",
                                                                            "total_cull_cost")],
                                                   na.rm = T)
# Replace NA with 0
# Important to get correct stats
all_sims <- data.frame(sim_no = 1:3000)
full_epidemic_costs <- merge(all_sims,
                             full_epidemic_costs,
                             all = TRUE)
full_epidemic_costs <- data.table(full_epidemic_costs)
missing_sims <- full_epidemic_costs[is.na(total_epidemic_cost)]
missing_sims <- missing_sims[!(sim_no %in% time_summary$sim_no)]
write.csv(missing_sims,
          here::here("outputs",
                     scenario_name,
                     "economics",
                     "missing_simulations.csv"),
          row.names = T)
full_epidemic_costs[is.na(full_epidemic_costs)] <- 0

# Save
write.csv(full_epidemic_costs,
          here::here("outputs",
                     scenario_name,
                     "economics",
                     "full_epidemic_costs.csv"),
          row.names = F)

# Get summary statistics -------------------------------------------------------

# Loop over functions
functions <- function(x){
  c(mean = mean(x, na.rm = T),
    sd = sd(x, na.rm = T),
    median = median(x, na.rm = T),
    q05 = quantile(x, 0.05, na.rm = T),
    q95 = quantile(x, 0.95, na.rm = T))
}

epidemic_summary <- sapply(full_epidemic_costs, functions)

# Tidy up data frame
epidemic_summary <- as.data.frame(t(epidemic_summary))
colnames(epidemic_summary) <- c("mean", "sd", "median", "q05", "q95")
rownames(epidemic_summary) <- 1:nrow(epidemic_summary)
epidemic_summary$cost_component <- colnames(full_epidemic_costs)
epidemic_summary <- epidemic_summary[-1, ] # Remove simNo

# Save
write.csv(epidemic_summary,
          here::here("outputs",
                     scenario_name,
                     "economics",
                     "summary_epidemic_costs.csv"),
          row.names = T)
