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

peak <- aquanet::epidemicPeak(batch_res, summary = T)
write.csv(peak, paste0(economics_dir, "/", scenario_name, "_peak.csv"),
          row.names = F)

# Full
peak_full <- aquanet::epidemicPeak(batch_res, summary = F)
write.csv(peak_full, paste0(economics_dir, "/", scenario_name, "_peak_full.csv"),
          row.names = F)

# Load and process outputs -----------------------------------------------------

# # Load and process results- using importAndCondense
if(file.exists(paste0(economics_dir, "/", scenario_name, "-details-condensed.parquet"))){
  time_summary <- read_parquet(paste0(economics_dir, "/", scenario_name, "-details-condensed.parquet"))
} else {
  time_summary <- aquanet::importAndCondense(scenario_name = scenario_name)
}

## Outputs for the app =========================================================

# Saves in outputs directory 
aggregateScenerioOutputs(scenario_name)
#SizeDurFullOutput(scenario_name)
#SizeDurPeakSummary(scenario_name)

# Check mechanisms of transmission ---------------------------------------------

trans_mech <- aquanet::transmissionRouteProportions(scenario_name)
write.csv(trans_mech, paste0(economics_dir, "/", scenario_name, "_trans_mech.csv"),
          row.names = F)

# Load in economic costing -----------------------------------------------------

cull_cost <- read.csv(here::here("analyses",
                                 "Economics",
                                 "FC1216",
                                 "cull_costs.csv"))

unit_cost <- read.csv(here::here("analyses",
                                 "Economics",
                                 "FC1216",
                                 "unit_costs.csv"))

# ca cost= site_contact_sampling (Cost per farm visit, code multiplies this number per contact detection (trans_type =12)),
# outbreak_ancillary (Incurred once per outbreak, to all scenarios other than no controls), 
# catchment (To all scenarios other than ‘no controls’ and ‘no catchment controls’,Multiplies this number by the number of catchments under controls in that time step (created in loadResultsSummary).)

ca_cost <- read.csv(here::here("analyses",
                               "Economics",
                               "FC1216",
                               "ca_costs.csv"))


#########################################################################################
# Farm costs
#########################################################################################

# Calculate daily/monthly costs --------------------------------------------------------

# Filter out non-farms
# We don't have any duration cost data for fisheries

#create farm_vector column- with farm= 1 and fishery = 0
time_summary <- time_summary[, farm_vector := ifelse(Category == "Farm", 1, 0)]

# Change Y to 1 and N to 0 in site columns
catergory_columns <- c("SmallHatchery", "LargeHatchery", "SmallOngrower" , "MediumOngrower", "LargeOngrower","SmallRestocker","MediumRestocker" ,"LargeRestocker" , "SmallTable", "MediumTable", "LargeTable", "SmallFishery", "MediumFishery", "LargeFishery")  # Columns to modify

time_summary[, (catergory_columns) := lapply(.SD, function(x) ifelse(x == "Y", 1, 0)), 
             .SDcols = catergory_columns]

# Only select farms
time_summary_farms <- time_summary[farm_vector == 1]

# Filter out fisheries
# To do cull cost
#time_summary_non_farms <- time_summary[farm_vector == 0]

# Get a list of site types
site_types <- cull_cost$site_type

# StateCosts calculates time spent in each state for each site type for fallow, no mange, contact trace and catchment control per sim. This 'total_duration' 
# is then multiplied by corresponding site_type daily costs to get state_costs
# Fallow
fallow_costs <- aquanet::stateCosts(data = time_summary_farms,
                                    state = "fallow",
                                    site_types = site_types)

# No management
if(grepl("no_controls", scenario_name)){
  no_manage_costs <- aquanet::stateCosts(data = time_summary_farms,
                                         state = "no_manage",
                                         site_types = site_types)
  
} else {
  no_manage_costs <- data.table(sim_no = 1:3024,
                                no_manage_total_cost = 0)
  no_manage_costs <- list("summary_state_costs" = no_manage_costs)
}




# Removed as no longer have farm costs for farms
# Contact tracing
#contact_trace_cost <- aquanet::stateCosts(data = time_summary_farms,
# state = "contact_trace",
#site_types = site_types)

####################################### Change catchment controls to monthly cost #####################################################################
# Catchment controls
catchment_controls <- aquanet::stateCosts(data = time_summary_farms,
                                          state = "catchment_control",
                                          site_types = site_types)

# Combine into single daily cost data frame
summary_simulation_unit_costs <- fallow_costs[["summary_state_costs"]] %>% 
  merge(no_manage_costs[["summary_state_costs"]], all = TRUE, by = "sim_no") %>%
  #merge(contact_trace_cost[["summary_state_costs"]], all = TRUE, by = "sim_no") %>%  ## contact tracing cost no longer farm cost
  merge(catchment_controls[["summary_state_costs"]], all = TRUE, by = "sim_no")

full_simulation_unit_costs <- fallow_costs[["full_state_costs"]] %>% 
  # merge(no_manage_costs[["full_state_costs"]], all = TRUE, by = c("sim_no", "site_types")) %>%
  #merge(contact_trace_cost[["full_state_costs"]], all = TRUE, by = c("sim_no", "site_types")) %>%
  merge(catchment_controls[["full_state_costs"]], all = TRUE, by = c("sim_no", "site_types"))

# Add in cull costs ------------------------------------------------------------
# Change cull_state to TRUE=1 and FALSE = 0
time_summary_farms[, cull_state := ifelse(cull_state == "TRUE", 1, 0)]

full_cull_cost_sim <- cullCost(farm_data = time_summary_farms,
                               #non_farm_data = time_summary_non_farms,
                               cull_cost = cull_cost,
                               site_types = site_types) # less unique sim_no as only includes data where cull= TRUE

# Combine cost per simulation ------------------------------------------
# Join together
full_cost_by_sim <- summary_simulation_unit_costs %>%
  merge(full_cull_cost_sim, all = TRUE, by = c("sim_no"))

# Save
write.csv(full_cost_by_sim, paste0(economics_dir, "/", scenario_name, "_full_cost_by_sim.csv"),
          row.names = F)

# Combine cost breakdown by site type ------------------------------------------

# Join together
#full_cost_by_type <- full_simulation_unit_costs %>%
#merge(cull_cost_farm_type, all = TRUE, by = c("sim_no", "site_types")) #not going to join together anymore as cull cost not split by site type

# Remove durations
#remove <- c("fallow_total_duration",
#"no_manage_total_duration",
#"contact_trace_total_duration",
#"catchment_control_total_duration")
#full_cost_by_type <- full_cost_by_type[, !..remove]

# Replace NAs with 0s
#full_cost_by_type[is.na(full_cost_by_type)] <- 0

# Save
write.csv(full_simulation_unit_costs, paste0(economics_dir, "/", scenario_name, "_unit_cost_by_site_type.csv"),
          row.names = F) # TO DO: think of cull costs- can we do something with counting site type too? Difficult asonly choosing highest cost to cull per site type

#########################################################################################
# CA costs
#########################################################################################

## Contact sampling ============================================================

# this code counts the number of contact detection by sim number. Multiples by cost. In scenarios other than no controls and no_contact_tracing 
## NOTE: double check name of scenarios include these
## SA to check if this code successful picks up no controls and no contract tracing

if(!(grepl("no_controls", scenario_name) |
     grepl("no_contact_tracing", scenario_name))) {
  ca_cost <- data.table(ca_cost)
  contact_sampling_cost <- dplyr::count(time_summary[trans_type == 12], # trans_type = 12 -> contact detection
                                        by = sim_no) 
  contact_sampling_cost$n <- contact_sampling_cost$n * ca_cost[cost_type == "site_contact_sampling"]$cost # multiple the number of times trans_type = 12 by contact cost to CA
  data.table::setnames(contact_sampling_cost, old = "by", new = "sim_no")
  data.table::setnames(contact_sampling_cost, old = "n", new = "ca_contact_sampling")
} else {
  contact_sampling_cost <- data.frame(sim_no = 1:3024,
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
  catchment_cost <- data.frame(sim_no = 1:3024,
                               ca_catchment = 0)
}

## Ancillary costs =============================================================
# this one applies ancillary costs to all sims (every time there is an outbreak)- other than no controls scenerio
if(!(scenario_name %like% "no_controls")){
  ancillary_cost <- data.frame(sim_no = 1:3024,
                               ca_ancilliary = ca_cost[cost_type == "outbreak_ancillary"]$cost)
} else {
  ancillary_cost <- data.frame(sim_no = 1:3024,
                               ca_ancilliary = 0)
}

## Join together ===============================================================

ca_non_cull_costs <- contact_sampling_cost %>%
  merge(catchment_cost, all = TRUE, by = "sim_no") %>%
  merge(ancillary_cost, all = TRUE, by = "sim_no")

# Combine all costs ------------------------------------------------------------

# Make into single data frame
full_epidemic_costs <- summary_simulation_unit_costs %>% 
  merge(full_cull_cost_sim, all = TRUE, by = "sim_no") %>%
  merge(ca_non_cull_costs, all = TRUE, by = "sim_no")

# Calculate total epidemic cost
full_epidemic_costs$total_epidemic_cost <- rowSums(full_epidemic_costs[, -c("sim_no",
                                                                            "total_cull_cost")],
                                                   na.rm = T)
# Replace NA with 0
# Important to get correct stats
number_of_sims <- uniqueN(batch_res$sim_no)

all_sims <- data.frame(sim_no = 1:number_of_sims)
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
