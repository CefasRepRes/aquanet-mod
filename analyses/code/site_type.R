#### Assign economics via site type ####

library(here)
library(dplyr)

# Load outputs -----------------------------------------------------------------

filenames <- list.files(here::here("outputs",
                                   "full_economics",
                                   "batch_results"),
                        pattern = "states-*")

sim_states <- data.frame()
for(i in 1:length(filenames)){
  load(here::here("outputs",
                  "full_economics",
                  "batch_results",
                  filenames[i]))
  sim_states <- rbind(sim_states, simStates.longTable)}

sim_time <- data.frame()
for(i in 1:length(filenames)){
  load(here::here("outputs",
                  "full_economics",
                  "batch_results",
                  filenames[i]))
  sim_time <- rbind(sim_time, simTimes.longTable)}

# Summarise outputs ------------------------------------------------------------

# Combine states and time spent in each state
sites_summary <- dplyr::left_join(sim_states, sim_time, by = c("timeID",
                                                               "simNo"))

# Select relevant columns
sites_summary <- dplyr::select(sites_summary,
                               siteID,
                               state,
                               tdiff,
                               simNo)

threes <- filter(sites_summary, state %in% c(3, 13, 23, 33))

# Filter out initialization (tdiff = NA)
sites_summary <- dplyr::filter(sites_summary, !is.na(tdiff))
str(sites_summary)

# Get actual site id

site_id_mod <- read.csv(here::here("outputs",
                                   "full_economics",
                                   "site_details_with_model_id.csv"))

site_id_mod <- dplyr::select(site_id_mod, siteID, modelID)

sites_summary <- left_join(sites_summary, site_id_mod, by = "modelID")
sites_summary <- dplyr::select(sites_summary, -modelID)

# Get site type ----------------------------------------------------------------

# Read in site type vector
site_type <- read.csv(here::here("outputs",
                                 "test",
                                 "site_types.csv"))

# Join to summary 
sites_summary_type <- dplyr::left_join(sites_summary, site_type, 
                                       by = c("siteID" = "site_code"))

# Filter out non-farms
sites_summary_type <- filter(sites_summary_type, farm_vector == 1)

# Load in economic costing -----------------------------------------------------

cull_cost <- read.csv(here::here("data",
                                 "Economics",
                                 "cull_costs.csv"))

daily_cost <- read.csv(here::here("data",
                                  "Economics",
                                  "daily_costs.csv"))

# Filter by state --------------------------------------------------------------
  # TODO: Expand to other states

source(here::here("analyses",
                  "code",
                  "fun_state_costs.R"))

site_types <- cull_cost$site_type


## Fallow ======================================================================

fallow_costs <- state.costs(data = sites_summary_type,
                            state = "fallow",
                            state_codes = c(4, 14, 24, 34),
                            site_types = site_types)

## No management ===============================================================
  # TODO: find out if this should only exist in the no controls scenario
no_manage_costs <- state.costs(data = sites_summary_type,
                               state = "no_manage",
                               state_codes = 10,
                               site_types = site_types)

## Contact tracing =============================================================

contact_trace_cost <- state.costs(data = sites_summary_type,
                                  state = "contact_trace",
                                  state_codes = c(1, 11, 21, 31,
                                                 7, 17, 27, 37),
                                  site_types = site_types)

## Catchment controls ==========================================================

catchment_controls <- state.costs(data = sites_summary_type,
                                  state = "catchment_control",
                                  state_codes = c(20:29),
                                  site_types = site_types)

# Add in cull costs ------------------------------------------------------------

fallow <- dplyr::filter(sites_summary_type, state %in% c(4, 14, 24, 34))
sims <- unique(sites_summary_type$simNo)

full_cull_cost <- data.frame()
for(k in 1:length(sims)){
for(i in 1:length(site_types)){
  type <- site_types[[i]]
  fallow_by_sim <- dplyr::filter(fallow, simNo == k)
  filter_cull_cost <- dplyr::filter(cull_cost, site_type == type)
  farm_cull_cost <- sum(fallow_by_sim[, type]) * filter_cull_cost$cull_cost_farm
  fhi_cull_cost <- sum(fallow_by_sim[, type]) * filter_cull_cost$cull_cost_fhi
  comb_cull_cost <- data.frame(cull_cost_farm = farm_cull_cost,
                               cull_cost_fhi = fhi_cull_cost)
  full_cull_cost <- rbind(full_cull_cost, comb_cull_cost)
}}

# Make into data frame
cull_costs <- data.frame(stage = c("cull_farm", "cull_fhi"),
                         mean_cost = c(mean(full_cull_cost$cull_cost_farm, na.rm = T), 
                                       mean(full_cull_cost$cull_cost_fhi, na.rm = T)),
                         sd_cost = c(sd(full_cull_cost$cull_cost_farm, na.rm = T), 
                                     sd(full_cull_cost$cull_cost_fhi, na.rm = T)),
                         median_cost = c(median(full_cull_cost$cull_cost_farm, na.rm = T), 
                                         median(full_cull_cost$cull_cost_fhi, na.rm = T)),
                         min_cost = c(min(full_cull_cost$cull_cost_farm, na.rm = T), 
                                      min(full_cull_cost$cull_cost_fhi, na.rm = T)),
                         max_cost = c(max(full_cull_cost$cull_cost_farm, na.rm = T), 
                                      max(full_cull_cost$cull_cost_fhi, na.rm = T)))

# Combine all ------------------------------------------------------------------
# TODO: get the total average properly

outbreak_costs <- rbind(fallow_costs,
                        no_manage_costs,
                        catchment_controls,
                        contact_trace_cost,
                        cull_costs)
