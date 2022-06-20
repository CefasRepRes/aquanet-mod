#### Assign economics via site type ####

library(here)
library(dplyr)

# Load outputs -----------------------------------------------------------------

filenames <- list.files(here::here("outputs",
                                   "test",
                                   "batch_results"),
                        pattern = "states-*")

sim_states <- data.frame()
for(i in 1:length(filenames)){
  load(here::here("outputs",
                  "test",
                  "batch_results",
                  filenames[i]))
  sim_states <- rbind(sim_states, simStates.longTable)}

sim_time <- data.frame()
for(i in 1:length(filenames)){
  load(here::here("outputs",
                  "test",
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

# Filter out initialization (tdiff = NA)
sites_summary <- dplyr::filter(sites_summary, !is.na(tdiff))

# Get site type ----------------------------------------------------------------

# Read in site type vector
site_type <- read.csv(here::here("outputs",
                                 "test",
                                 "site_types.csv"))

# Join to summary 
sites_summary_type <- dplyr::left_join(sites_summary, site_type, 
                                       by = c("siteID" = "site_code"))

# Filter out NAs
  # TODO: work out why we have these NAs. Fishery sites?
sites_summary_type <- filter(sites_summary_type, !is.na(smallhatch))

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

state.summary <- function(data){
  cbind(summarise(data, across(tdiff, sum)),
        summarise(data, across(-tdiff, first)))
}

## Fallow ======================================================================

# Select fallow states
fallow <- filter(sites_summary_type, state == c(4, 14, 24, 34))

# Summarise
fallow <- group_by(fallow, siteID) %>% state.summary()

# Allocate daily costs
fallow_daily_cost <- dplyr::filter(daily_cost, stage == "fallow")

# Get list of site types
site_types <- cull_cost$site_type

# Calculate daily costs for each site
costs <- data.frame()
for(i in 1:length(site_types)){
  type <- site_types[[i]]
  daily_site_cost <- dplyr::filter(fallow_daily_cost, site_type == type)
  duration_cost <- (fallow[, type] * fallow$tdiff) * daily_site_cost$farm_cost_per_day
  total_duration_cost <- sum(duration_cost)
  costs <- rbind(costs, total_duration_cost)
}

# Make into data frame
fallow_costs <- cbind(site_types, costs)
colnames(fallow_costs)[2] <- "duration_cost"

# Add in cull costs
full_cull_cost <- data.frame()
for(i in 1:length(site_types)){
  type <- site_types[[i]]
  filter_cull_cost <- dplyr::filter(cull_cost, site_type == type)
  farm_cull_cost <- sum(fallow[, type]) * filter_cull_cost$cull_cost_farm
  fhi_cull_cost <- sum(fallow[, type]) * filter_cull_cost$cull_cost_fhi
  comb_cull_cost <- data.frame(cull_cost_farm = farm_cull_cost,
                               cull_cost_fhi = fhi_cull_cost)
  full_cull_cost <- rbind(full_cull_cost, comb_cull_cost)
}

# Make into data frame
cull_costs <- cbind(site_types, full_cull_cost)
