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

# Filter by state --------------------------------------------------------------
  # TODO: Expand to other states

# Sum tdiff to get total time, but keep all other factors the same
state.summary <- function(data){
  cbind(summarise_each(select(data, tdiff), funs(sum)),
        summarise_each(select(data, -tdiff), funs(first)))
}

## Fallow ======================================================================

# Select fallow states
fallow <- filter(sites_summary_type, state == c(4, 14, 24, 34))

# Summarise
fallow <- group_by(fallow, siteID) %>% state.summary()

# Allocate costs
 # TODO: make this for all farms/states (currently dummy variables)
fallow_costs <- data.frame(medium_table = 50.00,
                           medium_fish = 40.00)

# Cost to medium table
  # TODO: find a way to loop/do for all
fallow$mediumtable <- fallow$mediumtable * fallow$tdiff * fallow_costs$medium_table
# Cost to medium fishing
fallow$mediumfish <- fallow$mediumfish * fallow$mediumfish * fallow_costs$medium_fish

# Get total costs per site
fallow$site_cost <- rowSums(fallow)

# Get total costs
fallow_total_cost <- sum(fallow$site_cost)
