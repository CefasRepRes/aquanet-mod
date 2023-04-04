#### Figure 4 ####

library(tidyverse)
library(here)
library(gridExtra)
library(data.table)

# Get number of connected sites in the network ---------------------------------

# Run test up to fourth script (prepare model objects)

connectedSites <- function(contact_prob_matrix){
  contact_prob_matrix <- contact_prob_matrix$matrix_movements_prob %>%
    methods::as("matrix") %>%
    as.data.frame()
  out_movements <- data.frame(site_id = rownames(contact_prob_matrix),
                              out_movements = rowSums(contact_prob_matrix))
  in_movements <- data.frame(site_id = colnames(contact_prob_matrix),
                             in_movements = colSums(contact_prob_matrix))
  movements <- dplyr::full_join(out_movements, in_movements,
                                by = "site_id")
  movements$total_movements <- movements$out_movements + movements$in_movements
  connected_sites <- dplyr::filter(movements, total_movements > 0)
  connected_sites_number <- nrow(connected_sites)
  return(connected_sites_number)
}

max_sites <- connectedSites(contact_probability_matrix)
max_sites_top_removed <- connectedSites(contact_probability_matrix_top_sites_removed)

# Wrangle data -----------------------------------------------------------------

infectionsPerScenario <- function(results){
  results <- data.table(results)
  valid_results <- results[sim_no != 0]
  no_inf <- valid_results[, c("sim_no", "cumulative_no_infected_sites")][, 
                                                                         by = sim_no, .(max_inf = max(cumulative_no_infected_sites))][, 
                                                                                                                                      c("sim_no", "max_inf")]
}

baseline <- loadResultsSummary("baseline")
baseline_inf <- infectionsPerScenario(baseline)
baseline_inf$scenario <- "baseline"
baseline_inf$scenario_no <- 3

no_catchment <- loadResultsSummary("no_catchment_controls")
no_catchment_inf <- infectionsPerScenario(no_catchment)
no_catchment_inf$scenario <- "no_catchment_controls"
no_catchment_inf$scenario_no <- 4

no_contact <- loadResultsSummary("no_contact_tracing")
no_contact_inf <- infectionsPerScenario(no_contact)
no_contact_inf$scenario <- "no_contact_tracing"
no_contact_inf$scenario_no <- 5

no_controls_inf <- read.csv(here::here("outputs",
                                       "no_controls_infections.csv"))
no_controls_inf$scenario <- "no_controls"
no_controls_inf$scenario_no <- 1
no_controls_inf <- no_controls_inf %>% dplyr::select(-X)

top_sites_removed_inf <- read.csv(here::here("outputs",
                                       "top_removed_infections.csv"))
top_sites_removed_inf$scenario <- "top_sites_removed"
top_sites_removed_inf$scenario_no <- 2
top_sites_removed_inf <- top_sites_removed_inf %>% dplyr::select(-X)

data_all <- rbind(baseline_inf,
                  no_catchment_inf,
                  no_contact_inf,
                  no_controls_inf,
                  top_sites_removed_inf)

# Create graph -----------------------------------------------------------------

scen_comp <- ggplot(data_all, aes(x = scenario_no, y = max_inf)) +
  geom_point(position = "jitter", alpha = 0.2, col = "#0072B2") +
  # Segments for normal network
  geom_segment(aes(x = 0.5, xend = 1.5, y = max_sites, yend = max_sites)) +
  geom_segment(aes(x = 2.5, xend = 5.5, y = max_sites, yend = max_sites)) +
  geom_segment(aes(x = 1.5, xend = 2.5, y = max_sites_top_removed, yend = max_sites_top_removed)) +
  xlab("Scenario number") +
  ylab("Maximum number of \ninfected sites") +
  theme_light()

scen_comp

svg(here::here("plots",
               "fig_4.svg"),
    width = 6, height = 4)
scen_comp
dev.off()

png(here::here("plots",
               "fig_4.png"),
    width = 6, height = 4, units = "in", res = 180)
scen_comp
dev.off()
