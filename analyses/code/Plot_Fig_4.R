#### Figure 4 ####

library(tidyverse)
library(here)
library(gridExtra)

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

no_catchment <- loadResultsSummary("no_catchment_controls")
no_catchment_inf <- infectionsPerScenario(no_catchment)
no_catchment_inf$scenario <- "no_catchment_controls"

no_contact <- loadResultsSummary("no_contact_tracing")
no_contact_inf <- infectionsPerScenario(no_contact)
no_contact_inf$scenario <- "no_contact_tracing"

no_controls_inf <- read.csv(here::here("outputs",
                                       "no_controls_infections.csv"))
no_controls_inf$scenario <- "no_controls"
no_controls_inf <- no_controls_inf %>% dplyr::select(-X)

top_sites_removed_inf <- read.csv(here::here("outputs",
                                       "top_removed_infections.csv"))
top_sites_removed_inf$scenario <- "top_sites_removed"
top_sites_removed_inf <- top_sites_removed_inf %>% dplyr::select(-X)

data_all <- rbind(baseline_inf,
                  no_catchment_inf,
                  no_contact_inf,
                  no_controls_inf,
                  top_sites_removed_inf)

scen_comp <- ggplot(data_all, aes(x = scenario, y = max_inf)) +
  geom_point(position = "jitter", alpha = 0.2, col = "#0072B2") +
  geom_hline(yintercept = 2272, alpha = 0.7) +
  xlab("Scenario") +
  ylab("Maximum number of \ninfected sites") +
  scale_x_discrete(labels = c("E&W policy",
                              "No catchment \ncontrols",
                              "No contact \ntracing",
                              "No controls",
                              "No controls top \nsites removed")) +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

svg(here::here("plots",
               "fig_4.svg"),
    width = 6, height = 4)
scen_comp
dev.off()
