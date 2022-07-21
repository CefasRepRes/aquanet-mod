#### Plot economics outputs ####

library(here)
library(tidyverse)
library(reshape2)

# Stop scientific notation
options(scipen = 999)

# Load data --------------------------------------------------------------------

# Load full economics data
econom_data <- read.csv(here::here("outputs",
                                   "full_run_for_economics",
                                   "economics",
                                   "full_outbreak_costs.csv"))
str(econom_data)

# Extract component data
component_data <- econom_data %>% dplyr::select(-simNo, -total_outbreak_cost)
component_data <- melt(component_data)

# Plot -------------------------------------------------------------------------

# Plot component data
  # TODO: add gridlines in between categories to make it more readable
ggplot(component_data, aes(x = variable, y = value)) +
  geom_jitter(col = "cornflowerblue", alpha = 0.3) +
  # Set better y axis breaks
  scale_y_continuous(breaks = seq(0, 
                                  max(component_data$value, na.rm = T), 
                                  by = 10000000)) +
  # Set better labels for the cost components
  scale_x_discrete(labels = c("fallow_total_cost" = "Fallow",
                              "no_manage_total_cost" = "Unmanaged infection",
                              "contact_trace_total_cost" = "Contact tracing",
                              "catchment_control_total_cost" = "Infected catchment",
                              "cull_cost_farm" = "Cull cost to site",
                              "cull_cost_fhi" = "Cull cost to FHI")) +
  # Set better labels
  ylab("Total outbreak cost (GBP)") +
  xlab("Cost component") +
  # Add marker for the mean
  stat_summary(
    geom = "point",
    fun = "mean",
    col = "black",
    shape = 3
  ) +
  # Get better theme
  theme_light() +
  # Put x-axis text on an angle
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
