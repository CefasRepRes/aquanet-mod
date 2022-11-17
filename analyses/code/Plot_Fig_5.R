#### Figure 5 ####

library(tidyverse)
library(here)
library(gridExtra)

# Baseline data ----------------------------------------------------------------

baseline_size <- read.csv(here::here("outputs",
                                     "large_run_csvs",
                                     "epidemic_size_scenario_summary.csv"))
baseline_size_mean <- baseline_size %>%
  dplyr::filter(scenario == "baseline") %>%
  dplyr::select(mean_infections) %>%
  as.numeric()
baseline_size_q95 <- baseline_size %>%
  dplyr::filter(scenario == "baseline") %>%
  dplyr::select(q95_infections) %>%
  as.numeric()

# Duration
baseline_duration <- read.csv(here::here("outputs",
                                         "large_run_csvs",
                                         "epidemic_duration_scenario_summary.csv"))
baseline_duration_mean <- baseline_duration %>%
  dplyr::filter(scenario == "baseline") %>%
  dplyr::select(mean_duration) %>%
  as.numeric()
baseline_duration_q95 <- baseline_duration %>%
  dplyr::filter(scenario == "baseline") %>%
  dplyr::select(q95_duration) %>%
  as.numeric()

# Get data ---------------------------------------------------------------------

# Size
sens_size <- read.csv(here::here("outputs",
                                 "large_run_csvs",
                                 "epidemic_size_sensitivity.csv"))
sens_size$mean_inf_diff <- (sens_size$mean_infections - baseline_size_mean)/baseline_size_mean * 100
sens_size$q95_inf_diff <- (sens_size$q95_infections - baseline_size_q95)/baseline_size_q95 * 100

# Duration
sens_duration <- read.csv(here::here("outputs",
                                     "large_run_csvs",
                                     "epidemic_duration_sensitivity.csv"))
sens_duration$mean_dur_diff <- (sens_duration$mean_duration - baseline_duration_mean)/baseline_duration_mean * 100
sens_duration$q95_dur_diff <- (sens_duration$q95_duration - baseline_duration_q95)/baseline_duration_q95 * 100

# Select model params --------------------------------------------------------

# Size
model_params_size <- sens_size %>%
  dplyr::filter(scenario %in% c("sens_a_high",
                                "sens_a_low",
                                "sens_b_high",
                                "sens_b_low",
                                "sens_d_high",
                                "sens_d_low",
                                "sens_s_high",
                                "sens_s_low"))
model_params_size <- model_params_size %>% 
  select(scenario, mean_inf_diff, q95_inf_diff) %>%
  pivot_longer(cols = c(mean_inf_diff,
                        q95_inf_diff))

# Duration
model_params_duration <- sens_duration %>%
  dplyr::filter(scenario %in% c("sens_a_high",
                                "sens_a_low",
                                "sens_b_high",
                                "sens_b_low",
                                "sens_d_high",
                                "sens_d_low",
                                "sens_s_high",
                                "sens_s_low"))
model_params_duration <- model_params_duration %>% 
  select(scenario, mean_dur_diff, q95_dur_diff) %>%
  pivot_longer(cols = c(mean_dur_diff,
                        q95_dur_diff))

# Set param names
model_params_names <- c(expression(paste(alpha, " +")),
                        expression(paste(alpha, " -")),
                        expression(paste(beta, " +")),
                        expression(paste(beta, " -")),
                        expression(paste(delta, " +")),
                        expression(paste(delta, " -")),
                        expression(paste(italic("s"), " +")),
                        expression(paste(italic("s"), " -")))
model_params_names <- c(expression(paste(italic("s"), " -")),
                        expression(paste(italic("s"), " +")),
                        expression(paste(delta, " -")),
                        expression(paste(delta, " +")),
                        expression(paste(beta, " -")),
                        expression(paste(beta, " +")),
                        expression(paste(alpha, " -")),
                        expression(paste(alpha, " +")))

# Plots ------------------------------------------------------------------------

# Size
model_size <- ggplot(model_params_size, aes(x = value,
                                                y = scenario,
                                                fill = name,
                                                shape = name)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_fill_manual(values = c("#D55E00", "#E69F00"),
                    name = "Epidemic values",
                    labels = c("Mean size", "95% size")) +
  scale_shape_manual(values = c(21, 23),
                     name = "Epidemic values",
                     labels = c("Mean size", "95% size")) +
  ylab("") +
  xlab("% change from baseline") +
  scale_y_discrete(limits = rev,
                   labels = model_params_names) +
  geom_vline(xintercept = 0) +
  theme_light()+
  labs(tag = "A")
model_size

# Duration
model_duration <- ggplot(model_params_duration, aes(x = value,
                                                        y = scenario,
                                                        fill = name,
                                                        shape = name)) +
  geom_point(alpha = 0.6, size = 3) +
  scale_fill_manual(values = c("#56B4E9", "#009E73"),
                    name = "Epidemic values",
                    labels = c("Mean duration", "95% duration")) +
  scale_shape_manual(values = c(21, 23),
                     name = "Epidemic values",
                     labels = c("Mean duration", "95% duration")) +
  ylab("") +
  xlab("% change from baseline") +
  scale_y_discrete(labels = model_params_names,
                 limits = rev) +
  geom_vline(xintercept = 0) +
  theme_light() +
  labs(tag = "B")
model_duration

# Arrange
grid.arrange(model_size,
             model_duration,
             ncol = 1)

svg(here::here("plots",
               "fig_5.svg"),
    width = 6, height = 8)
grid.arrange(model_size,
             model_duration,
             ncol = 1)
dev.off()

png(here::here("plots",
               "fig_5.png"),
    width = 6, height = 8, units = "in", res = 180)
grid.arrange(model_size,
             model_duration,
             ncol = 1)
dev.off()

