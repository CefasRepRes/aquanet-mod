#### Figure 6 ####

library(tidyverse)
library(here)
library(gridExtra)

# Baseline data ----------------------------------------------------------------

baseline_size <- read.csv(here::here("outputs",
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
                                 "epidemic_size_sensitivity.csv"))
sens_size$mean_inf_diff <- (sens_size$mean_infections - baseline_size_mean)/baseline_size_mean * 100
sens_size$q95_inf_diff <- (sens_size$q95_infections - baseline_size_q95)/baseline_size_q95 * 100

# Duration
sens_duration <- read.csv(here::here("outputs",
                                     "epidemic_duration_sensitivity.csv"))
sens_duration$mean_dur_diff <- (sens_duration$mean_duration - baseline_duration_mean)/baseline_duration_mean * 100
sens_duration$q95_dur_diff <- (sens_duration$q95_duration - baseline_duration_q95)/baseline_duration_q95 * 100

# Select disease params --------------------------------------------------------

# Size
disease_params_size <- sens_size %>%
  dplyr::filter(scenario %in% c("sens_f_high",
                                "sens_f_low",
                                "sens_sc_high",
                                "sens_sc_low",
                                "sens_ct_high",
                                "sens_ct_low",
                                "sens_se_high",
                                "sens_se_low",
                                "sens_sl_high",
                                "sens_sl_low",
                                "sens_dr_high",
                                "sens_dr_low"))
disease_params_size <- disease_params_size %>% 
  select(scenario, mean_inf_diff, q95_inf_diff) %>%
  pivot_longer(cols = c(mean_inf_diff,
                        q95_inf_diff))

# Duration
disease_params_duration <- sens_duration %>%
  dplyr::filter(scenario %in% c("sens_f_high",
                                "sens_f_low",
                                "sens_ct_high",
                                "sens_ct_low",
                                "sens_sc_high",
                                "sens_sc_low",
                                "sens_se_high",
                                "sens_se_low",
                                "sens_sl_high",
                                "sens_sl_low",
                                "sens_dr_high",
                                "sens_dr_low"))
disease_params_duration <- disease_params_duration %>% 
  select(scenario, mean_dur_diff, q95_dur_diff) %>%
  pivot_longer(cols = c(mean_dur_diff,
                        q95_dur_diff))

# Set param names
disease_params_names <- c("sl -",
                          "sl +",
                          "se -",
                          "se +",
                          expression(paste(italic("sc"), " -")),
                          expression(paste(italic("sc"), " +")),
                          "f -",
                          "f +",
                          expression(paste(italic("dr"), " -")),
                          expression(paste(italic("dr"), " +")),
                          expression(paste(italic("ct"), " -")),
                          expression(paste(italic("ct"), " +")))

# Plots ------------------------------------------------------------------------

# Size
disease_size <- ggplot(disease_params_size, aes(x = value,
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
  scale_y_discrete(labels = disease_params_names, 
                   limits = rev) +
  scale_x_continuous(breaks = seq(-20, 30, 10)) +
  geom_vline(xintercept = 0) +
  theme_light()+
  labs(tag = "A")

disease_size

# Duration
disease_duration <- ggplot(disease_params_duration, aes(x = value,
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
  scale_y_discrete(labels = disease_params_names, 
                   limits = rev) +
  scale_x_continuous(breaks = seq(-15, 10, 5)) +
  geom_vline(xintercept = 0) +
  theme_light() +
  labs(tag = "B")

# Arrange
grid.arrange(disease_size,
             disease_duration,
             ncol = 1)

svg(here::here("plots",
               "fig_6.svg"),
    width = 6, height = 8)
grid.arrange(disease_size,
             disease_duration,
             ncol = 1)
dev.off()

png(here::here("plots",
               "fig_6.png"),
    width = 6, height = 8, units = "in", res = 180)
grid.arrange(disease_size,
             disease_duration,
             ncol = 1)
dev.off()
