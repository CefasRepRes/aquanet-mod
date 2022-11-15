#### Results ####

library(aquanet)
library(tidyverse)
library(here)
library(beepr)

# Main scenarios ---------------------------------------------------------------

## Load ========================================================================

baseline <- loadResultsSummary("baseline")
no_catchment_controls <- loadResultsSummary("no_catchment_controls")
no_contact_tracing <- loadResultsSummary("no_contact_tracing")

## Epidemic size ===============================================================

baseline_size <- epidemicSize(baseline)
no_catchment_size <- epidemicSize(no_catchment_controls)
no_contact_size <- epidemicSize(no_contact_tracing)

no_controls_size <- read.csv(here::here("outputs",
                                        "no_controls_size.csv"))
top_sites_removed_size <- read.csv(here::here("outputs",
                                              "top_removed_size.csv"))

epidemic_size <- rbind(baseline_size,
                       no_catchment_size,
                       no_contact_size,
                       dplyr::select(no_controls_size, -X),
                       dplyr::select(top_sites_removed_size, -X))
epidemic_size$scenario <- c("baseline",
                             "no_catchment",
                             "no_contact",
                             "no_controls",
                             "top_sites")
epidemic_size[, 1:9] <- round(epidemic_size[, 1:9], digits = 2)
write.csv(epidemic_size, here::here("outputs",
                                    "epidemic_size_scenario_summary.csv"))

## Epidemic duration ===============================================================

baseline_duration <- epidemicDuration(baseline)
no_catchment_duration <- epidemicDuration(no_catchment_controls)
no_contact_duration <- epidemicDuration(no_contact_tracing)

no_controls_duration <- read.csv(here::here("outputs",
                                        "no_controls_duration.csv"))
top_sites_removed_duration <- read.csv(here::here("outputs",
                                              "top_removed_duration.csv"))

epidemic_duration <- rbind(baseline_duration,
                       no_catchment_duration,
                       no_contact_duration,
                       dplyr::select(no_controls_duration, -X),
                       dplyr::select(top_sites_removed_duration, -X))
epidemic_duration$scenario <- c("baseline",
                            "no_catchment",
                            "no_contact",
                            "no_controls",
                            "top_sites")
epidemic_duration[, 1:9] <- round(epidemic_duration[, 1:9], digits = 2)
write.csv(epidemic_duration, here::here("outputs",
                                    "epidemic_duration_scenario_summary.csv"))

## Epidemic peak ===============================================================

baseline_peak <- epidemicPeak(baseline)
no_catchment_peak <- epidemicPeak(no_catchment_controls)
no_contact_peak <- epidemicPeak(no_contact_tracing)

no_controls_peak <- read.csv(here::here("outputs",
                                            "no_controls_peak.csv"))
top_sites_removed_peak <- read.csv(here::here("outputs",
                                                  "top_removed_peak.csv"))

epidemic_peak <- rbind(baseline_peak,
                           no_catchment_peak,
                           no_contact_peak,
                           dplyr::select(no_controls_peak, -X),
                           dplyr::select(top_sites_removed_peak, -X))
epidemic_peak$scenario <- c("baseline",
                                "no_catchment",
                                "no_contact",
                                "no_controls",
                                "top_sites")
epidemic_peak[, 1:8] <- round(epidemic_peak[, 1:8], digits = 0)
write.csv(epidemic_peak, here::here("outputs",
                                        "epidemic_peak_scenario_summary.csv"))

# Sensitivity testing ----------------------------------------------------------

# List all sensitivity tests
sens_tests <- c("sens_a_high", "sens_a_low",
                "sens_b_high", "sens_b_low",
                "sens_d_high", "sens_d_low",
                "sens_dr_high", "sens_dr_low",
                "sens_ct_high", "sens_ct_low",
                "sens_f_high", "sens_f_low",
                "sens_s_high", "sens_s_low",
                "sens_sc_high", "sens_sc_low",
                "sens_se_high", "sens_se_low",
                "sens_sl_high", "sens_sl_low")

# Get epidemic size
epidemic_size_sens <- data.frame()
for(i in 1:length(sens_tests)){
  l <- loadResultsSummary(sens_tests[i])
  size <- epidemicSize(l)
  size$scenario <- sens_tests[i]
  epidemic_size_sens <- rbind(epidemic_size_sens,
                              size)
};beep()

# Get epidemic duration
epidemic_duration_sens <- data.frame()
for(i in 1:length(sens_tests)){
  l <- loadResultsSummary(sens_tests[i])
  duration <- epidemicDuration(l)
  duration$scenario <- sens_tests[i]
  epidemic_duration_sens <- rbind(epidemic_duration_sens,
                              duration)
};beep()

# Save
epidemic_size_sens[, 1:9] <- round(epidemic_size_sens[, 1:9], digits = 2)
write.csv(epidemic_size_sens, here::here("outputs",
                                         "epidemic_size_sensitivity.csv"))
epidemic_duration_sens[, 1:9] <- round(epidemic_duration_sens[, 1:9], digits = 2)
write.csv(epidemic_duration_sens, here::here("outputs",
                                         "epidemic_duration_sensitivity.csv"))
