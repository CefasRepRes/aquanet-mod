epidemic.size <- function(results) {
  # Remove simNo 0
  results <- results %>% dplyr::filter(simNo != 0)
  # Epidemic size
  max_inf_per_sim <- results %>%
    dplyr::select(simNo, cumulativeNoInfectedSites) %>%
    dplyr::group_by(simNo) %>%
    dplyr::mutate(max_inf = max(cumulativeNoInfectedSites)) %>%
    dplyr::select(simNo, max_inf)
  max_inf_per_sim <- unique(max_inf_per_sim)
  # Statistics
  mean_inf_sites <- mean(max_inf_per_sim$max_inf)
  min_inf_sites <- min(max_inf_per_sim$max_inf)
  max_inf_sites <- max(max_inf_per_sim$max_inf)
  median_inf_sites <- median(max_inf_per_sim$max_inf)
  q5_inf_sites <- quantile(max_inf_per_sim$max_inf, 0.05)
  q95_inf_sites <- quantile(max_inf_per_sim$max_inf, 0.95)
  die_out <- (sum(max_inf_per_sim$max_inf < 6)/3000)*100
  # Get number of iterations
  iterations <- max(results$simNo)
  scenario_results <- data.frame(mean_inf_sites = mean_inf_sites,
                                 min_inf_sites = min_inf_sites,
                                 max_inf_sites = max_inf_sites,
                                 median_inf_sites = median_inf_sites,
                                 q05_inf_sites = q5_inf_sites,
                                 q95_inf_sites = q95_inf_sites,
                                 percent_die_out = die_out,
                                 no_iter = iterations)
  
  return(scenario_results)
}

