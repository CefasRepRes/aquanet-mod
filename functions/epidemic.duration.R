epidemic.duration <- function(results) {
  # Remove simNo 0
  results <- results %>% dplyr::filter(simNo != 0)
  # Epidemic duration
  no_days <- results %>%
    dplyr::select(simNo, t) %>%
    dplyr::group_by(simNo) %>%
    dplyr::mutate(max_t = max(t)) %>%
    dplyr::select(simNo, max_t)
  no_days <- unique(no_days)
  # Statistics
  mean_duration <- mean(no_days$max_t)
  min_duration <- min(no_days$max_t)
  max_duration <- max(no_days$max_t)
  median_duration <- median(no_days$max_t)
  q5_duration <- quantile(no_days$max_t, 0.05)
  q95_duration <- quantile(no_days$max_t, 0.95)
  time_out <- (sum(no_days$max_t < 5*360)/3000)*100
  # Get number of iterations
  iterations <- max(results$simNo)
  scenario_results <- data.frame(mean_duration = mean_duration,
                                 min_duration = min_duration,
                                 max_duration = max_duration,
                                 median_duration = median_duration,
                                 q05_duration = q5_duration,
                                 q95_duration = q95_duration,
                                 percent_time_out = time_out,
                                 no_iter = iterations)
  return(scenario_results)
}