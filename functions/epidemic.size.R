epidemic.size <- function(results){
  # Convert results into data.table
  results <- data.table(results)
  # Remove sim_no 0 (due to overallocation)
  valid_results <- results[sim_no != 0]# Epidemic size
  no_inf <- valid_results[, c("sim_no", "cumulative_no_infected_sites")][ # Select sim_no and cumulative infected sites
    , by = sim_no, .(max_inf = max(cumulative_no_infected_sites))][ # Get maximum infected sites per sim_no
      , c("sim_no", "max_inf")] # Select sim_no and max_inf
  no_inf <- unique(no_inf)
  # Statistics
  mean_infections <- mean(no_inf[, max_inf])
  min_infections <- min(no_inf[, max_inf])
  max_infections <- max(no_inf[, max_inf])
  median_infections <- median(no_inf[, max_inf])
  q5_infections <- quantile(no_inf[, max_inf], 0.05)
  q95_infections <- quantile(no_inf[, max_inf], 0.95)
  # Calculate percent of simulations which died out
  # Time out is where the epidemic infects fewer than 5 sites in total
  die_out <- (sum(no_inf[, max_inf] < 5)/3000)*100
  # Get number of iterations
  iterations <- max(results[, sim_no])
  # Put into nice table
  scenario_results <- data.table::data.table(mean_infections = mean_infections,
                                             min_infections = min_infections,
                                             max_infections = max_infections,
                                             median_infections = median_infections,
                                             q05_infections = q5_infections,
                                             q95_infections = q95_infections,
                                             percent_die_out = die_out,
                                             no_iter = iterations)
  return(scenario_results)
}

