state.costs <- function(data, state, state_codes, site_types){
  # Select state
  selected_state <- filter(data, state %in% state_codes)
  # Summarise
  state_sumamry <- cbind(summarise(data, across(tdiff, sum)),
                         summarise(data, across(-tdiff, first)))
  state_summary <- group_by(selected_state, siteID) %>% state.summary()
  state_summary <- state_summary[, -1] # Remove extra column called simNo
  # Get daily cost for that stage
  state_daily_cost <- dplyr::filter(daily_cost, stage == state)
  # Calculate duration costs for each site and sim
  sims <- unique(state_summary$simNo)
  costs <- data.frame()
  for(k in 1:length(sims)){ # Loop over simNo
    for(i in 1:length(site_types)){ # Loop over site type
      type <- site_types[[i]] # Set site type
      daily_site_cost <- dplyr::filter(state_daily_cost, site_type == type) # Select the cost for that site type
      state_summary_by_sim <- dplyr::filter(state_summary, simNo == k) # Select the simNo
      # Calculate the cost over the duration of the simulation
      # duration_cost = site type (1) * duration of time in the state * daily cost
      duration_cost <- (state_summary_by_sim[, type] * state_summary_by_sim[, "tdiff"]) * daily_site_cost$farm_cost_per_day
      total_duration_cost <- as.data.frame(sum(duration_cost)) # Calculate the total cost over the duration
      total_duration_cost$simNo <- k # Add simulation number
      costs <- rbind(costs, total_duration_cost)
    }}
  
  # Make into data frame
  state_costs <- cbind(site_types, costs)
  colnames(state_costs)[2] <- "duration_cost"
  
  # Calculate total cost over the course of a simulation
  sim_summary <- tidyr::pivot_wider(state_costs, names_from = site_types,
                                    values_from = duration_cost) # Reformat
  sim_summary$total_cost <- rowSums(sim_summary[, -1], na.rm = T) # Total cost [, -1] so as not to include the simNo in total
  
  # Calculate the average cost per simulation
  outbreak_costs <- data.frame(stage = state,
                               mean_cost = mean(sim_summary$total_cost),
                               sd_cost = sd(sim_summary$total_cost),
                               median_cost = median(sim_summary$total_cost),
                               min_cost = min(sim_summary$total_cost),
                               max_cost = max(sim_summary$total_cost))
  return(outbreak_costs)
}
