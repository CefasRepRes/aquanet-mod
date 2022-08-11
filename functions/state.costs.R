state.costs <- function(data, state, site_types){
  # Seelect state
    if(state == "fallow"){
      state_codes <- c(4, 5,
                       14, 15,
                       24, 25,
                       34, 35)
    } else if(state == "no_manage"){
      state_codes <- c(10, 30)
    } else if(state == "contact_trace"){
      state_codes <- c(1, 7,
                       11, 17,
                       21, 27,
                       31, 37)
    } else if(state == "catchment_control"){
      state_codes <- c(21, 24, 
                       25, 26, 
                       27)
    } 
    # Filter data by state
    state_summary <- data[state %in% state_codes]
    # Get daily cost for that stage
    daily_cost <- data.table(daily_cost)
    state_daily_cost <- daily_cost[stage == state]
    # Calculate duration costs for each site and sim
    sims <- unique(state_summary[, simNo])
    costs <- data.table()
    length(costs) <- length(sims)
    for(k in 1:length(sims)){ # Loop over simNo
      for(i in 1:length(site_types)){ # Loop over site type
        if(!(state %in% c("fallow", "no_manage", "contact_trace", "catchment_control"))) +
          stop("You have entered an incorrect state type. Please choose from fallow, no_manage, contact_trace or catchment_controls")
        type <- site_types[[i]] # Set site type
        daily_site_cost <- state_daily_cost[site_type == type]
        state_summary_by_sim <- state_summary[simNo == k]
        # Calculate the cost over the duration of the simulation
        # duration_cost = site type (1) * duration of time in the state * daily cost
        duration_cost <- (state_summary_by_sim[, ..type] * state_summary_by_sim[, "tdiff"]) * daily_site_cost$farm_cost_per_day
        total_duration_cost <- as.data.frame(sum(duration_cost)) # Calculate the total cost over the duration
        total_duration_cost$simNo <- sims[k] # Add simulation number
        costs <- rbind(costs, total_duration_cost)
      }
      print(sims[k])}
    
    # Make into data frame
    state_costs <- cbind(site_types, costs)
    colnames(state_costs)[2] <- "duration_cost"
    
    # Calculate total cost over the course of a simulation
    sim_summary <- tidyr::pivot_wider(state_costs, names_from = site_types,
                                      values_from = duration_cost) # Reformat
    sim_summary$total_cost <- rowSums(sim_summary[, -1], na.rm = T) # Total cost [, -1] so as not to include the simNo in total
    colnames(sim_summary)[16] <- paste0(state, "_total_cost")
    sim_summary <- sim_summary[, c("simNo", paste0(state, "_total_cost"))]
    return(sim_summary)
}
