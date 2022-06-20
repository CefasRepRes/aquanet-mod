state.costs <- function(state, site_codes, site_types){
  # Select state
  data <- filter(sites_summary_type, state %in% site_codes)
  # Summarise
  state_summary <- group_by(data, siteID) %>% state.summary()
  
  # Allocate daily costs
  state_daily_cost <- dplyr::filter(daily_cost, stage == state)
  
  # Calculate daily costs for each site
  costs <- data.frame()
  for(i in 1:length(site_types)){
    type <- site_types[[i]]
    daily_site_cost <- dplyr::filter(state_daily_cost, site_type == type)
    duration_cost <- (data[, type] * data$tdiff) * daily_site_cost$farm_cost_per_day
    total_duration_cost <- sum(duration_cost)
    costs <- rbind(costs, total_duration_cost)
  }
  
  # Make into data frame
  state_costs <- cbind(site_types, costs)
  colnames(state_costs)[2] <- "duration_cost"
  return(state_costs)
}
