cull.cost <- function(data, cull_costs){
  # When a site enters a fallow state enter a one-off cost
  data <- data.table(data)
  fallow <- data[state %in% c(14, 34)]
  sims <- unique(data[, simNo])
  # Loop over simulations and add the one-off fallow cost, depending on site type
  full_cull_cost <- data.table()
  system.time({
    for(k in 1:length(sims)){
      for(i in 1:length(site_types)){
        type <- site_types[[i]]
        fallow <- data.table(fallow)
        fallow_by_sim <- fallow[simNo == sims[k]]
        #filter_cull_cost <- dplyr::filter(cull_cost, site_type == type)
        cull_cost <- data.table(cull_cost)
        filter_cull_cost <- cull_cost[site_type == type]
        # Separate costs to the farm and the FHI (competent authority)
        farm_cull_cost <- sum(fallow_by_sim[, ..type]) * filter_cull_cost$cull_cost_farm
        fhi_cull_cost <- sum(fallow_by_sim[, ..type]) * filter_cull_cost$cull_cost_fhi
        # Combine into single data frame
        comb_cull_cost <- data.table(cull_cost_farm = farm_cull_cost,
                                     cull_cost_fhi = fhi_cull_cost,
                                     sim = k)
        # Save results
        full_cull_cost <- rbind(full_cull_cost, comb_cull_cost)
      }
      print(sims[k])
    }})
  
  # Summarise per simulation
  full_cull_cost_sim <- full_cull_cost[, by = sim,
                                       .(total_cull_cost_farm = sum(cull_cost_farm, na.rm = T),
                                         total_cull_cost_fhi = sum(cull_cost_fhi, na.rm = T))]
  return(full_cull_cost_sim)
}