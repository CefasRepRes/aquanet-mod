time.per.stage <- function(scenario_name){
  # Load dataset -----
  load(here::here("outputs",
                  scenario_name,
                  "economics",
                  paste0(scenario_name, "-economics.Rdata")))
  # Begin loop -----
  ## Loop over simulations =====
  # Create list of simulation numbers
  sims <- unique(sites_summary_type$simNo)
  # Empty data frame to save results into
  sims_site_time_summary <- data.frame()
  for(k in 1:length(sims)){
    # Filter by simulation
    sim <- dplyr::filter(sites_summary_type, simNo == sims[k])
    ## Loop over sites =====
    # Create list of site IDs
    site_ID <- unique(sim$siteID)
    # Empty data frame to store results
    store <- data.frame()
    for(s in 1:length(site_ID)){
      # Filter by site
      by_site <- as.data.frame(dplyr::filter(sim, siteID == site_ID[s]))
      # Create a record ID for each row
      by_site$recordID <- rownames(by_site)
      # Create a vector to store rows to be removed
      remove_row <- rep(NA, nrow(by_site))
      ## Loop over rows ======
      # This loop checks if he state matches that on the previous row.
      # If it does match, it sums the time (tdiff) and removes the previous row
      # If not, the time remains the same.
      for(i in 1:nrow(by_site)){
        # If the state is the same as the previous state, add the times together
        if((by_site$state[i] == data.table::shift(by_site$state)[i]) %in% TRUE){
          by_site$tdiff[i] <- by_site$tdiff[i] + by_site$tdiff[i - 1]
          # Save the previous row for removal
          remove_row[[i]] <- as.numeric(i - 1)
        } else { # If the states are not the same, save the time
          by_site$tdiff[i] <- by_site$tdiff[i]
        }
      }
      # Remove excess rows
      remove_row <- na.omit(remove_row)
      by_site <- by_site %>% dplyr::filter(!(recordID %in% remove_row))
      # Bind to data frame
      store <- rbind(store, by_site)
    }
    # Bind to data frame
    sims_site_time_summary <- rbind(sims_site_time_summary, store)
    # Print the simulation number to see progress
    print(sims[k])
  }
  ## Save the results =====
  save(sims_site_time_summary,
       file = here::here("outputs",
                         scenario_name,
                         "economics",
                         paste0(scenario_name, "-site-time-summary.Rdata")))
  return(sims_site_time_summary)
}