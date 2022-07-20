
#### Economics results processing ####

# Load dataset

sites_summary_type <- load(here::here("outputs",
                                      scenario_name,
                                      "economics",
                                      paste0(scenario_name, "-economics.Rdata")))

sims <- unique(sites_summary_type$simNo)
sims_site_time_summary <- data.frame()
for(k in 1:length(sims)){
  sim <- dplyr::filter(sites_summary_type, simNo == sims[k])
  site_ID <- unique(sim$siteID)
  store <- data.frame()
  for(s in 1:length(site_ID)){
    by_site <- as.data.frame(dplyr::filter(sim, siteID == site_ID[s]))
    by_site$recordID <- rownames(by_site)
    remove_row <- rep(NA, nrow(by_site))
    for(i in 1:nrow(by_site)){
      if((by_site$state[i] == data.table::shift(by_site$state)[i]) %in% TRUE){
        by_site$tdiff[i] <- by_site$tdiff[i] + by_site$tdiff[i - 1]
        remove_row[[i]] <- as.numeric(i - 1)
      } else {
        by_site$tdiff[i] <- by_site$tdiff[i]
      }
    }
    remove_row <- na.omit(remove_row)
    by_site <- by_site %>% dplyr::filter(!(recordID %in% remove_row))
    store <- rbind(store, by_site)
  }
  sims_site_time_summary <- rbind(sims_site_time_summary, store)
}


library(dplyr)