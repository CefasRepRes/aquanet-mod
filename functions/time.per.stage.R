
#### Economics results processing ####

# Load dataset

sites_summary_type <- load(here::here("outputs",
                                      scenario_name,
                                      "economics",
                                      paste0(scenario_name, "-economics.Rdata")))

sims <- unique(sites_summary_type$simNo)

for(k in 1:length(sims)){
  sim <- dplyr::filter(sites_summary_type, simNo == k)
}

test <- dplyr::filter(sites_summary_type, simNo == 1)
test_siteID <- unique(sites_summary_type$siteID)
test_site <- as.data.frame(dplyr::filter(test, siteID == 1191))

trial <- test_site
for(i in 1:nrow(test_site)){
  if((test_site$state[i] == data.table::shift(test_site$state,
                                              fill = 100)[i]) == T){
    test_site$tdiff[i] <- test_site$tdiff[i] + test_site$tdiff[i - 1]
  } else {
    test_site$tdiff[i] <- test_site$tdiff[i]
  }
}

test_site_2 <- test_site %>% group_by(state) %>% filter(tdiff == max(tdiff))
library(dplyr)