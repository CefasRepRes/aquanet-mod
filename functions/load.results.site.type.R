load.results.site.type <- function(scenario_name){
  # Create economics folder
  dir.create(file.path(here::here("outputs",
                                  scenario_name,
                                  "economics")), 
             showWarnings = FALSE)
  # Get filenames of where outputs are
  filenames <- list.files(here::here("outputs",
                                     scenario_name,
                                     "full_results"),
                          pattern = "batchNo-*")
  # Load simulation outputs
  sims_all <- data.frame()
  for(i in 1:length(filenames)){
    load(here::here("outputs",
                    scenario_name,
                    "full_results",
                    filenames[i]))
    sims_all <- rbind(sims_all, sims)}
  # Select relevant columns
  sites_summary <- data.table(sims_all)
  sites_summary <- sites_summary[, .(siteID, 
                                     state,
                                     simNo,
                                     tdiff,
                                     t)]
  threes <- sites_summary[state %in% c(3, 13, 23, 33)]
  if(nrow(threes) == 0) return(warning("There are no state threes in your outputs. Check your simulation code"))
  # Change siteID to modelID (because that's what it is)
  colnames(sites_summary)[1] <- "modelID"
  # Get actual site id
  site_id_mod <- data.table::fread(here::here("outputs",
                                              scenario_name,
                                              "site_details_with_model_id.csv"))
  site_id_mod <- site_id_mod[, .(siteID,
                                 modelID)]
  sites_summary <- merge(sites_summary, 
                         site_id_mod, 
                         all.x = TRUE, 
                         by = "modelID")
  sites_summary <- sites_summary[, modelID := NULL]
  # Read in site type vector
  site_type <- data.table::fread(here::here("outputs",
                                   scenario_name,
                                   "site_types.csv"))
  # Join to summary 
  sites_summary_type <- dplyr::left_join(sites_summary, site_type, 
                                         by = c("siteID" = "site_code"))
  sites_summary_type <- merge(sites_summary,
                              site_type,
                              all.x = TRUE,
                              by.x = "siteID",
                              by.y = "site_code")
  # Save as R datafile
  save(sites_summary_type,
       file = here::here("outputs",
                         scenario_name,
                         "economics",
                         paste0(scenario_name, "-economics.Rdata")))
  return(sites_summary_type)
}