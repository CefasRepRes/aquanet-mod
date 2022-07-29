load.results.site.type <- function(scenario_name){
  # Create economics folder
  dir.create(file.path(here::here("outputs",
                                  scenario_name,
                                  "economics")), 
             showWarnings = FALSE)
  # Get filenames of where outputs are
    # TODO: update with correct file path
  filenames <- list.files(here::here("outputs",
                                     scenario_name,
                                     "FullDetails"),
                          pattern = "batchNo-*")
  # Load simulation states
  sim_states_all <- data.frame()
  for(i in 1:length(filenames)){
    load(here::here("outputs",
                    scenario_name,
                    "FullDetails",
                    filenames[i]))
    sim_states_all <- rbind(sim_states_all, sim_states)}
  # Load simulation times
  sim_times_all <- data.frame()
  for(i in 1:length(filenames)){
    load(here::here("outputs",
                    scenario_name,
                    "FullDetails",
                    filenames[i]))
    sim_times_all <- rbind(sim_times_all, sim_times)}
  # Combine states and time spent in each state
  sites_summary <- dplyr::left_join(sim_states_all, 
                                    sim_times_all,
                                    by = c("timeID", "simNo"))
  # Select relevant columns
  sites_summary <- dplyr::select(sites_summary,
                                 siteID,
                                 state,
                                 tdiff,
                                 simNo)
  threes <- filter(sites_summary, state %in% c(3, 13, 23, 33))
  if(nrow(threes) == 0) return(warning("There are no state threes in your outputs. Check your simulation code."))
  # Correct tdiff for initialization (tdiff = NA)
  sites_summary$tdiff[is.na(sites_summary$tdiff)] <- 0  
  # Change siteID to modelID (because that's what it is)
  colnames(sites_summary)[1] <- "modelID"
  # Get actual site id
  site_id_mod <- read.csv(here::here("outputs",
                                     scenario_name,
                                     "site_details_with_model_id.csv"))
  site_id_mod <- dplyr::select(site_id_mod, siteID, modelID)
  sites_summary <- left_join(sites_summary, site_id_mod, by = "modelID")
  sites_summary <- dplyr::select(sites_summary, -modelID)
  # Read in site type vector
  site_type <- read.csv(here::here("outputs",
                                   scenario_name,
                                   "site_types.csv"))
  # Join to summary 
  sites_summary_type <- dplyr::left_join(sites_summary, site_type, 
                                         by = c("siteID" = "site_code"))
  # Save as R datafile
  save(sites_summary_type,
       file = here::here("outputs",
                         scenario_name,
                         "economics",
                         paste0(scenario_name, "-economics.Rdata")))
  return(sites_summary_type)
}