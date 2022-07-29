load.results.summary <- function(no.variables, locationSaveResults) {
  library(data.table)
  locationOfSummary <- paste(locationSaveResults, "/batch_results/", sep="")
  
  files <- list.files(path = locationOfSummary, 
                      pattern = "\\.RData$",
                      recursive = TRUE)
  
  batchNo <- as.integer(substring(
    text = regmatches(x = files, 
                      m = regexpr(perl = TRUE, 
                                  text = files, 
                                  pattern = "batchNo-[0-9]+")),
    first = 9))
  
  
  listFiles <- data.frame(files = files, batchNo = batchNo)
  
  listFiles <- listFiles[order(batchNo),]
  
  no.per.state.wide <- data.table(rep(0, no.variables + 8))
  
  for (i in 1:nrow(listFiles)) {
    load(file = paste(locationOfSummary,as.character(listFiles[i,c('files')]), sep = ""))
    no.per.state.wide[, as.character((ncol(no.per.state.wide)+1):(ncol(no.per.state.wide)+ncol(summaryStates.table))):=summaryStates.table]
    print(i)
  }
  
  if (nrow(summaryStates.table) != (no.variables + 8)) {
    print("Warning: The script may have been feed the wrong parameters.")
  }
  
  no.per.state.wide = data.frame(t(as.matrix(no.per.state.wide)))
  colnames(no.per.state.wide) = c('batchNo',
                                  'k',
                                  't',
                                  'tdiff',
                                  'simNo',
                                  'rate.type',
                                  'no.controlled.catchments',
                                  'cumulativeNoInfectedSites',
                                  as.character(1:no.variables))
  rownames(no.per.state.wide) = 1:nrow(no.per.state.wide)
  
  return(no.per.state.wide)
}
