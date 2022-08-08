state.list <- function(scenario){
  sum_per_state <- as.data.frame(colSums(scenario[,-c(1:8)]))
  sum_per_state$state <- 1:42
  colnames(sum_per_state)[1] <- "count"
  return(sum_per_state)
}
