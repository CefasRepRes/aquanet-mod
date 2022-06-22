state.summary <- function(data){
  cbind(summarise(data, across(tdiff, sum)),
        summarise(data, across(-tdiff, first)))
}