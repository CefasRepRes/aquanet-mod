imports.exports <- function(contact_network, imports_exports){
  if(imports_exports == "imports"){
    trade <- igraph::degree(contact_network,
                            mode = "in")
  } else if (imports_exports == "exports") {
    trade <- igraph::degree(contact_network,
                              mode = "out")
  }
  most_trade <- which(trade == max(trade))
  movements <- data.frame(mean = mean(trade),
                             median = median(trade),
                             min_movement = min(trade),
                             max_movement = max(trade),
                             quant_95 = quantile(trade, 0.95),
                             site_with_most_trade = names(most_trade))
  trade_hist <- hist(trade,
       main = paste("Histogram of", imports_exports),
       xlab = paste("Number of fish", imports_exports))
  return(movements)
}

