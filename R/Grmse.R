Grmse <-
function(ximp, xtrue) {
  
  nmissing <- function(x) sum(is.na(x))
  err <- ximp - xtrue
  rmse <- sqrt((sum((unlist(err)^2), na.rm = TRUE))/(length(unlist(err)) - 
                                                          nmissing(err)))
  return(rmse)
}
