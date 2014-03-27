SimEval <-
function(n, FUN, cut = FALSE, ...){
  n.sim <- n # number of simulations
  FUN <- match.fun(FUN) # imputation function to be applied
  
  # simulating the missing data matrix
  sim.data <- vector("list", n.sim)
  for (i in seq_len(n.sim)){  
    sim.data[[i]] <- BlockMissing()
    if(cut){
    sim.data[[i]]$date <- date.month[325:900]
    }
  }
  
  # record the evaluation time
  eval.time <- system.time(eval <- mclapply(sim.data, FUN = FUN, ...))
  
  # calculat the rmse
  rmse <- numeric(n.sim)
  for (i in seq_len(n.sim)){
    err4 <- as.matrix(eval[[i]] - complete.chunk)
    n <- nmissing(sim.data[[i]])
    rmse[i] <- sqrt(sum(err4^2) / n)
  }
  return(list(rmse = rmse, time = eval.time))
}
