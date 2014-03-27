MissSimulation <-
function(n = 84, maxlen = 15, cnst = 15, prob = 0.03) {
  
  rec <- rep(NA, n)
  
  for (i in 1:length(rec)){
    
    if (i==1) {
      rec[i] <- count <- rbinom(1, 1, prob)
    }
    if (count == maxlen) {
      rec[i] <- count <- 0
    } 
    else if (count == 0) {
      rec[i] <- count <- rbinom(1, 1, prob)
    } 
    else {
      rec[i] <- rbinom(1, 1, (count + cnst)/(maxlen + cnst))
      count <- count + rec[i]
    }
    
  }
  return(rec)
}
