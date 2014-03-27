missMICE <-
function(x, m = 5, mincor = 0.5, ...){
  imp <- mice(x, m = m, pred = quickpred(x, mincor = mincor), ...)
  comp <- vector("list", m)
  for (i in 1:m){
    comp[[i]] <- complete(imp, i)
    
  }
  # using Reduce function to do a summation on a list of matrices
  com <- Reduce("+", comp)/m
  return(com)
}
