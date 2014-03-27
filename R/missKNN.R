missKNN <-
function(data, k = 5, ...){
  require(DMwR)
  knnImputation(data = data, k = k, ...)
}
