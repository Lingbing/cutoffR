missSVD <-
function(x, ...){
  require(SpatioTemporal)
  SVDmiss(x, ...)$Xfill
}
