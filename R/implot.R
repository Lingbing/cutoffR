implot <-
function(i = 1, start = "1911-01-01", end = "2010-12-01", oridata = hqmr.reorder, impdata = hqmr.new, iso = TRUE, ..., pch = 21, cex = 2){
  
  require(ggplot2)
  
  x <- as.data.frame(cbind(oridata[, i], impdata[, i]))
  x$month <- date.month
  nn <- is.na(x$V1)
  x.na <- x[, -1]
  x.na[!nn, 1] <- NA
  
  p <- ggplot(x, aes(month, V1)) + geom_line()
  
  if(iso) {
    p <- p + geom_line(data = x.na, aes(month, V2), col = "red", lty = 2) +
      geom_point(data = x.na, aes(month, V2), col = "red", pch = pch, cex = cex) +
      ylab("Rainfall")
    p <- p + scale_x_date(limits = c(as.Date(start), as.Date(end)))
  }
  
  else {
    p <- p + geom_line(data = x.na, aes(month, V2), col = "red", lty = 2) + ylab("Rainfall")
    p <- p + scale_x_date(limits = c(as.Date(start), as.Date(end)))
  }
  p
}
