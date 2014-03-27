implot2 <-
function(x){
  require(grid)
  len <- length(x)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(len, 1)))
  vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
  
  for (i in 1:len){
    print(x[[i]], vp = vplayout(i, 1))
  }
}
