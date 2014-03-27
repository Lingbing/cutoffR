HeatStruct <-
function (data, high.col = "steelblue", missing.col = "gold", xlab = "", ylab = "" ){
  
  #  This is a function to inspect the structure of a data matrix, data could be a data frame or a matrix
  #  Like a heatmap for dataframe, but particulary useful to draw out the missing value pattern 
  #  high.col: colour for the values, default is "darkgreen"; low values have been set to be white 
  #  missing.col: colour for missing values, default is "red"
  #  all ggplot routines can be added to polish this plot, e.g. use + labs("stations", "Months") to add labels
  #  xlab and ylab are for labelling the axies
  require(ggplot2)
  if(!is.data.frame(data)){
    data <- as.data.frame(data)
  }
  
  ggpcp(data) +
    aes_string(y = "ROWID", fill = "value", x = "variable") +
    geom_tile() +
    scale_y_continuous(expand=c(0, 1)) +
    scale_fill_continuous(low = "white", high = high.col, na.value = missing.col, guide="colorbar") +
    labs(x = xlab, y = ylab)
}
