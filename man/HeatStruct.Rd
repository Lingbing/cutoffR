% Generated by roxygen2 (4.0.0): do not edit by hand
\name{HeatStruct}
\alias{HeatStruct}
\title{Structure Heatmap with Missing Value Demonstration}
\usage{
HeatStruct(data, high.col = "steelblue", low.col = "white",
  missing.col = "gold", xlab = "", ylab = "")
}
\arguments{
\item{data}{a data frame or matrix, possibly with missing values denoted by NA}

\item{high.col}{color for high values, can be a number or a color name, default
is "steelblue.}

\item{low.col}{color for high values, can be a number or a color name, default
is "white".}

\item{missing.col}{color for missing values, can be a number or a color nam,
default is "gold"}

\item{xlab}{a title for the x axis.}

\item{ylab}{a title for the y axis .}
}
\description{
Structure Heatmap with Missing Value Demonstration
}
\details{
Structure heatmap is like a normal heatmap, but is particulary useful
when they are missing values in the data matrix. Default color were carefully
chosen so normally it is a good choice for your data. However, you are still
encouraged to play around with it.
}
\examples{
data(hqmr.data)
# use a subset of the hqmr.data
# notice the gold chunks which represent missing values
subdata <- hqmr.data[1000:1200, 1:30]
HeatStruct(subdata)
# change colors for high.col, low.col and missing.col
HeatStruct(subdata, low.col = "blue", high.col = "red", missing.col = "black")
}

