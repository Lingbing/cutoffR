\name{HeatStruct}
\alias{HeatStruct}

\title{
Structure heatmap of a data matrix with missing values
}
\description{
This function is used to detect the missing pattern of a data matrix. 
}
\usage{
HeatStruct(data, high.col = "steelblue", missing.col = "gold", xlab = "", ylab = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
 a data matrix
}
  \item{high.col}{
 color for non-missing values
}
  \item{missing.col}{
 color for representing missing values
}
  \item{xlab}{
 labels for the x axis
}
  \item{ylab}{
 labels for the y axis
}
}

\author{
Lingbing Feng
}



\examples{
data(hqmr.cube)
HeatStruct(hqmr.cube[, -79])
}

