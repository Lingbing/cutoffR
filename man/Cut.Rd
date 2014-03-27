\name{Cut}
\alias{Cut}

\title{
An simpler and quick version of the function Cutoff(). 
}
\description{
This is an simpler version of the Cutoff() function. It imputes a spatio-temporal data matrix using the default setting of Cutoff().
}
\usage{
Cut(data, cutoff = 0.75, method = "pearson", ID = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
 The data to be imputed, with a date column
}
  \item{cutoff}{
  The cuoff value to be used
}
  \item{method}{
 The type of correlation coefficient, could be "pearson", "spearman" or "kendall"
}
  \item{ID}{
  logical. Should the reference ID information be kept?
}
  \item{\dots}{
 Other arguments that can be passed to the cor() function
}
}
\details{
The data must contain a date column names "date". All the values in the data matrix must be numeric or missing. Missing values should be denoted by NA. 
If ID is "TRUE", a list of IDs will be reserved for inspection. Each element in the list contains a vector of IDs for that candidate station(a station with missing value and needs to be imputed).
}
\value{
It returns a data matrix with exact the same size as the input data, but all missing values will be imputed by the CUTOFF method. 
}

\author{
Lingbing Feng
}

\examples{
data(hqmr.cube)
data(date.month)
hqmr.cube$date <- date.month
# impute the missing data matrix
hqmr.new <- Cut(hqmr.cube)
# examine by counting the missing values after imputation
# should be zero!
nmissing(hqmr.new)

# or by visulizing the data matrix using HeatStruct()
HeatStruct(hqmr.new)
}
