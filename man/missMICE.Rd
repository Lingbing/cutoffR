\name{missMICE}
\alias{missMICE}

\title{
Imputation using the MICE algorithm
}
\description{
This function imputes a data matrix with missing values by employing the MICE algorithm.
}
\usage{
missMICE(x, m = 5, mincor = 0.5, ...)
}

\arguments{
  \item{x}{
a data matrix withe missing value
}
  \item{m}{
the number of multiple imputations
}
  \item{mincor}{
the minimum correlation for setting the prediction matrix for the MICE algorithm.
}
  \item{\dots}{
some other arguments that can be passed down to the mice() function in the mice package.
}
}
\details{
This function imputes multiple replicates of the data matrix by the MICE algorithm and then takes the average as the final imputed value. The arguments "mincor"" is useful for those matrices with many columns.
}
\value{
a imputed data matrix with no missing values
}

\seealso{
\code{\link{mice}}
}
\examples{
data(hqmr.cube)
new <- missMICE(hqmr.cube[, -79])
nmissing(new)
}
