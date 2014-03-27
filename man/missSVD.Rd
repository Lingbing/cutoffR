\name{missSVD}
\alias{missSVD}
\title{
imputation by SVD method
}
\description{
This function is used to impute missing values based on singular value decomposition (SVD) technique. 
}
\usage{
missSVD(x, ...)
}

\arguments{
  \item{x}{
a numeric data matrix
}
  \item{\dots}{
other argumets for the original SVD function 
}
}
\details{
The 
}
\value{
given a data matrix with missing value, it returns a complete data matrix.
}


\seealso{
  \code{\link{SVDmiss}}
}
\examples{
data(hqmr.cube)
new <- missSVD(hqmr.cube)
nmissing(new)
}

