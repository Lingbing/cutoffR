\name{missKNN}
\alias{missKNN}

\title{
missing value imputation by KNN method
}
\description{
missing value imputation by KNN method
}
\usage{
missKNN(data, k = 5, ...)
}

\arguments{
  \item{data}{
A data matrix
}
  \item{k}{
The numbero of nearest neighbors
}
  \item{\dots}{
Other named arguments passed down to knnImpute()
}
}
\details{
Missing value imputation by KNN method. The original function is the knnImputation() function from the DMwR package.
}
\value{
It returns a imputed matrix with no missing values
}

\examples{
data(hqmr.cube)
new <- missKNN(hqmr.cube)
nmissing(new)
}

