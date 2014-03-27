\name{nmissing}
\alias{nmissing}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
This function counts the total number of missing values in a data matrix
}
\description{

}
\usage{
nmissing(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
]a matrix or dataframe
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
return the total number of missing values (NAs)
}

\author{
Lingbing Feng
}

\examples{
data(hqmr.cube)
nmissing(hqmr.cube)
}

\keyword{ missing value}
\keyword{ imputation }
