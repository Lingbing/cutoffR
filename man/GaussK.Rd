\name{GaussK}
\alias{GaussK}
\title{
  The Gaussian (or Normal) kernel function
}
\description{
  The Gaussian (or Normal) kernel function that can be passed as an option of the Cutoff() function.
}
\usage{
GaussK(x)
}
\arguments{
  \item{x}{
An numeric input vector.
}
}
\examples{
  x <- seq(-3, 3, len = 60)
  GaussK(x)
}