\name{Cutoff-package}
\alias{Cutoff-package}
\docType{package}
\title{
Spatio-Temporal Imputation Method
}
\description{
This package contains a set of tools to perform spatio-temporal imputation. The key function is the Cutoff() function which implements the CUTOFF imputation method with many options. It also provides some useful functions to carry out cross-validation, simulation and visulization for imputation purposes. 
}
\details{
\tabular{ll}{
Package: \tab Cutoff\cr
Type: \tab Package\cr
Version: \tab 0.0.1\cr
Date: \tab 2013-05-02\cr
License: \tab GPL-2\cr
}
~
}
\author{
Lingbing Feng

Maintainer: Lingbing Feng <lingbing.feng@anu.edu.au>

}

\keyword{ Spatio-temporal }
\keyword{ imputation }

\examples{
data(hqmr.cube)
nmissing(hqmr.cube)
HeatStruct(hqmr.cube)
hqmr.cube$date <- date.month
new <- Cut(hqmr.cube)
HeatStruct(new)
}
