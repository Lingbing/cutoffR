\name{MissSimulation}
\alias{MissSimulation}
\title{
simulate missing values
}
\description{
MissSimulation is used to simulate a vector of missing indicators. It returns 1s and 0s. 
1s are indicators for missing values. It should be used with other functions.
}
\usage{
MissSimulation(n = 84, maxlen = 15, cnst = 15, prob = 0.03)
}

\arguments{
  \item{n}{
 the length of the missing vector.
}
  \item{maxlen}{
 the maximum length of continuous missing values.
}
  \item{cnst}{
 a constant for controlling the missing performance.
}
  \item{prob}{
 the probability for every value to be missing.
}
}
\details{
This is a very useful function for simulating missing values that exhibit some sort of chunk missing pattern, that is, values tend to be missing together that represent big missing gaps or chunks. 
}
\value{
a vector contains 1s and 0s, in which 1 is an indicator for missing.
}

\author{
Lingbing Feng
}

\examples{
mvec <- MissSimulation(100)
mvec
}

