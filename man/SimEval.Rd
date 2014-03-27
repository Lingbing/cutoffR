\name{SimEval}
\alias{SimEval}

\title{
Simulate and evaluae imputation method
}
\description{
This function first simulates a set of data matrices in the complete chunk data with similiar missing pattern as the original rainfall data in the Murray-Darling basin, and then apply provided imputation function to the set of missing matrices and evalute its average imputation performance.
}
\usage{
SimEval(n, FUN, cut = FALSE, ...)
}

\arguments{
  \item{n}{
number of simulations
}
  \item{FUN}{
the imputation function to be evaluted
}
  \item{cut}{
Default if FALSE. If cut = TRUE, then date information in the simulated matrices will be kept.
}
  \item{\dots}{
other arguments to be passed to the impuation function
}
}
\details{
This function could be exhausitive and time consuming. So, initially, a small number of n can be tried. Normally, 100 will be enough. 
}
\value{

  \item{ rmse }{ mean RMSE of the imputation function applied to the simulted data}
  \item{ time }{elapsed time for the evaluation}

}

\author{
Lingbing Feng
}

\examples{
try <- SimEval(n = 100, FUN = Cut, cut = TRUE)
# the mean RMSE is
try$rmse
# the elapsed time for the evaluation is
try$time
}
