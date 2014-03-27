\name{missRF}
\alias{missRF}
\title{
imputation using random forest
}
\description{
This function is a simplifed version of the original function missForest() in the missForest package.
}
\usage{
missRF(x, mtry = 4, ntree = 50, ...)
}

\arguments{
  \item{x}{
a data matrix with missing values. The columns correspond to the variables and the rows to the observations
}
\item{mtry}{
mtry in missForest()
}
\item{ntree}{
number of trees for each splitting
}
  \item{\dots}{
some other arguments that can be passed to missForest() function
}
}
\details{

}
\value{
imputed data matrix of the same type as "x"
}

\seealso{
\code{\link{missForest}}
}
\examples{
data(hqmr.cube)
system.time(new <- missRF(hqmr.cube))
}

