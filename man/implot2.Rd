\name{implot2}
\alias{implot2}

\title{
put imputation plots into one column
}
\description{
This function can be used to put the imputation plots derived by implot() into one column
}
\usage{
implot2(x)
}

\arguments{
  \item{x}{
a list of ggplot2 objects
}
}

\author{
Lingbing Feng
}

\examples{
data(hqmr.cube)
hqmr.new <- Cut(hqmr.cube)
p1 <- implot(1, oridata = hqmr.cube[, -79], impdata = hqmr.new)
p2 <- implot(6, oridata = hqmr.cube[, -79], impdata = hqmr.new)
implot(p1, p2)
}
