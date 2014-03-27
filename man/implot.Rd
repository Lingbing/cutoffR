\name{implot}
\alias{implot}
\title{
plot for comparing imputation performance
}
\description{

}
\usage{
implot(i = 1, start = "1911-01-01", end = "2010-12-01", oridata = hqmr.reorder, impdata = hqmr.new, iso = TRUE, ..., pch = 21, cex = 2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{i}{
 the ith column to be plotted on.
}
  \item{start}{
start date for the plot
}
  \item{end}{
end date for the plot
}
  \item{oridata}{
the original data matrix
}
  \item{impdata}{
the imputed data matrix
}
  \item{iso}{
Logical, if TRUE, isolated missing points will not be linked by line.
}
  \item{\dots}{
Other named arguments can be passed down ge
}
  \item{pch}{
  point character.
}
  \item{cex}{
character expanding factor.
}
}

\value{
It returns a ggplot2 object. 
}

\author{
Lingbing Feng
}

\examples{
hqmr.new <- Cut(hqmr.cube)
p1 <- implot(1, oridata = hqmr.cube[, -79])
p1
}

