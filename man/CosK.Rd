\name{CosK}
\alias{CosK}

\title{
cosine kernel function
}
\description{
cosine kernel function, can be passed as an argument to the Cutoff() function.
}
\usage{
CosK(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
      any numeric vector
}
}

\value{
given a vector of values, it returns a vector of cosine kernel weights
}

\author{
Lingbing Feng
}

\examples{
x <- 1:10
y <- CosK(x)
plot(x, y, type = "b")

}

