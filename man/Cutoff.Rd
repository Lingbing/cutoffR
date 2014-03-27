\name{Cutoff}
\alias{Cutoff}

\title{
The Cutoff Spatio-Temporal Imputation Method
}
\description{
This function implements the CUTOFF imputation method and also provides extensive options to the CUTOFF method. 
}
\usage{
Cutoff(data, N = 4, cutoff = 0.75, P = 5, M = floor(P/2), Adj = 1, space.weight = FALSE, method = c("correlation", "number", "penalty"), time.opts = c("average", "adjacent"), kernel = FALSE, kerFUN = NULL, lambda = NULL, corr = "pearson", keep.ID = FALSE, ...)
}

\arguments{
  \item{data}{
 a data frame contains a date column withe the column name "date"
}
  \item{N}{
a number used for the "number" method: cutoff the number of reference stations by this number. It defaults to 4.
}
  \item{cutoff}{
the cutoff value for the "correlation" method. The default is 0.75.
}
  \item{P}{
It is a number that needs to be defined if the method is "penalty". This is the penalty number which means for those missing stations with more than P number of reference stations, we penalize the redundant reference stations by fixing the number to be P. It defaults to 5.

}
  \item{M}{
optional for the "penalty" method. For those missing stations with less than P reference stations, some might only have one or two reference stations, this value can be used to augment its reference file. The defaul value is floor(p/2).
}
  \item{Adj}{
A value indicating how many adjacent values will be used for imputation when time.opts is "adjacent". Default value is 1, the alternative value is 2. It has to be either 1 or 2.
}
  \item{space.weight}{
logical indicting whether the space weighting strategy will be employed. Default is FALSE.
}
  \item{method}{
Three options for the CUTOFF imputation method: "correlation", "number" or "penalty". The default method is "correlation"
}
  \item{time.opts}{
The options of operations in the time domain. The default is "average", can either to "adjacent", for which more values from the adjacent points will be used for imputing the missing values.
}
  \item{kernel}{
A logical value indicates whether or not the kernel smoothing method would be applied. The default setting is FALSE, and no kernel will be used.
}
  \item{kerFUN}{
If kenerl is TRUE, then this kerFUN has to be defined. It can be any kernel function that we can define one by ourself. In this package, we provide some common kernel functions: UnifK() for the uniform kernel, EpanK() for the Epanechnikov kernel , GaussK() for the Gaussian kernel and CosK() for the Cosine kernel.
}
  \item{lambda}{
a number for controlling the window size for the kernel functions
}
  \item{corr}{
the correlation coefficient used for the "correlation" method. The default kind is Pearson's correlation coefficient, thus "pearson".
}
  \item{keep.ID}{
logical indicating whether to keeo the reference IDs for the impuation task. If TRUE, after imputing the reference IDs for each station can be inpected.
}
  \item{\dots}{
Other arguments that can be passed down.
}
}
\details{
Details are in the paper draft. Will be here soon.
}
\value{
If keep.id = FALSE, then it returns an imputed data matrix with no NA values.
If keep.id = TRUE, then it returns a list contains two elements:
  \item{ imputed }{ the imputed data matrix with no missing values}
  \item{ ID }{ a list of reference IDs during imputation for each station}

}
\author{
Lingbing Feng <lingbing.feng@anu.edu.au>
}


\seealso{
A simpler version is \code{\link{Cut}}
}
\examples{
data(hqmr.cube)
data(date.month)
new <- Cutoff(hqmr.cube)
nmissing(new)
HeatStruct(new)
# more examples will be here soon
}

