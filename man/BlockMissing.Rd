\name{BlockMissing}
\alias{BlockMissing}
\title{
Simulate the block-missing pattern as in the rainfall data fron the Murray-Darling
basin in southeastern Australia
}
\description{
This function actually wraps up the more general function MissSimulation to simulate
the missing pattern in rainfall data from the Murray-Darling basin.Fore different missing matrices are simulated separately by setting careful-chosen parameters to the MissiSimulation() function and combined together to ensemble a whole data matrix with block missing pattern that is similar to the original missing pattern.
}
\usage{
BlockMissing()
}

\details{
No arguments required for this function. It only works for simulating the missing pattern in the Muray-Darling basin, in terms of both data value and dimension.

}
\value{
It returns a data matrix with similar missing pattern as the raindfall data from the Murray-Darling basin. 
}

\author{
Lingbing Feng
}


\examples{
data(complete.chunk)
data(hqmr.cube)
mdb.new <- BlockMissing()
# examine its missing pattern
HeatStruct(mdb.new)
# compare to the whole data
HeatStruct(hqmr.cube)
}
