\name{impCV}
\alias{impCV}

\title{
Spatio-temporal cross-validation for imputation methods
}
\description{
This function can be used to evaluate the performance of an imputation method given an data matrix with missing values.
}
\usage{
impCV(data, FUN = Cut, date.info = TRUE, cfold = 10, rfold = 10, ...)
}

\arguments{
  \item{data}{
 a data matrix or a data frame
}
  \item{FUN}{
 the imputation function that needs to be evaluated. 
}
  \item{date.info}{
a logical indicating whether the data contains a date information which is required by the imputation method
}
  \item{cfold}{
a number defines the number of column folds for cross-validation
}
  \item{rfold}{
a number defines the number of row folds for cross-validation
}
  \item{\dots}{
Other arguments passed to the imputation function
}
}
\details{
The form of the imputation function has to be exactly like the imputation provided in this package, that is , a function takes a data matrix with missing values as input and outputs a data matrix with no missing values. 

}
\value{
It returns the cross-validation RMSE for the imputation method that applied to complete the data matrix with missing values.
}

\author{
Lingbing Feng
}

\examples{
data(hqmr.cube)
cv_results <- impCV(data = hqmr.cube, FUN = Cut, date.info = TRUE)
cv_results
}

