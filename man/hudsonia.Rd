\name{hudsonia}
\alias{hudsonia}
\docType{data}
\title{ Projection matrices for mountain golden heather }
\description{
  Projection matrices for the mountain golden heather (\emph{Hudsonia montana}) for the years 1985 through 1988
}
\usage{data(hudsonia)}
\format{
A list of 4 matrices from 1985-1988
}
\details{
    A projection matrix with 6 size classes: seeds, seedlings, and 4 size classes divided by plant area.
}
\source{
   Table 6.7 in Morris and Doak (2002).  The original data is from Frost (1990).

Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability
analysis. Sinauer, Sunderland, Massachusetts, USA.

}

\examples{
data(hudsonia)
## various ways to return lambda1 for each year
sapply(hudsonia, function(x) eigen.analysis(x)$lambda1)
#sapply(hudsonia, eigen.analysis)["lambda1",]
#sapply(lapply(hudsonia, eigen.analysis), "[[", "lambda1")

## mean matrix 
mean(hudsonia)
eigen.analysis(mean(hudsonia))$lambda1

}
\keyword{datasets}
