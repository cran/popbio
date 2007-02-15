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
   Table 6.7 in Morris and Doak (2002), Quantitative Conservation Biology.  The original data is from Frost (1990).

}

\examples{
data(hudsonia)

lapply(hudsonia, function(x){Re(eigen(x)$values[1])})

}
\keyword{datasets}
