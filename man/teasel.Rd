\name{teasel}
\alias{teasel}
\docType{data}
\title{T and F matrix for teasel}
\description{
  Transition T and Fertility F matrices for the plant teasel
}
\usage{data(teasel)}
\format{
  A list with T and F matrices 
  }
}
\source{
   Example 5.2
}
\references{
   Caswell 2001.  Matrix Population Models.  Second edition.
}
\examples{
data(teasel)
A<-teasel$T + teasel$F
A
}
\keyword{datasets}
