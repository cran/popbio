\name{net.reproductive.rate}
\alias{net.reproductive.rate}

\title{ Net reproductive rate }
\description{
  Calculates the net reproductive rate of a stage classified matrix using the dominant eigenvalue of the matrix R.
}
\usage{
net.reproductive.rate(T, F)
}

\arguments{
  \item{T}{ transition matrix }
  \item{F}{ fertility matrix }
}
\details{ see section 5.3.4 in Caswell (2001). }
\value{
  Net reproductive rate
}
\references{ Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA. }
\author{ Chris Stubben }

\seealso{ see \code{\link{fundamental.matrix}} and \code{\link{generation.time}} for other age-specific traits }
\examples{

data(whale)
net.reproductive.rate(whale$T, whale$F)

}
\keyword{ survey }

