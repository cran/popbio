\name{generation.time}
\alias{generation.time}

\title{ Generation time }
\description{
  Calculates the generation time of a stage-classified matrix
}
\usage{
generation.time(T, F)
}

\arguments{
  \item{T}{ transition matrix }
  \item{F}{ fertility matrix }
}
\details{ see section 5.3.5 in Caswell (2001). }
\value{
  Generation time
}
\references{ Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA. }
\author{ Chris Stubben }
%\note{ }
\seealso{ see \code{\link{fundamental.matrix}} and \code{\link{net.reproductive.rate}} for other age-specific traits }
\examples{

data(whale)
generation.time(whale$T, whale$F)

}
\keyword{ survey }

