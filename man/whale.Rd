\name{whale}
\alias{whale}
\docType{data}
\title{T and F matrix for killer whale}
\description{
  Transition and Fertility matrices for killer whales
}
\usage{data(whale)}
\format{
  A list with T and F matrices 
  }
}
\source{
 see Chapter 5 in Caswell (2001)
}
\references{
Brault, S., and H. Caswell. 1993. Pod-specific demography of killer whales (\emph{Orcinus orca}). Ecology 74:1444-1454.

Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA.
}
\examples{
data(whale)
whale$T + whale$F
}
\keyword{datasets}
