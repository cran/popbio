\name{splitA}
\Rdversion{1.1}
\alias{splitA}
\title{
Split a projection matrix into separate T and F matrices
}
\description{
Splits a projection matrix into transition and fertility matrices where
\code{A = T + F}.
}
\usage{
splitA(A, r = 1, c = -1)
}
\arguments{
  \item{A}{ a projection matrix}
  \item{r}{ rows containing fertilities, default is first row}
  \item{c}{ columns containing fertilities, default is all columns
      except first}
}
\details{ see section 5.1 in Caswell (2001) }

\note{ By default, the fertility matrix will include elements in the first row (except first
  element).   In a prebreeding census, it may be possible  to have
  matrix elements that include both fertility and transition rates and
therefore this function will not work.  }

\value{ A list with T and F matrices}

\seealso{ functions like  \code{\link{generation.time}} and
  \code{\link{net.reproductive.rate}} use
  \code{splitA} internally to split the matrix  }


\references{ Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.}

\author{ Chris Stubben }

%\seealso{ }
\examples{
data(whale)
splitA(whale)
# teasel -fertilitiles in last column
data(teasel)
splitA(teasel, r=1:6, c=6)
# hudsonia - fertilities in first two columns
data(hudsonia)
splitA(hudsonia[[1]], r=1:2)
# survival curves
x<-sapply(hudsonia, function(x) colSums(splitA(x, r=1:2)$T))
matplot2(t(x), legend="bottomright", ylab="Survival", 
main="Hudsonia survival curves")

}

\keyword{ survey}
