\name{countCDFxt}
\alias{countCDFxt}

\title{ Calculate count-based extinction probabilities and bootstrap
  confidence intervals  }
\description{
  This function takes parameters derived from abundance time-series and
  calculates the probability of extinction with bootstrap confidence
  intervals for a density-independent model, using a diffusion approximation.
}
\usage{
countCDFxt(mu, sig2, nt, tq, Nc, Ne, tmax=50, Nboot=500)
}
\arguments{
  \item{mu }{ estimated value of mean mu}
  \item{sig2}{estimated value of sample variance }
  \item{nt}{number of transitions in the data set}
  \item{tq}{length of the census (in years)}
  \item{Nc}{current population size}
  \item{Ne }{quasi-extinction threshold}
  \item{tmax}{latest time to calculate extinction probability, default
    50}
  \item{Nboot}{ number of bootstrap samples for calculating confidence intervals
   for extinction probabilities, default 500)}
}
%\details{  }
\value{
  The function plots the cumulative probabilities of quasi-extinction through
  time, with 95\% confidence interval. It also creates a data frame with
  the extinction time CDF for the best parameter estimates (Gbest), and the
  lower and upper bootstrap confidence limits for extinction probabilites (Glo, Gup).
}

\source{converted Matlab code from  Box 3.4 in Morris and Doak (2002)}

\references{ Dennis et al. 1991, Ecological Monographs 61: 115-143.

Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA. }

\author{ Adapted to R by Patrick Nantel, 4 May 2005,
         from program 'extprob' of Morris \& Doak (2002: 79-86) }

%\note{}
%\seealso{ }

\examples{
## Return a graph like Figure 3.8 in Morris and Doak (2002: 84).

countCDFxt(mu=0.02134, sig2=0.01305, nt=38, tq=38, Nc=99, Ne=20)

}

\keyword{survey }
