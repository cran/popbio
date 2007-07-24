\name{vitalsim}
\alias{vitalsim}
\title{ Calculate stochastic growth rate and extinction time CDF using
  vital rates with within-year, auto-, and cross-correlations}
\description{
 This function runs a series of stochastic PVA population projections by sampling
 vital rates from a beta, stretched beta, or lognormal distribution
 and includes within-year, auto-, and cross-correlations.
}
\usage{
vitalsim(vrmeans, vrvars, corrin, corrout, makemx, n0,
yrspan, Ne=500, tmax=50,runs=100, corr.power="matrix", vrtypes=NULL,
vrmins=NULL, vrmaxs=NULL, sumweight=NULL)
}
\arguments{
  \item{vrmeans}{means of vital rates}
  \item{vrvars}{variance of vital rates}
  \item{corrin}{within year correlation}
  \item{corrout}{between year correlations}
  \item{makemx}{ a function that creates a square projection matrix
      from a vector of \code{vrmeans}}
  \item{n0}{initial population vector}
  \item{yrspan}{the number of years of correlations to build into the M12 matrix}
  \item{Ne}{ quasi-extinction threshold }
  \item{tmax}{latest time to calculate extinction probability, default 50}
  \item{runs}{the number of trajectories, default is 100, but 1000 is recommended}
  \item{corr.power}{calculate the multi-year correlation matrix using
      'matrix' power or 'element' by element power. The
      default is 'matrix' exponentiation following Box 8.10. See note for details}
  \item{vrtypes}{identifies the distribution for each rate in vrmeans
      where 1 = beta, 2 = stretched beta, 3 = lognormal, default is
      all ones}
  \item{vrmins}{minimum value for each vital rate; use zeros for rates
      that are not stretched betas, default is all zeros}
  \item{vrmaxs}{maximum value for each vital rate; use zeros for rates
      that are not stretched betas, default is all zeros}
  \item{sumweight}{a vector of weights, with 0 to omit a class and 1 to include it
                     when computing the summed density to compare to
                     the quasi-extinction threshold, default is to
                     include all classes}
}
\details{
  Vital rates used must be either
 fertility values or binomial probabilities, i.e., probabilities for events
 with only two possible outcomes (such as survival). Means and variances of the
 vital rates should preferably be corrected to remove sampling errors and
 demographic stochasticity. Note that this version of the function does not
 simulate demographic stochasticity and is density-independent.


}
\value{
  The function plots a histogram of log stochastic growth rates and the cumulative probability of quasi-extinction and returns a list with 4 items:
  \item{detLambda}{the deterministic population growth rate computed from
  the mean matrix}
  \item{stochlambda}{the mean stochastic growth rate with 95\% confidence intervals.}
  \item{logLambdas}{ a vector of all log stochastic growth rates in first plot}
  \item{CDFExt}{a vector of cumulative probabilities of
      quasi-extinction in second plot}

}

\source{converted Matlab code from Box 8.10 in Morris and Doak (2002)}


\references{ Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA.}

\author{ Original MATLAB program by Morris and Doak (2002: 301 - 304).
Adapted to R by Patrick Nantel, 12 July 2005 with slight modifications
by Chris Stubben. }

\note{ We included the option to calculate correlations using
  both matrix or element by element powers.  On page 294, Morris and Doak (2002)  explain that
in  matrix 8.16, "higher powers of B or B' for pairs of rates in years
that are increasingly distant from one another reflect the decay of
correlation over time". With matrix exponentiation, values in B and
B' increase through time instead of decreasing - the effect of the matrix exponentiation is that
correlations among vital rates are exagerated, which may explain why the
lambda is lower and extinction probabilities higher than
with matrix selection
}

\seealso{ \code{\link{hudmxdef}}, \code{\link{hudvrs}} and \code{\link{hudcorrs}}  }

\examples{
## load vital rates and correlation matrices
data(hudvrs)
data(hudcorrs)
## set vrtypes
hudvrtypes<-c(rep(1,13), rep(3,5), rep(1,6))
## using default corr.power="matrix" as in box 8.10
x<-vitalsim(hudvrs$mean, hudvrs$var, hudcorrs$corrin,
hudcorrs$corrout, hudmxdef, vrtypes=hudvrtypes,
n0=c(4264,3,30,16,25,5), yrspan=20 )
## deterministic and stochastic lambda
x[1:2]
## log stochastic lambda
log(x$stochLambda)
}

\keyword{ survey }
