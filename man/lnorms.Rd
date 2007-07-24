\name{lnorms}
\alias{lnorms}
\title{ Generate random lognormal values for fertility rates}
\description{Converts standard normal random values to
 lognormals with defined means and variances}
\usage{
lnorms(means, vars, rawelems)
}
\arguments{
  \item{means}{ mean value of the fertility rate}
  \item{vars}{variance of the vital rate (not standard deviation)}
  \item{rawelems}{a vector of one or more normal random values}
}
%\details{}
\value{
  A vector of random lognormal values.
}

\source{converted Matlab code from  Box 8.4 in Morris and Doak (2002) }


\references{Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability analysis.
Sinauer, Sunderland, Massachusetts, USA.}

\author{Original Matlab code by Morris and Doak (2002: 281).
Adapted to R by Patrick Nantel, 20 June 2005. }

\note{This function could probably be replaced with built-in functions
  for the Log Normal Distribution \code{\link{rlnorm} }}
\seealso{  \code{\link{stretchbetaval} }}
\examples{
lnorms(3,1.5,rnorm(1)) 
 # Generate lognormal random fertilities
 # for a population of 1000 mature individuals (Ni) with mean fertility (f) of
 # 3 and inter-individual variance in fertility (varF) of 1.5.
 Ni   <- 1000
 f    <-  3
 varF <-  1.5
 rndfert  <- lnorms(f,varF,rnorm(Ni,0,1))

 hist(rndfert,40, main="Lognormal random fertilities", 
xlab="Fertility rate", col="blue")

## or using rlnorm in stats
# hist(rlnorm(1000, log(3), log(1.5)), 40, xlim=c(0,10), col="blue")


 }
\keyword{ survey }

