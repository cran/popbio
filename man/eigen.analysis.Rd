\name{eigen.analysis}
\alias{eigen.analysis}

\title{ Eigenvalue and eigenvector analysis of a projection matrix }
\description{
 Calculate population growth rate and other demographic parameters from a projection matrix model using matrix algebra 
}
\usage{
eigen.analysis(A, zero=TRUE)
}

\arguments{
  \item{A}{ a projection matrix }
  \item{zero}{ Set sensitivities for unobserved transitions to zero }
}
\details{ The calculation of eigenvalues and eignvectors in Matlab is
  listed in section 4.8.1 (p. 107) in Caswell (2001).  In addition,
  the damping ratio is described in section 4.7.1, sensitivities in
  section 9.1, and elasticities in section 9.2}

\note{ If matrix A is singular,  then \code{eigen.analysis} will return
  elasticities, sensitivities, and reproductive values  with  NAs.
  This function is also included in \code{demogR} package.}

\value{
A list with 6 items
    \item{lambda1 }{dominant eigenvalue}
    \item{stable.stage}{proportional stable stage distribution}
    \item{sensitivities }{matrix of eigenvalue sensitivities}
    \item{elasticities}{matrix of eigenvalue elasticities}
    \item{repro.value}{reproductive value scaled so v[1]=1}
    \item{damping.ratio}{dominant eigenvalue divided by second largest eigenvalue}
}
\references{
Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA.
 }
\author{ Original code by  James Holland Jones, Stanford University,
  Department of Anthropological Sciences, 12 August 2005 at
  \url{http://popstudies.stanford.edu/summer_course/. } 
 }

\seealso{ \code{\link{projection.matrix}} to construct matrix model}
\examples{

data(whale)
whaleA<-whale$T+whale$F

a<-eigen.analysis(whaleA)

a

ymax<-max(a$repro.value)*1.25
barplot(a$repro.value, col="blue", ylim=c(0,  ymax ), xpd=FALSE, 
       ylab="Reproductive value", xlab="Killer whale stage class")
box()

## display sensitititivies of "impossible" transitions 
## (postreprod -> juvenile, yearling -> mature, etc)
eigen.analysis(whaleA, zero=FALSE)$sensitivities

data(teasel)
teaselA<-teasel$T + teasel$F


a<-eigen.analysis(teaselA)
a
barplot(a$stable.stage, col="green", ylim=c(0,1), 
       ylab="Stable stage proportion", xlab="Teasel stage class")
box()


}
\keyword{ survey }
