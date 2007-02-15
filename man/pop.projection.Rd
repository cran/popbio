\name{pop.projection}
\alias{pop.projection}
\title{ Calculate population growth rates by projection }
\description{
  Calculates the population growth rate and stable stage distribution by repeated projections of the equation \code{n(t+1)=An(t)}.
}
\usage{
pop.projection(A,n,iterations=20)
}

\arguments{
  \item{A}{ A projection matrix  }
  \item{n}{ An initial age or stage vector }
  \item{iterations}{ Number of iterations }

}
\details{  Eventually, structured populations will convergence to a stable stage distribution where each new stage vector is changing by the same proportion (lambda).  

}
\value{
A list with 5 items
  \item{lambda }{Estimate of lambda using change between the last two population counts}
  \item{stable.stage }{Estimate of stable stage distribution using proportions in last stage vector}
   \item{stage.vector}{A matrix with the number of projected individuals in each stage class}
    \item{pop.sizes}{Total number of projected individuals}
    \item{pop.changes}{Proportional change in population size}
 
}
\references{see section 2.2 in Caswell 2001

}
\author{ Chris Stubben }

\seealso{ \code{\link{stage.vector.plot}} to plot stage vectors}
\examples{

stages<-c("seedling", "vegetative", "flowering")
A<-matrix(c(
0,    0,    6,
0.4,  0.6,  0.02,
0.01, 0.2,  0.25
), nrow=3, byrow=TRUE, 
    dimnames=list(stages,stages) 
)

n<-c(25,25,25)
pop.projection(A,n, 15)

eigen.analysis(A)$damping.ratio
pop.projection(A,n, 50)$pop.changes



data(whale)
A<-whale$T+whale$F
n<-c(4,38,36,22)

## convergence is slow with damping ratio close to 1
pop.projection(A,n, 10)

eigen.analysis(A)$damping.ratio

pop.projection(A,n, 250)$pop.changes

}
\keyword{ survey }
