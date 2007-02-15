\name{stage.vector.plot}
\alias{stage.vector.plot}

\title{ Plot stage vector projections }
\description{
 Plots short-term dynamics and convergence to stage stage distribution using stage vector projections.  
}
\usage{
stage.vector.plot(stage.vectors, proportions=TRUE, legend.coords="topright", 
        ylim=NULL, xlab="Years", ylab=NULL,  ... )
}
 
\arguments{
  \item{stage.vectors}{ a matrix listing stage class vectors in columnw}
  \item{proportions}{ plot proportional changes or total numbers, defaults to proportions. }
  \item{legend.coords}{ a \code{\link{legend}} keyword or vector of x,y coordinates, defaults to top-right corner }
  \item{ylim}{the y limits of the plot, defaults to min and max values in stage.vector.projections}
  \item{xlab}{a label for the x axis}
  \item{ylab}{a label for the y axis}
  \item{...}{additional options are passed to \code{\link{plot}} function}


}
\details{   A plot of stage class projections }
\references{see section 2.2 in Caswell 2001 }
\author{ Chris Stubben }

\seealso{ see \code{\link{pop.projection}} }
\examples{


stages<-c("seedling", "vegetative", "flowering")
A<-matrix(c(
0,     0,     6,
0.4,  0.6,  0.02,
0.01,0.2,  0.25
), nrow=3, byrow=TRUE, 
    dimnames=list(stages,stages) 
)

n<-c(25,25,25)
p<-pop.projection(A,n, 15)


op <- par(ask = TRUE )
stage.vector.plot(p$stage.vectors)

stage.vector.plot(p$stage.vectors, prop=FALSE, legend=c(1,300))
## log scale
stage.vector.plot(p$stage.vectors, prop=FALSE, log='y', 
   legend="bottomright", ylab="Number in stage class (log scaled)")

par(op)

}
\keyword{ survey }
