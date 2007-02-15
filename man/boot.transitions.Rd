\name{boot.transitions}
\alias{boot.transitions}

\title{ Bootstrap observed census transitions }
\description{
  Calculate bootstrap distributions of population growth rates (lambda) and stage vectors, and projection matrix elements by randomly sampling with replacement from a data frame of observed transitions
}
\usage{
boot.transitions(transitions, iterations, by.stage.counts = FALSE, ...)
}

\arguments{
  \item{transitions}{ A transition table with \link{ordered} stage and fate columns, plus one or more fertility columns }
  \item{iterations}{ Number of bootstrap iterations }
  \item{by.stage.counts}{ Resample transitions with equal probability (default) or by subsets of initial stage counts }
  \item{...}{ additional options passed to \code{\link{projection.matrix}} }

}
\details{  Currently, the transition table must have fertility column names matching a stage class level
  
}
\value{
A list with 3 items
  \item{lambda }{A vector containing bootstrap values for lambda}
  \item{matrix }{A matrix containing bootstrap transtion matrices with one projection matrix per row.}
   \item{vector }{A matrix containing bootstrap stage vectors with one stage vector per row.}
 
}
\references{see Morris and Doak 2005 in \url{http://esapubs.org/Archive/mono/M075/004/appendix-A.htm} for resampling by stage class counts}
\author{ Chris Stubben }

\seealso{ See Also \code{\link{projection.matrix}} }
\examples{

data(aq.trans)

x<-subset(aq.trans, year==1996)

# calculate lamda, seed survival and recruitment rate using aq.matrix
rec<-nrow(subset(aq.trans, year==1997 & stage == "recruit"))
aq.96<- aq.matrix(x, rec)

# add  individual fertilities to data frame 
aq.96.trans<-aq.matrix(x, rec, summary=FALSE)

# pass estimated transitions in aq.96 to projection matrix 
aq.96.boot<-boot.transitions(aq.96.trans, 200, add=c(1,1, aq.96$seed.survival, 2,1, aq.96$recruitment.rate) )

# calcualte confidence intervals using quantile()
ci<- quantile(aq.96.boot$lambda, c(0.025,0.975) )

aq.96$lambda
ci

# plot histogram
hist(aq.96.boot$lambda, col="green", xlab="Lambda", main=paste('Bootstrap estimates of population\ngrowth rate from 1996-1997'))
abline(v=ci, lty=3)



}
\keyword{ survey }
