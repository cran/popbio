\name{boot.transitions}
\alias{boot.transitions}

\title{ Bootstrap observed census transitions }
\description{
  Calculate bootstrap distributions of population growth rates
  (lambda), stage vectors, and projection matrix elements by
  randomly sampling with replacement from a stage-fate data frame of observed
  transitions
}
\usage{
boot.transitions(transitions, iterations, by.stage.counts = FALSE, ...)
}

\arguments{
  \item{transitions}{ a stage-fate data frame with stage or age class in the
      current census,  fate in the subsequent census, and one or
      more fertility columns }
  \item{iterations}{ Number of bootstrap iterations }
  \item{by.stage.counts}{ Resample transitions with equal probability (default) or by subsets of initial stage counts }
  \item{...}{ additional options passed to \code{\link{projection.matrix}} }

}
%\details{  }


\value{
A list with 3 items
  \item{lambda }{A vector containing bootstrap values for lambda}
  \item{matrix }{A matrix containing bootstrap transtion matrices with one projection matrix per row.}
   \item{vector }{A matrix containing bootstrap stage vectors with one stage vector per row.}
 
}
\references{see Morris and Doak 2005 in \url{http://esapubs.org/Archive/mono/M075/004/appendix-A.htm} for resampling by stage class counts}
\author{ Chris Stubben }

\seealso{  \code{\link{projection.matrix}} }
\examples{


data(test.census)

## create stage-fate dataframe using merge and subset
test.trans <- subset(
             merge(test.census, test.census, by="plant", sort=FALSE),  
                     year.x==2001 & year.y==2002)

## format column and row names
test.trans<-test.trans[,c(1:4,6)]
colnames(test.trans)[2:5] <- c("year", "stage", "fruits", "fate")
rownames(test.trans) <- 1:nrow(test.trans)
# order stage columns corresponding to matrix
test.trans$stage <- ordered(test.trans$stage, levels = c("seedling", "vegetative", "reproductive"))


## add individual fertilities using prebreeding census with no seed bank 
##  based on the proportional reproductive outputs of flowering plants 
## and the total number of seedlings at the end of the projection interval

seedlings<-nrow(subset(test.census, year==2002 & stage=="seedling"))
test.trans$seedling<-test.trans$fruits/sum(test.trans$fruits) * seedlings

test.trans

## STEP by step instructions for bootstrapping dataframe

n<-nrow(test.trans)
n

x <- sample(n, replace=TRUE)
x

test.trans[x,]


## or respample by stage class counts 

 lapply(split(test.trans, test.trans$stage, drop=TRUE), 
      function(x) x[sample(nrow(x), replace=TRUE),])

## using boot.transitions

boot.transitions(test.trans, 5)

boot.transitions(test.trans, 5, by.stage=TRUE)


## Aquilegia example


data(aq.trans)

x<-subset(aq.trans, year==1996)

# calculate lamda, seed survival and recruitment rate using aq.matrix
rec<-nrow(subset(aq.trans, year==1997 & stage == "recruit"))
aq.96<- aq.matrix(x, rec)

# add  individual fertilities to data frame only
aq.96.trans<-aq.matrix(x, rec, summary=FALSE)

# pass estimated transitions in aq.96 to projection matrix 
aq.96.boot<-boot.transitions(aq.96.trans, 200, 
            add=c(1,1, aq.96$seed.survival, 2,1, aq.96$recruitment.rate) )

# calculate confidence intervals using quantile()
ci<- quantile(aq.96.boot$lambda, c(0.025,0.975) )

aq.96$lambda
ci

# plot histogram
hist(aq.96.boot$lambda, col="green", xlab="Lambda", 
        main=paste('Bootstrap estimates of population\ngrowth rate from 1996-1997'))
abline(v=ci, lty=3)



}
\keyword{ survey }
