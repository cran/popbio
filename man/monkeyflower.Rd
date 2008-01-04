\name{monkeyflower}
\alias{monkeyflower}
\docType{data}
\title{Projection matrices for monkeyflower}
\description{
  Pooled and annual projection matrices of central and marginal
  populations of monkeyflowers (\emph{Mimulus cardinalis} and
  \emph{M. lewisii})
}

\usage{data(monkeyflower)}
\format{
  A data frame with 32 projection matrices, arranged with one matrix per row
   \describe{
    \item{\code{species}}{M. cardinalis or M. lewisii}
    \item{\code{site}}{Study site}
    \item{\code{year}}{Start year of projection interval or pooled for all three years}
    \item{\code{a11}}{matrix element a11;  seed to seed transition or seed bank survival}
    \item{\code{a12}}{matrix element a12;  small nr to seed - fertility}
    \item{\code{a13}}{matrix element a13;  large nr to seed - fertility}
    \item{\code{a14}}{matrix element a14;  reprod to seed - fertility}
    \item{\code{a21}}{matrix element a21;  seed to small nr - growth}
    \item{\code{a22}}{matrix element a22;  small nr to small nr -stasis}
    \item{\code{a23}}{matrix element a23;  large nr to small nr - regress}
    \item{\code{a24}}{matrix element a24;  reprod to small nr - regress}
    \item{\code{a31}}{matrix element a31;  seed to large nr - growth  }
    \item{\code{a32}}{matrix element a32;  small nr to large nr - growth  }
    \item{\code{a33}}{matrix element a33;  large nr to large nr - stasis  }
    \item{\code{a34}}{matrix element a34;  reprod to large nr - regress  }
    \item{\code{a41}}{matrix element a41;  seed to reprod - growth  }
    \item{\code{a42}}{matrix element a42;  small nr to reprod - growth  }
    \item{\code{a43}}{matrix element a43;  large nr to reprod - growth  }
    \item{\code{a44}}{matrix element a44;  reprod to reprod - stasis  }
  }
}
\details{
 Matrix constructed using a post-breeding census with four stage
 classes: Seeds, small non-reproductive, large non-reproductive, and
 reproductive.

}
\source{
  \url{http://www.esapubs.org/archive/ecol/E087/126/appendix-E.htm}
}
\references{
   Amy Lauren Angert. 2006. Demography of central and marginal
   populations of monkeyflowers (\emph{Mimulus cardinalis} and
   \emph{M. lewisii}). Ecology 87:2014-2025.
}
\examples{
data(monkeyflower)
A <- subset(monkeyflower, 
           species=="cardinalis" & site=="Carlon" & year=="pooled",  
           select=c(4:19))
stages<-c("seed", "sm.nr", "lg.nr", "repro")
A
class(A)

## Convert dataframe to matrix or vector first -either one works 
## matrix(as.matrix(A), nrow=4, byrow=TRUE, dimnames=list(stages, stages) )
A <-matrix(unlist(A), nrow=4, byrow=TRUE, dimnames=list(stages, stages) )
A
a<-eigen.analysis(A)
a

barplot(a$stable.stage, col="green", ylim=c(0,1), 
       ylab="Stable stage proportion", xlab="Mimulus stage class")
box()

}
\keyword{datasets}
