\name{calathea}
\alias{calathea}
\docType{data}
\title{  Projection matrices for a tropical understory herb}
\description{
 Projection matrices for a tropical understory herb (\emph{Calathea
   ovandensis}) for plots 1-4 and years 1982-1985. 
}
\usage{data(calathea)}
\format{
  A list of 16 matrices ordered by plot then year.

}
\details{
   A projection matrix constructed using a post-breeding census with 8 size classes: seed, seedling, juvenile,
   pre-reproductive, and 4 reproductive classes divided by leaf area. 
}
\source{
  Table 7 in Horvitz and Schemske (1995).
}
\references{
  Horvitz, C.C. and D.W. Schemske. 1995.  Spatiotemporal variation in
  demographic transitions of a tropical understory herb: Projection
  matrix analysis.  Ecological Monographs 65:155-192.
}
\examples{
data(calathea)
## Single matrix
calathea[[11]]
image2(calathea[[11]], text.cex=.8)
title( paste("Calathea", names(calathea[11])), line=3)

## MEAN matrix (pooled matrix in table 8 - see LTRE example)
mean(calathea)

## all plot 1
calathea[1:4]
## all 1982 matrices 
calathea[ grep("1982", names(calathea)) ]
# OR
# calathea[seq(1,16,4)]
# split(calathea, 1:4)[[1]]

## Growth rates -see Figure 7
x<- matrix(sapply(lapply(calathea, eigen.analysis), '[[', "lambda1"), nrow=4)
colnames(x)<-paste("Plot", 1:4)
matplot(x, type='l', las=1, xlim=c(0.7,4.3), ylim=c(.7,1.3), yaxs='i', 
xaxt='n', ylab="Growth rate")
axis(1, 1:4, paste(82:85, "-", 83:86, sep=""))
legend(1.8,1.3, paste("Plot", 1:4), col=1:4, lty=1:4, ncol=2, bty='n')


## Damping ratios -see Table 12
#x<-matrix(sapply(lapply(calathea, eigen.analysis), '[[',"damping.ratio"), nrow=4)
x<-matrix(sapply(calathea, eigen.analysis)["damping.ratio",], nrow=4)
dimnames(x)<-list(paste(1982:1985, "-", 1983:1986, sep=""), paste("plot", 1:4))
x
}
\keyword{datasets}
