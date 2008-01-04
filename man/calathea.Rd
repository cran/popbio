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
calathea[[1]]
names(calathea)
calathea[["plot1 1983"]]

## all plot 1
calathea[1:4]

## all 1982 matrices 
calathea[seq(1,16,4)]
## all 1983
## calathea[seq(1,16,4)+1]

## MEAN matrix
matrix(apply(matrix(unlist(calathea), nrow=16, byrow=TRUE), 2, mean), nrow=8)

## Growth rates -see Figure 7
x<- matrix(sapply(lapply(calathea, eigen.analysis), '[[', "lambda1"), nrow=4)
matplot(x, type='l', las=1, xlim=c(0.7,4.3), ylim=c(.7,1.3), yaxs='i', xaxt='n', ylab="Growth rate")
axis(1, 1:4, paste(82:85, "-", 83:86, sep=""))

## Damping ratios -see Table 12
x<- matrix(sapply(lapply(calathea, eigen.analysis), '[[', "damping.ratio"), nrow=4)
dimnames(x)<-list(paste(1982:1985, "-", 1983:1986, sep=""), paste("plot", 1:4))
x
}
\keyword{datasets}
