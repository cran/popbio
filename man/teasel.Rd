\name{teasel}
\alias{teasel}
\docType{data}
\title{T and F matrix for teasel}
\description{
  Transition T and Fertility F matrices for the plant teasel
}
\usage{data(teasel)}
\format{
  A list with T and F matrices 
}
\source{
   Example 5.2
}
\references{
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.
}
\examples{
data(teasel)
A<-teasel$T + teasel$F
A
tea<-eigen.analysis(A, zero=FALSE)
tea$lambda
## Summed elasticities?  Growth in bottom-left triangle, stasis on diagonal, and
# fertility in a prebreeding census for a monocarpic plant is in last column.
elas<-tea$elasticities
elas
el<-c(fertility=sum(elas[,6]), stasis=sum(diag(elas)), 
         growth=sum(elas[row(elas)>col(elas)]))
el

tea$sensitivities
# Matrix plots like figure 9.4 in Caswell.  No 3d barplot in R yet.  Note log10 scale.
op<-par(oma=c(1,2,4,0))
heatmap(log10(tea$sensitivities[6:1, ]), Rowv = NA, 
    Colv = NA, scale = "none", margin = c(8,8), 
   ylab = "Fate",     xlab = "Stage", 
   # col = gray(0:24 / 24))
     col = heat.colors(24))
mtext("Sensitivity matrix for teasel", 3, outer=TRUE, line=1, cex=1.5)
par(op)

plot(log10(c(tea$sensitivities)), type="s", ylim=c(-5,2),
xlab="Stage", ylab=expression(paste(Log[10], " sensitivity of ", lambda)), xaxt="n")
axis(1, seq(1,36,6), 1:6)
text(log10(c(tea$sensitivities)), cex=.7, pos=3, 
     labels=paste(" ", 1:6, rep(1:6,each=6), sep=""))


}
\keyword{datasets}
