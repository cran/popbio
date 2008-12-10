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
tea$lambda1
## Elasticities
elas<-tea$elasticities
elas
image2(elas, mar=c(1,3.5,5,1), label.off=0.05)
 title("Elasticity matrix", line=2.5)
## Summed elasticities.  Growth in bottom-left triangle, stasis on diagonal, and
# fertility in a prebreeding census for a monocarpic plant is in last column.
el<-c(fertility=sum(elas[,6]), stasis=sum(diag(elas)), 
         growth=sum(elas[row(elas)>col(elas)]))
el
## Sensitivities
sens<-tea$sensitivities
sens

## with smaller boxes using box.offset option
image2(sens, mar=c(1,3.5,5,1), label.off=0.05, box.offset=.1)
 title("Sensitivity matrix", line=2.5)

## other plot options
log10lab<-expression(paste(Log[10], " sensitivity of ",lambda))
# Stair step plot like  figure 9.4 in Caswell 
plot(log10(c(sens)), type="s", las=1, ylim=c(-5, 2),
xlab="Stage at time t", xaxt="n",
ylab=log10lab, 
main="Sensitivity matrix using stair-step")
axis(1, seq(1,36,6), 1:6)
text(log10(c(sens)), cex=.7, adj=c(0,-.3), 
     labels=paste(" ", 1:6, rep(1:6,each=6), sep=""))

##  or using matplot
matplot(t(sens), log='y', type='b', yaxt='n',
xlab="Stage at time t",  
ylab=log10lab,
main="Sensitivity matrix using matplot")
axis(2, at=10^(-5:1), labels=-5:1, las=1)

}
\keyword{datasets}
