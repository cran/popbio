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
## Summed elasticities.  Growth in bottom-left triangle, stasis on diagonal, and
# fertility in a prebreeding census for a monocarpic plant is in last column.
elas<-tea$elasticities
elas
el<-c(fertility=sum(elas[,6]), stasis=sum(diag(elas)), 
         growth=sum(elas[row(elas)>col(elas)]))
el
## Some plotting examples for sensitivity/elasticity matrices
tea$sensitivities

##  also try dotplot in lattice package
matplot(t(tea$sensitivities), las=1, 
 xlab="Stage at time t", ylab="Sensitivity",
 main="Sensitivity matrix using matplot #1")
legend(4,45, colnames(elas), pch=as.character(1:6), col=1:6,
title="Fate at time t+1")
## log-scale
matplot(t(tea$sensitivities), log='y', yaxt='n', type='b',
xlab="Stage at time t",  ylab="Sensitivity (log scale)",
main="Sensitivity matrix using matplot #2")
axis(2, at=c(0.0001, 0.01, 1), labels=expression(10^-4, 10^-2, 10^0) ,
las=1)


# Matrix plots like figure 9.4 in Caswell (no 3d barplot in R) 

## image plot with color key 
def.par <- par(no.readonly = TRUE) # save default, for resetting
layout(matrix(c(1,2), nrow=1), wid=c(1,3))
clrs<-heat.colors(24)
z<-range(log10(tea$sensitivities))
## key
plot(c(0,1), z, xaxs="i", type = "n", bty="n", xaxt="n", 
xlab="", ylab="Log10 sensitivity", las=1, cex.lab=0.9)
i <- seq(z[1], z[2],length= length(clrs)+1 )
rect(0, i[-(length(clrs)+1)], 1, i[-1], col=clrs, border=NA)
rect(0,z[1], 1, z[2], lwd=2)
# plot
image(1:6, 1:6, t(log10(tea$sensitivities)[6:1,]), col=clrs, yaxt='n',
   xlab = "Stage at time t", ylab = "Fate at time t+1")
axis(2, 1:6, 6:1, las=1)
box()
par(xpd=NA)
text(2,7, expression(bold("Sensitivity matrix using image")), cex=1.2)
par(def.par)


plot(log10(c(tea$sensitivities)), type="s", ylim=c(-5,2), las=1,
xlab="Stage at time t", xaxt="n", 
ylab=expression(paste(Log[10], " sensitivity of ",
lambda)), main="Sensitivity matrix using stair-step")
axis(1, seq(1,36,6), 1:6)
text(log10(c(tea$sensitivities)), cex=.7, pos=3, 
     labels=paste(" ", 1:6, rep(1:6,each=6), sep=""))


}
\keyword{datasets}
