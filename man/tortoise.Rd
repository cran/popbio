\name{tortoise}
\alias{tortoise}
\docType{data}
\title{ T and F matrices for desert tortoise}
\description{
   Transition matrix and fertility estimates for the desert tortoise \emph{Gopherus agassizii}
}
\usage{data(tortoise)}
\format{
 A list of 1 average transition matrix (T) and 4 fertility matrix estimates (low, medium low, medium high, and high)

}
\source{
   Table 5 in Doak et al (1994).  Used by Caswell (2001) in chapter 9 on sensitivity analysis.
}
\references{
Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA.

Doak, D., P. Kareiva, and B. Kleptetka. 1994. Modeling population viability for the desert tortoise in the Western Mojave Desert. Ecological Applications 4:446-460.

}
\examples{
data(tortoise)
A<-tortoise$T + tortoise$F.med.high
A

tor<-eigen.analysis(A, zero=FALSE)
tor$lambda

## All 4 growth rates (first add T to all four F matrices)
tor4<-lapply(tortoise[2:5], "+", tortoise$T)
sapply(tor4,  function(A){Re(eigen(A)$values[1])}   )

## Summed elasticities, example 9.4.  Growth on subdiagonal
elas<-tor$elasticities
elas
el<-c(F=sum(elas[1,]), P=sum(diag(elas)), G=sum(elas[row(elas)==col(elas)+1]))
el

## Triangle plot like figure 9.11 in Caswell.  
## Nicer triangle plots are found in many different packages.
plot(c(0, 1, 2, 0), c(0, sqrt(3), 0, 0), type = "l", lwd = 2, 
 xlab = "", ylab = "", axes = FALSE, sub="Summed elasticities")
text(c(0, 2, 1), c(0,0, sqrt(3)), names(el), cex = 1.5, pos=c(1,1,3), xpd=TRUE)
points(2 - 2 * el[1] - el[3], el[3] * sqrt(3), cex=1.5)


## Senstivity plot like figure 9.3 in Caswell 
## use text to add labels closer to x-axis

sens<-tor$sensitivities
op<- par(mfrow=c(3,1), mar = c(1.5, 4.5, 1, 2) ,  oma=c(1.5,0,1.5,0), cex=1.2 )

## F in top row
ep<-barplot(sens[1,], ylim=c(0,.4), col="white", las=1,  
   ylab=expression(paste("to ", italic(F[i]))), names="")
box()
 text(ep, -.05,  1:8, xpd = TRUE)

## P on diagonal
ep<-barplot(diag(sens), ylim=c(0,.4), col="white", las=1, 
   ylab=expression(paste("to ", italic(P[i]))), names="")
box()
 text(ep, -.05,  1:8, xpd = TRUE)

# G on subdiagonal
barplot(c(sens[row(sens)==col(sens)+1],0), ylim=c(0,.4), col="white", las=1, 
   ylab=expression(paste("to ", italic(G[i]))) )
box()
 text(ep, -.05,  c(1:7, NA), xpd = TRUE)

mtext(expression(paste("Sensitivity of ", lambda, "...")), 3, outer=TRUE, cex=1.4)
mtext(expression(paste("Size class ", italic(i))), 1, outer=TRUE, cex=1.2)

par(op)

}

\keyword{datasets}
