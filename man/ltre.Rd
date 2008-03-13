\name{LTRE}
\alias{LTRE}
\title{ Life Table Response Experiment }
\description{
  Function to evaluate sensitivities in a fixed Life Table Response Experiment (LTRE).
}
\usage{
LTRE(trts, ref)
}
\arguments{
  \item{trts}{ A treatment matrix or a list of two or more treatment matrices }
  \item{ref}{ A reference matrix }
}
\details{
 Sensitivities are evaluated midway between the treatment and
 reference matrices as described in section 10.1.1 in Caswell (2001).
}
\value{
  A matrix of contributions (equation 10.4 in Caswell) or a list of
  matrices with one matrix of contributions per treatment

}
\references{ 
Caswell, H. 2001. Matrix population models: construction, analysis,
and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA.


 }
\author{ Chris Stubben }
\note{ The examples of a fixed LTRE are from 

Horvitz, C. C., D. W. Schemske, and H. Caswell. 1997. The
relative importance of life-history stages to population
growth: prospective and retrospective analyses. Pages 247-271 in S. Tuljapurkar and H. Caswell, editors. Structured
population models in marine, terrestrial and freshwater systems.
Chapman and Hall, New York.

A.L. Angert. 2006. Demography of central and marginal
populations of monkeyflowers (Mimulus cardinalis and
M. lewisii). Ecology 87:2014-2025.

The example below also includes variance decomposition in a random
design using killer whale from Caswell (2001).

}
% \seealso{   }
\examples{
#######  Calathea ovandensis
data(calathea)
## Pooled matrix, all plots and years in Table 8 in Horvitz and Schemske (1995).
calathea_pool <- matrix(c(
0.4983, 0,      0.5935, 7.139, 14.2715, 24.6953, 34.9027, 40.5437,
0.0973, 0.0110, 0.0191, 0,      0,       0,       0,      0,
0.0041, 0.0442, 0.3378, 0.0698, 0.0251,  0.0065,  0.0085, 0,
0,      0.0014, 0.1355, 0.4286, 0.1736,  0.0968,  0.0427, 0.0435,
0,      0,      0.0363, 0.3841, 0.6025,  0.4258,  0.2991, 0.2174,
0,      0,      0.0019, 0.0254, 0.113,   0.2387,  0.1709, 0.2826,
0,      0,      0,      0.0095, 0.0272,  0.1548,  0.3248, 0.1957,
0,      0,      0,      0.0032, 0.0063,  0.0452,  0.1282, 0.2391)
, nrow=8, byrow=TRUE)

## Create plots like FIGURE 7 in Horvitz et al 1997
##PLOTS
plots<- split(calathea, rep(1:4,each=4))
## use Mean matrix since pooled not available
plots<- lapply(plots, mean)
Cm<-LTRE(plots, calathea_pool)
pe<-sapply(Cm, sum)
barplot(pe, xlab="Plot", ylab="Plot effect" , ylim=c(-.25, .25),
col="blue", las=1)
abline(h=0)
box()
title(expression(italic("Calathea ovandensis")))

##YEARS -- split recycles vector
yrs<-split(calathea, 1:4)
yrs <- lapply(yrs, mean)
names(yrs)<-1982:1985
Cm<-LTRE(yrs, calathea_pool)
ye<-sapply(Cm, sum)
barplot(ye, xlab="Year", ylab="Year effect" , ylim=c(-.25, .25), col="blue", las=1)
abline(h=0)
box()
title(expression(italic("Calathea ovandensis")))

## INTERACTION
Cm<-LTRE(calathea, calathea_pool)
ie<-sapply(Cm, sum)
## minus plot, year effects
ie<- ie - rep(pe, each=4) - rep(ye, 4)
names(ie)<-NULL
names(ie)[seq(1,16,4)]<-1:4
barplot(ie, xlab="Plot (years 82-83 to 85-86)", ylab="Interaction effect" , ylim=c(-.25, .25), col="blue", las=1)
abline(h=0)
box()
title(expression(italic("Calathea ovandensis")))


#######  Mimulus 
## Pooled M. cardinalis reference matrix kindly provided by Amy Angert 1/2/2008.
m_card_pool<-matrix( c(
1.99e-01, 8.02e+02, 5.82e+03, 3.05e+04,
2.66e-05, 7.76e-02, 2.31e-02, 1.13e-03,
7.94e-06, 8.07e-02, 3.22e-01, 2.16e-01,
2.91e-07, 1.58e-02, 1.15e-01, 6.01e-01), byrow=TRUE, nrow=4)

## Population effects using pooled population matrices 
data(monkeyflower)
card<-subset(monkeyflower,  species=="cardinalis" & year=="pooled")
## split rows into list of 4 matrices 
Atrt<-lapply(split(as.matrix(card[,4:19]), 1:4),  matrix, nrow=4, byrow=TRUE)
names(Atrt)<-card$site
Cm<-LTRE(Atrt, m_card_pool)
x<-sapply(Cm, sum)
x
names(x)<-c("BU", "RP", "WA", "CA")

## Plot like Figure 2A in Angert (2006)
op<-par(mar=c(5,5,4,1))
barplot(x, xlab="Population", ylab="" , ylim=c(-.4, .4), xlim=c(0,6.5), las=1, space=.5, col="blue")
abline(h=0)
mtext(expression(paste(sum(a[ij]), " contributions")), 2, 3.5)
title(expression(paste(italic("M. cardinalis"), " Population effects")))
box()

## and Plot like Figure 3A
x<-matrix(unlist(Cm), nrow=4, byrow=TRUE)
colnames(x)<-paste("a", rep(1:4, each=4), 1:4, sep="")
bp<-barplot(x[1:2,], beside=TRUE, ylim=c(-.2,.2), las=1,
xlab="Transition", ylab="", xaxt='n')
mtext(expression(paste("Contribution of ", a[ij], "to variation in ", lambda)), 2, 3.5)
## rotate labels
text(bp[1,]-0.5, -.22, labels=colnames(x), srt=45, xpd=TRUE)
title(expression(paste(italic("M. cardinalis"), " Range center")))
box()
par(op)


#######  Random design and variance decomposition
data(whale)
pods<- whale$pods
## Covariance matrix
p1<-matrix(unlist(pods), nrow=18, byrow=TRUE)
# addcolumn names
colnames(p1)<- paste("a", rep(1:4,each=4), 1:4, sep="")
## re-order columns to plot matrix by columns 
x<- order(paste("a", 1:4, rep(1:4,each=4), sep=""))
p1<-p1[,x]
covmat<- cov(p1)

## plots matching figure 3 in Brault & Caswell 1993 or figure 10.10 in Caswell 2001 (p 272)
persp(covmat, theta=45, phi=15, box=FALSE,
 main=expression("Killer Whale Covariances"))
w1<- matrix(apply(p1, 2, mean), nrow=4)
wS<-eigen.analysis(w1, zero=FALSE)$sensitivities
## V matrix of contributions
contmat <- covmat * c(wS) %*% t(c(wS))
persp(contmat, theta=45, phi=15, box=FALSE,
main=expression(paste("Contributions to V(", lambda, ")")) )
## contributions of V associated with aij  (matrix on page 271 in Caswell)
A<-matrix(apply(contmat, 2, mean), nrow=4)
round(A/sum(A),3)
}
\keyword{ survey }

