\name{ltre}
\alias{ltre}
\title{ Life Table Response Experiment }
\description{
  Function to evaluate sensitivities in a fixed Life Table Response Experiment (LTRE).
}
\usage{
ltre(trts, ref)
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
\note{ The example of a fixed LTRE is from 

Amy Lauren Angert. 2006. Demography of central and marginal
populations of monkeyflowers (Mimulus cardinalis and
M. lewisii). Ecology 87:2014-2025.

Also see the example for variance decompostions in a random design, which
  may be added to the function later.

}
% \seealso{   }
\examples{
## Pooled Reference matrix for Mimulus cardinalis kindly provided
## by Amy Angert 1/2/2008.
Aref<-matrix( c(
1.99e-01, 8.02e+02, 5.82e+03, 3.05e+04,
2.66e-05, 7.76e-02, 2.31e-02, 1.13e-03,
7.94e-06, 8.07e-02, 3.22e-01, 2.16e-01,
2.91e-07, 1.58e-02, 1.15e-01, 6.01e-01), byrow=TRUE, nrow=4)

## single pooled matrix for Buck Meadows
data(monkeyflower)
Atrt <- as.matrix(subset(monkeyflower,  species=="cardinalis" &
site=="Buck Meadows" & year=="pooled", select=c(4:19)), nrow=4, byrow=TRUE)
Atrt<-matrix(Atrt, nrow=4, byrow=TRUE)

## matrix of differences (Caswell p 262)
Dm<-Atrt-Aref
Dm
## matrix halfway between treatment and reference
Ac<-(Atrt+Aref)/2
Ac
## sensitivity of midway matrix Ac
SAc<-eigen.analysis(Ac)$sensitivities
SAc
## matrix of contributions
Cm<-Dm*SAc
Cm
sum(Cm)

##  OR using ltre function
Cm <-ltre(Atrt, Aref)
Cm
sum(Cm)

## Population effects using pooled population matrices for treatments 
## Plot like Figure 2A in Angert (2006)
card<-subset(monkeyflower,  species=="cardinalis" & year=="pooled")
## split matrices in rows into  list of 4 matrices 
Atrt<-lapply(split(as.matrix(card[,4:19]), 1:4),  matrix, nrow=4, byrow=TRUE)
names(Atrt)<-card$site
Cm<-ltre(Atrt, Aref)
x<-sapply(Cm, sum)
x
names(x)<-c("BU", "RP", "WA", "CA")

op<-par(mar=c(5,5,4,1))
barplot(x, xlab="Population", ylab="" , ylim=c(-.4, .4), xlim=c(0,6.5), las=1, space=.5, col="blue")
abline(h=0)
mtext(expression(paste(sum(a[ij]), " contributions")), 2, 3.5)
title(expression(italic("M. cardinalis")))

## and Plot like figure 3A
x<-matrix(unlist(Cm), nrow=4, byrow=TRUE)
colnames(x)<-paste("a", rep(1:4, each=4), 1:4, sep="")
bp<-barplot(x[1:2,], beside=TRUE, ylim=c(-.2,.2), las=1,
xlab="Transition", ylab="", xaxt='n')
mtext(expression(paste("Contribution of ", a[ij], "to variation in ", lambda)), 2, 3.5)
## rotate labels
text(bp[1,]-0.5, -.22, labels=colnames(x), srt=45, xpd=TRUE)
par(op)


#######  Random design and variance decomposition

data(whale)
pods<- whale$pods
## Covariance matrix
p1<-matrix(unlist(pods), nrow=18, byrow=TRUE)
# addcolumn names
colnames(p1)<- paste("a", rep(1:4,each=4), 1:4, sep="")

## re-order columns to plot matrix by columns (not by rows)
x<- order(paste("a", 1:4, rep(1:4,each=4), sep=""))
p1<-p1[,x]
covmat<- cov(p1)

## plots matching figure 3 in Brault & Caswell 1993 or figure 10.10 in Caswell 2001 (p 272)
persp(covmat, theta=45, phi=15, box=FALSE)


w1<- matrix(apply(p1, 2, mean), nrow=4)
wS<-eigen.analysis(w1, zero=FALSE)$sensitivities
## V matrix of contributions
contmat <- covmat * c(wS) %*% t(c(wS))
persp(contmat, theta=45, phi=15, box=FALSE)

## contributions of V associated with aij  (matrix on page 271 in Caswell)
A<-matrix(apply(contmat, 2, mean), nrow=4)
round(A/sum(A),3)


}

\keyword{ survey }

