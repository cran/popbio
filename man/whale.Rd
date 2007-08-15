\name{whale}
\alias{whale}
\docType{data}
\title{T and F matrix for killer whale}
\description{
  Transition and Fertility matrices for killer whales
}
\usage{data(whale)}
\format{
  A list with T and F matrices 
  }
}
\source{
 see Chapter 5 in Caswell (2001)
}
\references{
Brault, S., and H. Caswell. 1993. Pod-specific demography of killer whales (\emph{Orcinus orca}). Ecology 74:1444-1454.

Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA.
}
\examples{
data(whale)
A <- whale$T + whale$F
A
n <- c(4, 38, 36, 22)

## matrix multiplication (see pop.projection)
A \%*\% n
A \%*\% A \%*\% n

######### section 5.3.1 Age-specific survival  ###########
#  equation 5.35
fundamental.matrix(whale$T)$N

# Survivorship plot like figure 5.1 in Caswell.  
# Note example on page 120 uses matrix powers and not element by element 
# which is R default.  Matrix power is not part of base R, but for simple cases 
# this works to do A \%*\% A \%*\% A \%*\% A...

mp<-function(A,pow){
  if(pow==1){A}
  else{
    x<-A
    for(i in (2:pow)){
      A<-x\%*\%A
    }
  }
  A
}

## also use colSums for sum of matrix columns e^T
surv<-matrix(numeric(150*4), ncol=4)
for(x in 1:150)
{
   surv[x,]<-colSums(mp(whale$T,x))
}
## Just plot first stage column?
plot(surv[,1]/surv[1,1], type="l", ylim=c(0,1), las=1, 
  xlab="Age (years)", ylab=expression(paste("Survivorship ", italic(l(x)))))


######### section 5.3.2 Age-specific fertility  ###########
#  equation 5.44
T<- mp(whale$T,20)
whale$F \%*\% T \%*\% diag(1/colSums(T))

##  Figure 5.2 in Caswell
fert<-numeric(200)
for(x in 1:200)
{
  T<-mp(whale$T,x)
  phi<-whale$F \%*\% T \%*\% diag(1/colSums(T))
  fert[x]<-phi[1,1]
}

plot(fert, type="l", ylim=c(0,0.07), las=1,
  xlab="Age (years)", ylab="Age-specific fertility")

}
\keyword{datasets}
