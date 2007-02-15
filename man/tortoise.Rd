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
   Table 5 in Doak et al (1994).  Used by Caswell (2001) in chapter 9 on sensistivity analsysis.
}
\references{
Caswell, H. 2001. Matrix population models: construction, analysis, and interpretation, Second edition. Sinauer, Sunderland, Massachusetts, USA.

Doak, D., P. Kareiva, and B. Kleptetka. 1994. Modeling population viability for the desert tortoise in the Western Mojave Desert. Ecological Applications 4:446-460.

}
\examples{
data(tortoise)
A<-tortoise$T + tortoise$F.med.high

Re(eigen(A)$values[1])


y<-numeric(4)
for (i in 2:5)
{
A<-tortoise$T+tortoise[[i]]
y[i-1]<-Re(eigen(A)$values[1])
}
data.frame(row.names=names(tortoise)[2:5], lambda=y)
}

\keyword{datasets}
