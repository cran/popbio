\name{01.Introduction}
\alias{01.Introduction}
\alias{popbio}
\title{Introduction to the popbio Package} 
\description{ 
Popbio is a package for the construction and analysis of matrix
population models.   An overview of functions grouped by purpose is contained in the numbered sections
at the top of the \link[popbio:00Index]{popbio contents page}.
First, the popbio package consists  of the \R translation of
\code{Matlab} code found in Caswell (2001) or Morris and Doak
(2002).  A detailed description of converted functions for the
analysis of projection matrix models within each book can be accessed within R
using \code{help(Caswell)} and \code{help(Morris)},  or by following
the links to  \link{02.Caswell} and \link{03.Morris} from the help content pages.

Second, the \code{popbio} package includes a number of functions such
as \code{\link{projection.matrix}} to construct projection matrices
from raw census data typically collected in plant demography studies
(i.e., when vital rates can be estimated directly from annual censuses
of tagged individuals using transitioin frequency tables).  To
estimate vital rates in animal demography using capture-recapture
methods, try the \code{Rcapture} package instead.

Finally, many items within the alphabetical index of 
help pages are sample datasets consisting of either published
projection matrices or annual census data from demography studies.




}

\references{ 
Caswell, H. 2001. Matrix population models: Construction,
analysis, and interpretation, Second edition. Sinauer, Sunderland,
Massachusetts, USA. 

Morris, W. F., and D. F. Doak. 2002. Quantitative conservation
biology: Theory and practice of population viability
analysis. Sinauer, Sunderland, Massachusetts, USA.

}


\author{Chris Stubben}

\keyword{documentation}
