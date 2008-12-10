\name{image2}
\alias{image2}
\title{ Display a matrix image }
\description{
  Creates a grid of colored rectangles to display a projection,
  elasticity, sensitivity or other matrix.
}
\usage{
image2(x, col = c("white", rev(heat.colors(23))), breaks, log = TRUE,
 border = NA, box.offset = 0, round = 3, cex, text.cex = 1, 
 text.col = "black", mar = c(1, 3, 3, 1), 
 labels = 2:3, label.offset = 0.2, label.cex = 1)
}
\arguments{
  \item{x}{ A numeric matrix with row and column names }
  \item{col}{ A vector of colors for boxes}
  \item{breaks}{  A numeric vector of break points or number of
      intervals into which \code{x} is to be \code{\link{cut}}.
      Default is the length of \code{col}  }
  \item{log}{ Cut values in \code{x} using a log scale, default TRUE }
  \item{border}{ The border color for boxes, default is no borders }
  \item{box.offset}{ Percent reduction in box size (a number between 0
    and 1), default is no reduction }
  \item{round}{ Number of decimal places to display values of \code{x}
    in each box}
  \item{cex}{ Magnification size of text and labels, if specified this will replace
      values in both text.cex and label.cex  }
  \item{text.cex}{ Magnification size of text in cells only }
  \item{text.col}{ Color of text in cells, use NA to skip
    text labels }
  \item{mar}{ Margins on four sides of plot  }
  \item{labels}{ A vector giving sides of the plot (1=bottom, 2=left,
      3=top, 4=right) for row and column labels }
  \item{label.offset}{ Amount of space between label and boxes }
  \item{label.cex}{ Magnification size of labels }

}
\details{
  The minimum value in \code{x} is usually assigned to the first color
category and the rest of the values are then cut into equally spaced
intervals.  This was added to show transitions with
very low probabilities in a new color category, eg, 2e-06 would
ususally be grouped with 0 using \code{\link{image}}. Note if all
elements > 0, then the first color will not be used.

}
\value{ A image plot of the matrix in \code{x}}

%\references{ ~put references to the literature/web site here ~ }
\author{ Chris Stubben }
%\note{  }
\seealso{  \code{\link{image}}  }
\examples{
data(calathea)

op<-par(mfrow=c(2,2))
image2(calathea[[11]], text.cex=.8)
## labels on bottom right
image2( calathea[[11]], text.cex=.8, labels=c(1,4), mar=c(3,1,1,3))
## no text
image2( calathea[[11]], text.col=NA)
## with smaller boxes and gray border
image2( calathea[[11]], text.cex=.8, box.offset=0.2, border="gray70")


## if comparing two or more matrices, get the log10 range
## of values (not including zero) and pass to breaks
x<-unlist(calathea)
x<-log10(range(x[x!=0]))
par(mfrow=c(4,4))
for(i in 1:16)
  {
image2( calathea[[i]], cex=.7, breaks=seq(x[1], x[2], len=24))
  title(names(calathea[i]), line=3)
}
par(op)

## sometimes log scale is not needed
data(tortoise)
A<-tortoise$T + tortoise$F.med.high
image2(A, log=FALSE, box.offset=0.1)
}

\keyword{ color }
