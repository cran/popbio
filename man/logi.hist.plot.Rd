\name{logi.hist.plot}
\alias{logi.hist.plot}

\title{ Plot logistic regression  }
\description{ Plot combined graphs for logistic regressions 
  }
\usage{
logi.hist.plot(independ, depend, logi.mod = 1, type = "dit", 
boxp = TRUE, rug = FALSE, ylabel = "Probability", ylabel2 = "Frequency", 
xlabel = "", mainlabel = "", las.h = 1, counts = FALSE, ...)
}

\arguments{
  \item{independ}{ explanatory variable }
  \item{depend}{ dependent variable, typically a logical vector }
  \item{logi.mod}{ type of fitting, 1 = logistic; 2 = "gaussian" logistic }
  \item{type}{ type of representation, "dit" = dit plot; "hist" = histogram}
  \item{boxp}{ TRUE = with box plots, FALSE = without}
  \item{rug}{ TRUE = with rug plots, FALSE = without }
  \item{ylabel}{ y-axis label }
  \item{ylabel2}{ 2nd y-axis label }
  \item{xlabel}{ x-axix label }
  \item{mainlabel}{ overall title for plot }
  \item{las.h}{ orientation of axes labels (0 = vertical, 1 = horizontal }
  \item{counts}{ add counts above histogram bars }
  \item{\dots}{ additional options passed to logi.hist }
}
%\details{}
\value{
  A logistic regression plot
}
\references{ de la Cruz Rot, M. 2005. Improving the Presentation of Results of Logistic Regression with R.  ESA Bulletin 86:41-48.

\url{http://esapubs.org/bulletin/backissues/086-1/bulletinjan2005.htm}

 }
\author{ M. de la Cruz Rot }
\note{ Added options for axis labels }
\examples{

data(aq.trans)

aq.trans$survived<-aq.trans$fate!="dead"

a<-subset(aq.trans, leaf<50 & stage!="recruit", c(leaf,survived))

logi.hist.plot(a$leaf,  a$survived, 
type="hist", boxp=FALSE, counts=TRUE, las.h=0, int=10, 
ylabel="Survival probability", ylabel2="Number of plants", 
 xlab="Number of leaves" )



b<-glm(survived ~ leaf, binomial, data=a)
 summary(b)


}
\keyword{ survey }