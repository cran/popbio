\name{projection.matrix}
\alias{projection.matrix}

\title{ Construct projection matrix models }
\description{
  Construct an age or stage-structure projection model from a
  transition table listing stage in time \emph{t}, fate in time
  \emph{t+1}, and one or more individual fertility columns.
}
\usage{
projection.matrix(transitions, stage=NULL, fate=NULL, 
      fertility=NULL, sort=NULL,  add=NULL, TF=FALSE )

}

\arguments{
  \item{transitions}{ a stage-fate data frame with stage or age class in the
      current census,  fate in the subsequent census, and one or
      more fertility columns }
  \item{stage }{ a column name or position of the stage column in the
      stage-fate data frame.  Defaults to "stage"}
  \item{fate }{ name of the fate column in the stage-fate data frame.
      Defaults to "fate" }
  \item{fertility }{ one or more names of fertility columns in
      the stage-fate data frame.  By default, any column names matching names in the
      sort vector are assumed to contain individual fertilities}
  \item{ sort }{ a vector listing stage classes that correspond to the
      rows and columns of the desired projection matrix.  Default is
      to use \code{\link{levels}} in the stage column.  }

  \item{add}{ a vector listing row,column and value, used to add
      \emph{estimated} transtions to the transition matrix (e.g.,  a
      transition from seed bank to seedling).  May be repeated.}
  \item{TF}{ output separate transition (T) and fertility (F)
      matrices.  Default is to output single projection matrix A }
}
%\details{  }

\note{ Individual fertilities are the total number of offspring at the
  end of the census interval.  Therefore, fertilites should include
  offspring survival in a prebreeding censuses.  In a postbreeding
  census, new offspring were born just before the
  census, so the fertility rate is just the number of offspring in
  this case.


}
\value{
The default output is a single projection matrix A. If the TF flag is
true, then a list with 2 items where A=T+F
  \item{T }{Transition matrix}
  \item{F }{Fertility matrix} 
}
%\references{}
\author{ Chris Stubben }

%\seealso{    }
\examples{

data(test.census)

trans <- subset(merge(test.census, test.census, by = "plant",   sort =FALSE), 
                    year.x==2001 & year.y==2002 )
trans$seedferts <- trans$fruits.x/sum(trans$fruits.x) * 5

stages<-c("seedling", "vegetative", "reproductive")

## three ways to specify columns
projection.matrix(trans, stage=stage.x, fate=stage.y, fertility=seedferts, sort=stages)
projection.matrix(trans, 3, 6, 8, c(3,4,2))
projection.matrix(trans, "stage.x", "stage.y", "seedferts", stages)

## or use column defaults
names(trans)[c(3, 6, 8)] <- c("stage", "fate", "seedling")
# and order stages in dataframe
trans$stage<-ordered(trans$stage, stages)

projection.matrix(trans)
projection.matrix(trans, TF=TRUE)

## Example using Aquilegia data
data(aq.trans)
sf<- subset(aq.trans, year==1998 & plot==909, c(year, plant, stage, fruits, fate))

## seedlings next year
seedlings<-nrow(subset(aq.trans, plot==909 & year==1999 & stage=="recruit")) 

## ADD individual fetility estimates for recruits and seeds assuming seed bank and
## new seeds contribute to a common germinant pool with equal chance of recruitment

seed.survival<-.4
seed.bank.size<-1000
seeds.per.fruit<-50

seeds.from.plants<-sum(sf$fruits)*seeds.per.fruit
recruitment.rate<-seedlings/(seed.bank.size + seeds.from.plants)

## add two fertility columns
sf$recruit<- sf$fruits/sum(sf$fruits) * seeds.from.plants * recruitment.rate
sf$seed<-sf$fruits * seeds.per.fruit * seed.survival


## add seed bank survival and seed bank recruitment rate to transition matrix

A<-projection.matrix(sf, add=c(1,1, seed.survival, 2,1, recruitment.rate ))
A
max(Re(eigen(A)$values))


}
\keyword{ survey }
