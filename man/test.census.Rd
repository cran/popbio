\name{test.census}
\alias{test.census}
\docType{data}
\title{Census data for hypothetical plant}
\description{
   Three years of census data for a hypothetical plant with three stage classes.
}
\usage{data(test.census)}
\format{
  A data frame with 41 census observations on the following variables
    \describe{
    \item{\code{plant}}{Plant id number}
    \item{\code{year}}{Year of census}
    \item{\code{stage}}{Stage class: seedling, vegetative, or reproductive}
    \item{\code{fruits}}{Total number of fruits}
  }
  }

\examples{
data(test.census)

 test.stage <- c("seedling", "vegetative", "reproductive")

## Cross-tabulate stage vectors and order rows by stage
sv<- table(test.census$stage, test.census$year)[test.stage,]
sv
stage.vector.plot(sv)

## set xaxt='n' to avoid fractions of a year (2002.5)
stage.vector.plot(sv, prop=FALSE, xaxt="n")
 axis(1, 2001:2003, c(2001, 2002, 2003))


## Convert census data to state-fate transition table using reshape

reshape(test.census, direction="wide", idvar="plant", timevar="year")


## Convert census data  to state-fate transition table using merge

trans <- subset(merge(test.census, test.census, by="plant", sort=FALSE), 
               year.x==year.y-1)
trans

## Format column and row names
trans<-trans[,c(1:4,6)]
colnames(trans)[2:5] <- c("year", "stage", "fruits", "fate")
rownames(trans) <- 1:nrow(trans)
## Order stage and fate columns
trans$stage <- ordered(trans$stage, levels = test.stage)
trans$fate  <- ordered(trans$fate,  levels = c(test.stage,"dead"))

## Select transitions for 2001-2002 and count offspring (seedlings)

test.trans <- subset(trans, year==2001)
seedlings<-nrow(subset(test.census, year==2002 & stage=="seedling"))

## Add individual fertilities using "anonymous reproduction"  based on the 
## proportional reproductive outputs of flowering plants and the total number 
## of seedlings at the end of the projection interval

test.trans$seedling<-test.trans$fruits/sum(test.trans$fruits) * seedlings
test.trans

##  Create transition frequency table  and build T matrix

tf<-table( test.trans$fate, test.trans$stage, dnn=c("fate","stage") )
tf

T.mat<-prop.table(tf,2)[-4,]
T.mat

## Summarize stage-specific fertility rates and build F matrix

fert<-tapply(test.trans$seedling, test.trans$stage, mean)
fert

F.mat<-T.mat*0
F.mat[1,]<- fert
F.mat

## The final projection matrix is just

T.mat+F.mat

## OR use projection matrix function - 

projection.matrix(test.trans)


}
\keyword{datasets}
