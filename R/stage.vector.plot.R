stage.vector.plot<-function(stage.vectors, proportions=TRUE, legend.coords="topright", ylim=NULL, xlab="Years", ylab=NULL,  ... )
{
    p<-stage.vectors
    x<-colnames(p)                    # x-axis
    if(is.null(x)){x<-1:dim(p)[2]}
    n<-dim(p)[1]                      #number of stage vectors
    if(proportions)                       
    {
       if(is.null(ylab)){ylab<- "Proportion in stage class" }
       p<-prop.table(p, 2)         ## Change counts to proportions using prop.table
   
       if(is.null(ylim)){ylim=c(min(p), max(p))}
       plot(x, p[1,], type='n', ylim=ylim, xlab=xlab, ylab=ylab,...)                
    }
    else                            ## OR plot total number 
    {
       if(is.null(ylab)){ylab <- "Number in stage class" }
       if(is.null(ylim)){ylim=c(floor(min(p)), ceiling(max(p))) }
       plot(x, p[1,], type='n', ylim=ylim, xlab=xlab, ylab=ylab, ... )      
    }
    ## order legend by descreasing order of mean number in stage vector
    y<-sort(apply(p,1,mean), index.return=TRUE, decreasing=TRUE)

    for (i in y$ix )                ## Loop through stage classes
    {                 
       lines(x, p[i,],lty=i, col=rainbow(n)[i], lwd=2)
    }
    legend(legend.coords[1],legend.coords[2], paste(names(y$x), ""), lty=y$ix, col=rainbow(n)[y$ix], lwd=2)
}

