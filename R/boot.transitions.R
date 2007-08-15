
boot.transitions <- function(transitions, iterations, by.stage.counts=FALSE, ...)
{
    ## check orderd fate, stage,and one or more fertility columns?

  t <-iterations
   mat <- vector("list", t)                       ## initialize a list to store matrices
   vec <- vector("list", t)                       ## and stage vectors 
   lam <-numeric(t)                               ## and a vector for lambdas

  for (i in 1:t)
   { 
      if(by.stage.counts)
      {
         boot<-do.call(rbind,                           ## create new data frame with resampled transitions
         lapply(split(transitions, transitions$stage, drop=TRUE),  ## by counts in original class vector
         function(x) x[sample(nrow(x), replace=TRUE),]))    
      }                                           
      else                                              ## or default
      { 
         boot<-transitions[sample(nrow(transitions), replace=TRUE), ]
      }

      A<-projection.matrix(boot, ...) 

      vec[[i]] <- summary(boot$stage)
      mat[[i]] <- as.vector(A)
      lam[i]  <- max(Re(eigen(A)$values))         ## Calculate dominant eigenvalue
   }

   n<-dim(A)[1]

    
   boot.stage <- list(
    lambda= lam, 
    matrix= matrix(unlist(mat), byrow=TRUE, nrow=t,
          dimnames=list(1:t, paste("a", 1:n, rep(1:n,each=n), sep=""))), ## Store one matrix  
    vector= matrix(unlist(vec), byrow=TRUE, nrow=t, dimnames=list(1:t, rownames(A)))  ## and vector per row
   )
   boot.stage
}
