generation.time<-function(T,F)
{
   s <- length(diag(T))
  # check if matrix is singular
   N <- try(solve(diag(s) - T), silent=TRUE)
  if(class(N)=="try-error"){generation.time<-"T matrix is singular"}
   else{


   R <- F %*% N
   Ro<- max(Re(eigen(R)$values))
   A <- T+F
   lambda <- max(Re(eigen(A)$values)) 
   generation.time = log(Ro)/log(lambda)   
  }
   generation.time
}
