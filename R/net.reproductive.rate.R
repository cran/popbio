net.reproductive.rate<-function(T,F)
{
   s <- length(diag(T))
   N <- try(solve(diag(s) - T), silent=TRUE)
   if(class(N)=="try-error"){r<-"T matrix is singular"}
   else{

     R <- F %*% N
     r<-max(Re(eigen(R)$values))
   }
   r
}
