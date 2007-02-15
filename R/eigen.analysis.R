eigen.analysis<-function(A, zero=TRUE)
{
 ev <- eigen(A)
 lmax <- which(Re(ev$values)==max(Re(ev$values)))
 lambda <- Re(ev$values[lmax])
 W <- ev$vectors
 w <- abs(Re(W[,lmax]))
 V <- Conj(solve(W))
 v <- abs(Re(V[lmax,]))

 s <- v%o%w
 if(zero)
   {
 s[A == 0] <- 0
   }
 e <- s*A/lambda

  dr<-sort(Re(ev$values), dec=TRUE)

   x<-dimnames(A)
 ## include row/column names from A in output
  dimnames(s)<-x 
  names(w)<-x[[1]]
  names(v)<-x[[1]]
 
eigen.analysis <- list(
                        lambda=lambda,
                       stable.stage=w/sum(w),
                       sensitivities=s,
                       elasticities=e,
                       repro.value=v/v[1],
                       damping.ratio= dr[1]/abs(dr[2])
                       )
 eigen.analysis
}
