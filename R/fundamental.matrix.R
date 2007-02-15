fundamental.matrix<-function(T)
{

   s <- length(diag(T))
   # check if matrix is singular 
   N <- try(solve(diag(s) - T), silent=TRUE)
  if(class(N)=="try-error"){fundamental.matrix<-"T matrix is singular"}
   else{

   
   var<- (2*diag(diag(N)) - diag(s)) %*% N - N * N
     dimnames(var)<-dimnames(T)
   total <- margin.table(N,2)
   vareta <- margin.table(2*(N %*% N) - N, 2) - total * total

   fundamental.matrix <- list(
     N = N,
     var = var,
     cv = var^.5/N,
     meaneta = total,
     vareta = vareta
                )
   }
   fundamental.matrix
}
