eigen.analysis<-function(A, zero=TRUE)
{
    ev <- eigen(A)
    # R sorts eigenvalues in decreasing order, according to Mod(values)
    #  ususally dominant eigenvalue is first, but not always (example?)
    lmax <- which(Re(ev$values) == max(Re(ev$values)))
    lmax <- lmax[1]  ## lmax may be a vector if two identical dominant eigenvalues
    lambda <- Re(ev$values[lmax])
    dr <- sort(Re(ev$values), dec = TRUE)  ## sort for damping ratio
    
    W <- ev$vectors
    w <- abs(Re(W[, lmax]))
    ## check if matrix is singular-and output NAs rather than stop (better for loops and  bootstrap)
    V <- try(Conj(solve(W)), silent=TRUE)
    if (class(V) == "try-error") {
      eigen.analysis <- list(lambda1 = lambda, stable.stage = w/sum(w), 
        sensitivities = A*NA, elasticities = A*NA, repro.value = w*NA, 
        damping.ratio = dr[1]/abs(dr[2]))
                           }
    else{ 
    v <- abs(Re(V[lmax, ]))
    s <- v %o% w
    if (zero) {
        s[A == 0] <- 0
    }
    e <- s * A/lambda
    
    x <- dimnames(A)
    dimnames(s) <- x
    names(w) <- x[[1]]
    names(v) <- x[[1]]
    eigen.analysis <- list(lambda1 = lambda, stable.stage = w/sum(w), 
        sensitivities = s, elasticities = e, repro.value = v/v[1], 
        damping.ratio = dr[1]/abs(dr[2]))
  }
    eigen.analysis
}
