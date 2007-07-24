lnorms <- function(means,vars,rawelems)
{
    nmeans  <- log(means) - 0.5 * log(vars/means^2 + 1)
    nvars   <- log(vars/means^2 + 1)
    normals <- rawelems * sqrt(nvars) + nmeans
    lns     <- exp(normals)
    lns
    
} 

