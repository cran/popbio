\name{02.Caswell}
\alias{02.Caswell}
\alias{Caswell}
\title{Converted Matlab functions from Caswell (2001) } 
\description{ 

\bold{Chapter 2}.  Age-classified matrix models
\describe{
\item{ \code{\link{pop.projection}} }{ section 2.2.   Projection of
    population growth rates.  See  \code{\link{stage.vector.plot}} to
    plot stage (or age) vectors}
}

\bold{Chapter 4}.  Stage-classified matrix models
\describe{
\item{ \code{\link{eigen.analysis}} }{ section 4.8.   Computation of
    eigenvalues and eigenvectors.  Returns the dominant eigenvalue
    (section.4.4), stable stage distribution (section 4.5),
    reproductive value (section 4.6), and damping ratio (section 4.7)}
}

\bold{Chapter 5}.  Events in the Life Cycle
\describe{
\item{ \code{\link{fundamental.matrix}} }{   section 5.3.1.
    Calculate age-specific survival from a stage classified matrix
    using the fundamental matrix N }
\item{ \code{\link{net.reproductive.rate}} }{   section 5.3.4.
    Calculate the net reproductive rate of a stage classified matrix
    using the dominant eigenvalue of the matrix R.  }
\item{ \code{\link{generation.time}} }{ section 5.3.5.   Calculate the
    generation time of a stage-classified matrix  }

}
The \link{whale} dataset includes code to calculate age-specific survival and fertility curves (figure 5.1 and 5.2).

\bold{Chapter 6}. Parameter estimation
\describe{
\item{ \code{\link{projection.matrix}} }{ section 6.1.1.  Estimate
    vital rates and construct a projection matrix using transtion
    frequency tables}
\item{ \code{\link{QPmat}} }{ section 6.2.2.  Construct a projection
    matrix from a time series of  individuals per stage using Wood's
    quadratic programming method.  Requires \code{quadprog} library.}
}
See the \code{Rcapture} package for capture-recapture methods
described in section 6.1.2. 

\bold{Chapter 9}.  Sensitivity analysis
\describe{
\item{ \code{\link{eigen.analysis}} }{ section  9.1, and 9.2.
    Calculate sensitivities and elasticities.  The examples in the
    \link{teasel} and \link{tortoise} datasets also include code to
    sum elasticities and create figures like 9.3, 9.4, and 9.11. }
}
See the \code{secder} function in the \code{demogR} package for second derivatives of eigenvalues
described in section 9.7 

\bold{Chapter 10}.  Life Table Response Experiments
\describe{
\item{ \code{\link{LTRE}} }{ section 10.1 and 10.2.  Fixed designs in
    LTREs.  The example includes code for variance
    decomposition in random design. }

}

\bold{Chapter 14}.  Environmental stochasticity
\describe{
\item{ \code{\link{stoch.growth.rate}} }{ section 14.3.  Calculate the
    log stochastic growth rate by simulation and Tuljapukar's approximation  }
\item{ \code{\link{stoch.projection}} }{  section 14.5.3.  Project
    stochastic growth from a sequence of matrices in a uniform and
    nonuniform environment }
}
See the \code{stoch.sens} function in the \code{demogR} package for senstivity and elasticity of log
stochastic growth rate described in section 14.4.   

\bold{Chapter 15}.  Demographic stochasticity
\describe{
\item{ \code{\link{multiresultm}} }{ section 15.1.3.  Incorporate
    demographic stochasticity into population projections.  The
    example uses the \link{whale} dataset to create a plot like figure
    15.3. }

}





}
\author{Chris Stubben}

\keyword{documentation}
