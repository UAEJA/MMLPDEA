\name{dea.sample_DEAboot}
\alias{dea.sample_DEAboot}

\title{ resample efficiency scores in DEAboot   }

\description{ The function \code{dea.sample_DEAboot} is borrowed and modified from the Benchmarking package
to give the same answers as those obtained from the fortran code.  We thank the authors of the Benchmarking
package for allowing us to borrow and edit their code for this demonstration.  
}


\usage{
dea.sample_DEAboot(e,h,K=NULL,seedval)

}

\arguments{

  \item{e}{original DEA eff estimates}

  \item{h}{kernal value}

  \item{K}{Number of bootstrap replicats}

  \item{seedval}{seedvalue for fortran random number generator}
}


\details{ See the Benchmarking package's documentation of the dea.sample function for a complete desription
   of the arguments returned by the dea.sample function.}

\value{ 

  \item{estar}{Resampled Efficiencies}

  \item{seedval}{incremented seed value for fortran random number generator}

}

\examples{
#not run
}


\author{ 
The good stuff: Peter Bogetoft and Lars Otto \email{larsot23@gmail.com}

The bad stuff : Joe Atwood \email{jatwood@montana.edu}

}
