\name{sampleum}
\alias{sampleum}

\title{Samples with replacement using Fortran generated random numbers.}

\description{ Samples with replacement using Fortran generated random numbers.}

\usage{
sampleum(nobs,x,seedval)
}

\arguments{

  \item{nobs}{sample size}

  \item{x}{values to sample from}

  \item{seedval}{seed value for fortran random number generator}
}


\value{ 
  \item{xsample}{nobs values resampled from x}
}

\examples{
sampleum(10,1:5)
}


\author{ 
Joe Atwood \email{jatwood@montana.edu}

}
