\name{newseed}
\alias{newseed}

\title{newseed}

\description{generates new 32 bit seed value from seedval

u=ugen(1,seedval)

newseed=floor(u[1]*2147483645)

if(newseed==0,newseed=1)

}

\usage{
newseed(seedval)
}

\arguments{
  \item{seedval}{seed value for fortran random number generator}
}


\value{ 
  \item{newseed}{new 32 bit seed value for fortran random number generator}
}

\examples{
newseed(1001)
}


\author{ 
Joe Atwood \email{jatwood@montana.edu}

}
