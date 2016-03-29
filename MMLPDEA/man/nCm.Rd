\name{nCm}
\alias{nCm}

\title{ pulls nCm samples }
\description{
   pulls nCm samples giving same results as in the Fortran DDEAnCm bootstrap code 
}
\usage{
nCm(nvals=1:10,m=5,replaceum='F',seedval=1001)
}
\arguments{
  \item{nvals}{ values to sample from }
  \item{m}{ number of values to sample  }
  \item{replaceum}{ sample with 'T' or without 'F' replacement }
  \item{seedval}{ seed value }
  
}
\value{
 vector of m sampled values
 
}
\author{ Joe Atwood }
\examples{
#not run
####################################
# nCm(1:100,10)
####################################
}
