\name{lagum}
\alias{lagum}
\alias{uplag}

\title{ Generate a non-time series lagged vector }
\description{
   lagum generates a lagged vector
}
\usage{
lagum(x, nlag = 1)
}
\arguments{
  \item{x}{ vector of data to be lagged }
  \item{nlag}{ specifies the lag length - can be negative for 'uplag' }
}
\value{
 lagum returns a vector
 -- See examples below
}
\author{ Joe Atwood }
\examples{
#not run
###########################################
#x=1:10
#lagum(x,1)
# [1] NA  1  2  3  4  5  6  7  8  9
#lagum(x,3)
# [1] NA NA NA  1  2  3  4  5  6  7
#uplag(x)
# [1]  2  3  4  5  6  7  8  9 10 NA
###########################################
}
