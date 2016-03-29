\name{lagMat}
\alias{lagMat}

\title{ Generate a non-time series lagged matrix  }
\description{
   lagMat generates a lagged matrix 
}
\usage{
lagMat(x,lags=2,Lzero='F')
}
\arguments{
  \item{x}{ vector of data to be lagged }
  \item{lags}{ specifies the lag length(s) }
  \item{Lzero}{ include the zero lag vector in matrix}
}
\value{
 lagMat returns a lagged matrix
 -- See examples below
}
\author{ Joe Atwood }
\examples{
#not run
###########################################
# (x=1:10)
# [1] 1  2  3  4  5  6  7  8  9  10
#lagum(x,1)
# [1] NA  1  2  3  4  5  6  7  8  9
#lagum(x,3)
# [1] NA NA NA  1  2  3  4  5  6  7
#uplag(x)
# [1]  2  3  4  5  6  7  8  9 10 NA
#lagum(x,-1)
# [1]  2  3  4  5  6  7  8  9 10 NA
###########################################
#lagMat(x,2)
#      [,1] [,2]
# [1,]   NA   NA
# [2,]    1   NA
# [3,]    2    1
# [4,]    3    2
# [5,]    4    3
# [6,]    5    4
# [7,]    6    5
# [8,]    7    6
# [9,]    8    7
#[10,]    9    8
#lagMat(x,Lzero='T')
#      [,1] [,2] [,3]
# [1,]    1   NA   NA
# [2,]    2    1   NA
# [3,]    3    2    1
# [4,]    4    3    2
# [5,]    5    4    3
# [6,]    6    5    4
# [7,]    7    6    5
# [8,]    8    7    6
# [9,]    9    8    7
#[10,]   10    9    8
#lagMat(x,-1:2)
#      [,1] [,2] [,3] [,4]
# [1,]    2    1   NA   NA
# [2,]    3    2    1   NA
# [3,]    4    3    2    1
# [4,]    5    4    3    2
# [5,]    6    5    4    3
# [6,]    7    6    5    4
# [7,]    8    7    6    5
# [8,]    9    8    7    6
# [9,]   10    9    8    7
#[10,]   NA   10    9    8
#lagMat(x,2:-1)
#      [,1] [,2] [,3] [,4]
# [1,]   NA   NA    1    2
# [2,]   NA    1    2    3
# [3,]    1    2    3    4
# [4,]    2    3    4    5
# [5,]    3    4    5    6
# [6,]    4    5    6    7
# [7,]    5    6    7    8
# [8,]    6    7    8    9
# [9,]    7    8    9   10
#[10,]    8    9   10   NA
####################################
}
