\name{LP_DF}
\alias{LP_DF}
\docType{data}
\title{
Example data to demonstrate breakdown in MMLP code.
}
\description{
Example data to demonstrate breakdown in MMLP code.
}
\usage{data("LP_DF")}
\format{
  A data frame with 6 observations on the following 8 variables.
  \describe{
    \item{\code{V1}}{a numeric vector}
    \item{\code{V2}}{a numeric vector}
    \item{\code{V3}}{a numeric vector}
    \item{\code{V4}}{a numeric vector}
    \item{\code{V5}}{a numeric vector}
    \item{\code{V6}}{a numeric vector}
    \item{\code{rest}}{a character vector}
    \item{\code{rhs}}{a numeric vector}
  }
}
\details{
Example data to demonstrate breakdown in MMLP code.
}
\examples{
#not run
#data(LP_DF)
#require(lpSolve)
#(obj2=as.vector(t(LP_DF[6,1:6])))
#(A2=as.matrix(LP_DF[1:5,1:6]))
#(rest2=as.vector(LP_DF$rest[1:5]))
#(rhs2=as.vector(LP_DF$rhs[1:5]))
#MMLP(objtype='min',obj=obj2,A=A2,rest=rest2,rhs=rhs2,MMLPV=1)$objval
#lp("min",obj2,A2,rest2,rhs2)
#MMLP(objtype='min',obj=obj2,A=A2,rest=rest2,rhs=rhs2,MMLPV=2)$objval
}
\keyword{MMLP}
