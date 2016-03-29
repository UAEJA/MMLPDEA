\name{dea.boot_DEAboot}
\alias{dea.boot_DEAboot}

\title{ DEAboot using R  }

\description{ The function \code{dea.boot_DEAboot} is borrowed and modified from the Benchmarking package
to give the same answers as those obtained from the fortran code.  We thank the authors of the Benchmarking
package for allowing us to borrow and edit their code. 

This function in included in the DEAboot package primarily to demonstrate that the Fortran code based call DEAboot generates results equivilent
to those obtained from the Benchmarking package's function dea.boot if dea.boot is modified to use the same random numbers.  

The function DEAboot can be run independently from the function dea.boot_DEAboot.
}


\usage{
dea.boot_DEAboot(X, Y, NREP = 200, EFF = NULL, RTS = "vrs", ORIENTATION="in", 
         alpha = 0.05, XREF = NULL, YREF = NULL, EREF = NULL, 
         DIRECT = NULL, TRANSPOSE = FALSE, LP,printum=FALSE,printmod=5,saveum=FALSE,seedval=1001)

}

\arguments{

  \item{X}{Inputs of firms to be evaluated, a K x m matrix of
      observations of K firms with m inputs (firm x input) }

  \item{Y}{Outputs of firms to be evaluated, a K x n matrix of
      observations of K firms with n outputs (firm x input). }

  \item{NREP}{Number of bootstrap replicats}

  \item{EFF}{Efficiencies for (X,Y) relative to the technology
      generated from (XREF,YREF).}

  \item{RTS}{The returns to scale assumptions as in \code{\link{dea}},
      only works for "vrs", "drs", and "crs"; more to come.}

  \item{ORIENTATION}{Input efficiency "in" (1), output
      efficiency "out" (2), and graph efficiency "graph" (3). }

  \item{alpha}{One minus the size of the confidence interval for the
      bias corrected efficiencies}

  \item{XREF}{Inputs of the firms determining the technology, defaults
      to \code{X}. }

  \item{YREF}{Outputs of the firms determining the technology,
      defaults to \code{Y}. }

  \item{EREF}{Efficiencies for the firms in XREF, YREF.}

  \item{DIRECT}{Does not yet work and is therefore not used.}

  \item{TRANSPOSE}{Input and output matrices are K x m and K x n for
      the default value \code{TRANSPOSE=FALSE}; this is standard in R
      for statistical models.  When \code{TRANSPOSE=TRUE} data
      matrices are m x K and n x K.}

  \item{LP}{Only for debugging purposes.}

  \item{printum}{printum==TRUE prints runtime progress reports}
  
  \item{printmod}{if printum==TRUE, progress reports are printed every printmod'th iteration}

  \item{saveum}{if saveum!=FALSE, returns extra data at end of function call }

  \item{seedval}{A positive 32-bit seed value for the fortran random number generator.
  
  This value must equal the value sent to DEAboot to obtain identical answers from this function and the Fortran DEAboot call. 
  
  }
  

}


\details{ See the Benchmarking package's documentation of the dea.boot function for a complete desription
   of the arguments returned by the dea.boot function.}



\value{ The returned values from both functions are as follows:

  \item{eff}{Efficiencies}

  \item{eff.bc}{Bias-corrected efficiencies}

  \item{bias}{An array of bootstrap bias estimates for the firms in X,Y}

  \item{conf.int}{\code{K x 2} matrix with confidence interval for the
      estimated efficiencies}

  \item{var}{An array of bootstrap variance estimates for the firms in
      X,Y}

  \item{boot}{ The replica bootstrap estimates of the Farrell
      efficiencies, a K times NREP matrix. Note the bootstrap
      estimates are sorted for each firm.}.

}

\examples{
#not run
}


\references{ Peter Bogetoft and Lars Otto; \emph{Benchmarking with
   DEA, SFA, and R}; Springer 2011.

   Cinzia Dario and L. Simar; \emph{Advanced Robust and Nonparametric
   Methods in Efficiency Analysis}.  Methodology and Applications;
   Springer 2007.

   Leopold Simar and Paul .W. Wilson (1998), \dQuote{Sensitivity
   analysis of efficiency scores: How to bootstrap in nonparametric
   frontier models}, \emph{Management Science} 44, 49--61.

}


\author{ 
The good stuff: Peter Bogetoft and Lars Otto \email{larsot23@gmail.com}

The bad stuff : Joe Atwood \email{jatwood@montana.edu}

}
