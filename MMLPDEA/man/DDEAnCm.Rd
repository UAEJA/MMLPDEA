\name{DDEAnCm}
\alias{DDEAnCm}

\title{Fortran nCm bootstrapping of DDEA models}
\description{
Fortran nCm bootstrapping of DDEA models.
Uses our modification of Geyer's subsampling bootstrap suggestion to increase computational efficiency.

Note: Although this function allows the user to complete a nCm subsampling process for
multiple DMU's at the same time, it is recommended that the user complete this process 
on one DMU at a time and that the user carefully examine the boxplots of the bootstrapped
values .  Our endogenous process for determining the subsample sizes in mlist may need to 
be overridden by the user's exogenously generated mlist when a given DMU's efficiency scores 
are close to a boundary such as one for the input model or zero for the DDEA model. 
}
\usage{
 DDEAnCm(X,Y,orient='ddea',RTS='crs',nboot=250,bootlist=NULL,DX=NULL,DY=NULL,mlist=NULL,seedval=1001,
 replaceum='F',MMLPV=2,alpha=0.05,CILag=1,plotum='F',plottxt='')
}
\arguments{

\item{X}{An nDMU x nX matrix of Input observations }

\item{Y}{An nDMU x nY matrix of Output observations }

\item{orient}{Input efficiency "in" output efficiency "out"}

\item{RTS}{Returns to Scale: "vrs","drs","crs", and "irs"}
  
\item{nboot}{Number of bootstraps to complete for each sample size m.}
  
\item{bootlist}{list of nDMUboot DMU's to bootstrap. Set to 1:nDMU if no entry.}

\item{DX}{An nDMUboot x nX matrix of Input directions. Set internally if no entry.}

\item{DY}{An nDMUboot x nY matrix of Output directions. Set internally if no entry.}

\item{mlist}{list of sample sizes m.  If NA, an mlist will be generated internally}

\item{seedval}{A positive 32-bit integer}

\item{replaceum}{Sample with replacement}

\item{MMLPV}{1=Miller's original code, 2=Miller's corrected code (Atwood-2015)}

\item{alpha}{ Confidence Interval prob }

\item{CILag}{Lag for m interval selection process }

\item{plotum}{plot CI diagnostics use plotum='T' to plot }

\item{plottxt}{text to be included in plot}

}
\value{
   \item{effvals}{Vector of Efficiency Scores}
   \item{effvals.bc}{Vector of Bias-Corrected Efficiency Scores}
   \item{bias}{Vector of estimated bias levels}
   \item{mlist}{list of sample sizes m}   
   \item{boot}{nDMUboot by length(mlist) by nboot array of bootstrapped efficiency scores}
   \item{mchosen}{chosen m interval}
   \item{alpha}{alpha level computed}
   \item{CI}{Confidence Intervals }
   \item{effstatus}{Status of Efficiency Scores
       indstat = 0 the problem was solved;
       indstat = 1 the problem has no solution;
       indstat = 2 itermax iterations were performed-more needed;
       indstat = 3 sufficient accuracy could not be maintained to solve the problem;
       indstat = 4 the problem has an unbounded solution;
       indstat = 5 input error detected;
       indstat = 6 the solution may have been obtained;  
    }
   \item{bootstatus}{status array of bootstrapped efficiency scores (equal in dimension to boot array)}
   \item{seedval}{seedval used}
   }
\author{ Joe Atwood}
\references{

Geyer. C. J. "The Subsampling Bootstrap." http://www.stat.umn.edu/geyer/5601/notes/sub.pdf

Politis, D.N., Romano, J.P., Wolf, M., 1999. "Subsampling". Springer. New York.

Politis, D.N., Romano, J.P., Wolf, M., 2001. "On the asymptotic theory of subsampling."
Statistica Sinica 11, 1105-1124.

Simar, L., Wilson, P.W., 2011. "Inference by the m Out of n Bbootstrap in
Nonparametric Frontier Models." Journal of Productivity Analysis 36,33-53.

Simar, L. A. Vanhems, P.W. Wilson. 2012 "Statistical Inference for DEA Estimators of Directional Distances."
European J. of Operational Research. 220:853-864.

}
\examples{
## not run
##########################################################################
### simulate coverage level for DDEAnCm confidence intervals 
##########################################################################
#require(MMLPDEA)
#########################################################################
#set.seed(101)
#########################################################################
##model inputs
#########################################################################
#nDMU=1000
#nIN=3
#nOUT=1
#delta=1
#nsims=100
#nboot=2000
#alpha=0.05
#replaceum='F'
#CI=matrix(0,nsims,2)
##directional efficiency score for given DMU1 
#eff0=0.50
##DDEA "efficient" input/output values for DMU1
#(xe=rep(10,nIN))
##[1] 10 10 10
#(ye=prod(xe^(1/nIN))^delta)
##[1] 10
##########################################################################
## With in-out model dy=y0 and dx=x0
## ye=y0+eff*dy with eff0 <=ye=y0+eff*y0 <=ye=(1+eff)*y0 <=y0=ye/(1+eff) with eff0
## xe=x0-eff*dx <=xe=x0-eff*x0 <=xe=(1-eff)*x0 <=x0=xe/(1-eff) with 0<=eff<1
###########################################################################
## Generate "inefficient point for DMU 1
#(x0=xe/(1-eff0))
##[1] 20 20 20
#(y0=ye/(1+eff0))
##[1] 6.666667
#####################################################
#ns=1
#for(ns in 1:nsims){
#####################################################
##Generate "efficient points" for population of DMUs
#####################################################
##Generate nDMU points on efficient frontier
# XE=matrix(runif(nDMU*nIN,5,15),nDMU,nIN)
# YE=matrix((apply(XE^(1/nIN),1,prod))^delta,nDMU,1)
#####################################################
##Generate "inefficient points" for population of DMUs
#####################################################
##Generate DEA efficiency scores for population
#####################################################
# eff=rbeta(nDMU,1,5)
# summary(eff)
# eff[eff>0.90]=0.9
# X=XE/(1-eff)
# Y=YE/(1+eff)
##############
##put DMU 0 data in matrices
# (X[1,]=x0)
# (Y[1,]=y0)
# eff[1]=eff0
#############################################################################
# tmp4=DDEAnCm(X,Y,orient='inout',RTS='CRS',nboot=nboot,bootlist=1,
# replaceum=replaceum,alpha=alpha,CILag=2,plotum='T',plottxt=paste('rep',ns))
# CI[ns,]=tmp4$CI
#############################################################################
#}# end loop for(ns in 1:nsims)
#############################################################################
#INCI=ifelse(eff0>=CI[,1]&eff0<=CI[,2],1,0)
#(cover=mean(INCI))
##[1] 0.94
#1-alpha
##[1] 0.95
############################################################################
} 

