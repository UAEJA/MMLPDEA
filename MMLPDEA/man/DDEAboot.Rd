\name{DDEAboot}
\alias{DDEAboot}

\title{Fortran bootstrapping of DDEA models}
\description{
Fortran bootstrapping of DDEA models.
}
\usage{
 DDEAboot(X,Y,orient='ddea',RTS='crs',nboot=250,bootlist=NULL,dx=NULL,dy=NULL,
 DX=NULL,DY=NULL,alpha=0.05,seedval=1001,MMLPV=2)
}
\arguments{

\item{X}{An nDMU x nX matrix of Input observations }

\item{Y}{An nDMU x nY matrix of Output observations }

\item{orient}{Input efficiency "in", output efficiency "out", "inout", or "ddea". Used to create directions if dx,dy,DX,and DY are missing}

\item{RTS}{Returns to Scale: "vrs","drs","crs", and "irs"}
  
\item{nboot}{Number of bootstraps to complete}
  
\item{bootlist}{list of nDMUboot DMU's to bootstrap. This is set to 1:nDMU if no entry.}

\item{dx}{An nDMUboot x nX matrix of Input directions. Set internally if no entry.}

\item{dy}{An nDMUboot x nY matrix of Output directions. Set internally if no entry.}

\item{DX}{An nDMU x nX matrix of Input directions. Set internally using orient if no entry.}

\item{DY}{An nDMU x nY matrix of Output directions. Set internally using orient if no entry.}

\item{alpha}{ Confidence Interval prob }

\item{seedval}{A positive 32-bit integer}

\item{MMLPV}{1=Miller's original code, 2=Miller's corrected code (Atwood-2015)}
}
\value{
   \item{h}{bootstrapping h value}
   \item{effvals}{Vector of Efficiency Scores}
   \item{effvals.bc}{Bias Corrected Vector of Efficiency Scores}
   \item{bias}{estimated bias}
   \item{var}{paramater variances - see Benchmarking package}
   \item{boot}{nDMUboot by nboot matrix of bootstrapped efficiency scores}
   \item{alpha}{alpha level computed}   
   \item{CI}{confidence intervals}
   \item{effstatus}{Status of Efficiency Scores
       indstat = 0 the problem was solved;
       indstat = 1 the problem has no solution;
       indstat = 2 itermax iterations were performed-more needed;
       indstat = 3 sufficient accuracy could not be maintained to solve the problem;
       indstat = 4 the problem has an unbounded solution;
       indstat = 5 input error detected;
       indstat = 6 the solution may have been obtained;  
    }
   \item{bootstatus}{status matrix of bootstrapped efficiency scores}
   \item{seedval}{seedvalue used in Fortran bootstrapping}
}
\author{ Joe Atwood}
\references{ Peter Bogetoft and Lars Otto; \emph{Benchmarking with
   DEA, SFA, and R}; Springer 2011.

   Cinzia Dario and L. Simar; \emph{Advanced Robust and Nonparametric
   Methods in Efficiency Analysis}.  Methodology and Applications;
   Springer 2007.

   Leopold Simar and Paul .W. Wilson (1998), \dQuote{Sensitivity
   analysis of efficiency scores: How to bootstrap in nonparametric
   frontier models}, \emph{Management Science} 44, 49--61.
}\examples{
# not run
#########################################################################
## simulate coverage level for DDEAboot confidence intervals 
#########################################################################
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
#time1=seconds()
#ns=1
#for(ns in 1:nsims){
#plot(ns,nsims,main=paste('rep',ns,'of',nsims))
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
# DX=X
# DY=Y
############################################################################# 
#tmp4=DDEAboot(X,Y,orient='ddea',RTS='CRS',DX=DX,DY=DY,nboot=nboot,bootlist=1,alpha=alpha)
# CI[ns,]=tmp4$CI
#############################################################################
#}# end loop for(ns in 1:nsims)
#time2=seconds()
#############################################################################
#INCI=ifelse(eff0>=CI[,1]&eff0<=CI[,2],1,0)
#(cover=mean(INCI))
##[1] 0.92
#1-alpha
##[1] 0.95
############################################################################
#time2-time1
##[1] 47.91
############################################################################
} 

