\name{RDDEAnCm}
\alias{RDDEAnCm}

\title{R nCm bootstrapping of DDEA models}
\description{
R nCm bootstrapping of DDEA models.
Uses our modification of Geyer's subsampling bootstrap suggestion to increase computational efficiency.   

Comparison of R to Fortran nCm results and times
Replicates Fortran procedures in R to facilitate user understanding of Fortran DDEAnCm process
   }
\usage{
 RDDEAnCm(X,Y,orient='ddea',RTS='crs',nboot=250,bootlist=NULL,DX=NULL,DY=NULL,mlist=NULL,seedval=1001,
 replaceum='F',alpha=0.05,CILag=1,plotum='F',plottxt='')
}
\arguments{

\item{X}{An nDMU x nX matrix of Input observations }

\item{Y}{An nDMU x nY matrix of Output observations }

\item{orient}{Input efficiency "in" output efficiency "out"}

\item{RTS}{Returns to Scale: "vrs","drs","crs", and "irs"}
  
\item{nboot}{Number of bootstraps to complete for each sample size m.}
  
\item{bootlist}{list of nDMUboot DMU's to bootstrap. Set to 1:nDMU if no entry.}

\item{DX}{An nDMUboot x nX matrix of Input observations. Set internally if no entry.}

\item{DY}{An nDMUboot x nY matrix of Output observations. Set internally if no entry.}

\item{mlist}{list of sample sizes m.  If NA, an mlist will be generated internally}

\item{seedval}{A positive 32-bit integer}

\item{replaceum}{Sample with replacement}

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
# not run
#########################################################################
## contrast and time R versus Fortran nCm bootstrap results 
#########################################################################
#require(MMLPDEA)
#graphics.off()
#########################################################################
#set.seed(101)
#########################################################################
## model inputs
#nDMU=1000
#nIN=3
#nOUT=1
#delta=1
## directional efficiency score for given DMU1 
#eff0=0.50
## DDEA "efficient" input/output values for DMU1
#(xe=rep(10,nIN))
##[1] 10 10 10
#(ye=prod(xe^(1/nIN))^delta)
##[1] 10
###########################################################################
## With in-out model dy=y0 and dx=x0
## ye=y0+eff*dy with eff>0 <=ye=y0+eff*y0 <=ye=(1+eff)*y0 <=y0=ye/(1+eff) with eff>0
## xe=x0-eff*dx <=xe=x0-eff*x0 <=xe=(1-eff)*x0 <=x0=xe/(1-eff) with 0<=eff<1
###########################################################################
##Generate "inefficient" point for DMU 1
#(x0=xe/(1-eff0))
##[1] 20 20 20
#(y0=ye/(1+eff0))
##[1] 6.666667
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
# eff=rbeta(nDMU,1,5)
# summary(eff)
##     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##0.0002952 0.0554500 0.1266000 0.1654000 0.2321000 0.7771000 
#
# eff[eff>0.90]=0.9
#
# X=XE/(1-eff)
# Y=YE/(1+eff)
#############
##put DMU 0 data in matrices
# (X[1,]=x0)
##[1] 20 20 20
# (Y[1,]=y0)
##[1] 6.666667
# eff[1]=eff0
######################################################
##estimate eff scores for all DMU's
# time1=seconds()
# tmp1=RDDEAnCm(X,Y,orient='inout',RTS='CRS',nboot=0,bootlist=1:nDMU)
# time2=seconds()
# tmp2=DDEAnCm(X,Y,orient='inout',RTS='CRS',nboot=0,bootlist=1:nDMU)
# time3=seconds()
# summary(tmp1$effvals-tmp2$effvals)
##      Min.    1st Qu.     Median       Mean    3rd Qu. 
##-6.051e-13 -5.762e-14  5.153e-14  4.751e-14  1.561e-13 
##      Max. 
## 5.463e-13 
# plot(eff,tmp2$effvals)
# time2-time1
##[1] 1.7
# time3-time2
##[1] 0.13
# (time2-time1)/(time3-time2)
##[1] 13.07692
######################################################
##conduct,time, and contrast R versus Fortran nCm bootstraps 
# time4=seconds()
# tmp3=RDDEAnCm(X,Y,orient='inout',RTS='CRS',nboot=2000,bootlist=1)
# time5=seconds()
# tmp4=DDEAnCm(X,Y,orient='inout',RTS='CRS',nboot=2000,bootlist=1)
# time6=seconds()
# summary(as.vector(tmp3$boot-tmp4$boot))
##     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
##0.000e+00 0.000e+00 0.000e+00 2.252e-09 0.000e+00 6.987e-06 
# time5-time4
##[1] 43.57
# time6-time5
##[1] 0.45
# (time5-time4)/(time6-time5)
##[1] 96.82222
################################################################################
} 

