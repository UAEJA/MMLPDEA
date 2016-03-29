\name{DEAboot}
\alias{DEAboot}

\title{Fortran bootstrapping of input/output DEA models}
\description{
Fortran bootstrapping of input/output DEA models
   }
\usage{
DEAboot(X,Y,orient='in',RTS='crs',nboot=250,bootlist=NULL,alpha=0.05,seedval=1001,MMLPV=2)
}
\arguments{
  \item{X}{An nDMU x nX matrix of Input observations }
  \item{Y}{An nDMU x nY matrix of Output observations }
  \item{orient}{Input efficiency "in" output efficiency "out"}
  \item{RTS}{Returns to Scale: "vrs","drs","crs", and "irs"}
  \item{nboot}{Number of bootstraps to complete.
  nboot=0 calculates DEA efficiency scores for each dmu but does not bootstrap the results
  }
\item{bootlist}{list of DMU's to bootstrap. Set to 1:nDMU if no entry.}
\item{alpha}{Desired confidence interval}
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
\references{ 
   Peter Bogetoft and Lars Otto; \emph{Benchmarking with
   DEA, SFA, and R}; Springer 2011.

   Cinzia Dario and L. Simar; \emph{Advanced Robust and Nonparametric
   Methods in Efficiency Analysis}.  Methodology and Applications;
   Springer 2007.

   Leopold Simar and Paul .W. Wilson (1998), \dQuote{Sensitivity
   analysis of efficiency scores: How to bootstrap in nonparametric
   frontier models}, \emph{Management Science} 44, 49--61.
}
\author{ Joe Atwood}
\examples{
#not run
############################################################
#set.seed(2015)
#graphics.off()
############################################################
#orient="out"         # 1=in, 2=out
#RTS="crs"           # 1=vrs, 2=drs, 3=crs, 4=irs 
############################################################
#nDMU=250
#nboot=250
############################################################
## Define Cobb-Douglas technology with CRS
#b1=0.5
#b2=1-b1
############################################################
## Generate "input levels" 
#x1=runif(nDMU,5,10)
#x2=runif(nDMU,5,10)
## Generate "frontier" output levels
#y1=x1^b1*x2^(1-b1)
## Generate "inverse output efficiency scores" 
#eff0=seq(0.25,1.0,length.out=nDMU)
## Contract output levels away from the efficient frontier
#y1=eff0*y1
## Put input and output quantities into matrices X and Y
#X=as.matrix(cbind(x1,x2))
#Y=as.matrix(y1)
############################################################
#
############################################################
## Call the Fortran based DEAboot function
#time1=seconds() #Note: This function is in the MMLPDEA package.
# tmp1=DEAboot(X,Y,orient=orient,RTS=RTS,nboot=nboot)
#time2=seconds()
############################################################
## Run modified Benchmarking package bootstrapping code 
#time3=seconds()
# tmp2=dea.boot_DEAboot(X,Y,NREP=nboot,RTS=RTS,ORIENTATION=orient)
#time4=seconds()
#################
## Run modified Benchmarking package bootstrapping code with
## internal status printing
#time5=seconds()
# tmp3=dea.boot_DEAboot(X,Y,NREP=nboot,RTS=RTS,ORIENTATION=orient,
# printum=TRUE,printmod=25)
##[1] "Range of dist: "
##[1] 1.000000 3.937427
##[1] "25 time = 1.63000000000011"
##[1] "50 time = 3.09999999999991"
##[1] "75 time = 4.63000000000011"
##[1] "100 time = 6.11000000000013"
##[1] "125 time = 7.59000000000015"
##[1] "150 time = 9.11000000000013"
##[1] "175 time = 10.5900000000001"
##[1] "200 time = 12.0700000000002"
##[1] "225 time = 13.5500000000002"
##[1] "250 time = 15.0500000000002"
##[1] "time = 15.1100000000001"
#time6=seconds()
############################################################
## Run Benchmarking package bootstrapping code 
#time7=seconds()
#tmp4=dea.boot(X,Y,NREP=nboot,RTS=RTS,ORIENTATION=orient)
#time8=seconds()
############################################################
## Contrast DEAboot results to Benchmarking results
############################################################
#summary(tmp1$effvals-tmp2$eff)
##      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
##-4.434e-12 -7.748e-13 -7.139e-14 -1.916e-13  5.462e-13  2.589e-12 
#summary(tmp1$effvals.bc-tmp2$eff.bc)
##      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
##-3.473e-06 -1.960e-10  1.710e-10 -1.190e-08  7.890e-10  3.384e-06 
#summary(as.vector(tmp1$boot)-as.vector(tmp2$boot))
##      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
##-3.092e-04 -7.840e-09 -7.800e-10  1.190e-08  7.020e-09  3.122e-04 
#summary(as.vector(tmp1$ci)-as.vector(tmp2$conf.int))
##      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
##-1.642e-04 -7.660e-09 -7.200e-10 -3.297e-07  4.880e-09  3.594e-08 
########################
#(h1=tmp1$h)
##[1] 0.171173
#effhat1=tmp1$effvals
#boot1=tmp1$boot
########################
#(h2=tmp2$h)
##[1] 0.171173
#effhat2=as.vector(tmp2$eff)
#boot2=tmp2$boot
###############################################################
#summary(effhat1-effhat2)
##      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
##-4.434e-12 -7.748e-13 -7.139e-14 -1.916e-13  5.462e-13  2.589e-12 
#summary(as.vector(boot1)-as.vector(boot2))
##      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
##-3.092e-04 -7.840e-09 -7.800e-10  1.190e-08  7.020e-09  3.122e-04 
#x11()
#plot(as.vector(boot1),as.vector(boot2))
#abline(0,1,col=2)
###############################################################
## DEAboot computation time
#time2-time1
##[1] 0.95
## Benchmarking computation times
#time4-time3
##[1] 15.06
#time6-time5
##[1] 15.14
#time8-time7
##[1] 15.1
###############################################################
}

