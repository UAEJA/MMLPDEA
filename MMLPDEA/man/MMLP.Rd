\name{MMLP}
\alias{MMLP}

\title{Morris-Miller Fortran LP code interface}
\description{
Morris-Miller Fortran LP code interface
   }
\usage{
MMLP(objtype='max',obj,A,rest,rhs,itermax=1000,nsims=1,MMLPV=2)
}
\arguments{

  \item{objtype}{character string 'max' or 'min'}

  \item{obj}{vector of objective coefficients}

  \item{A}{matrix of constraint coefficients}
  
  \item{rest}{character vector of constraint signs '<=','>=', or'='}
  
  \item{rhs}{vector of RHS values}
  
  \item{itermax}{maximal number of LP iterations}

  \item{nsims}{number of repititions before returning results}
  
  \item{MMLPV}{1=Miller's original code, 2=Miller's corrected code (Atwood-2015)}
  
}
\value{
 
   \item{objval}{objective value}
   \item{xvals}{If indstat = 0 or 6, xvals returns the solution, the slack, and the surplus variable levels}
   \item{indstat}{
       indstat = 0 the problem was solved;
       indstat = 1 the problem has no solution;
       indstat = 2 itermax iterations were performed-more needed;
       indstat = 3 sufficient accuracy could not be maintained to solve the problem;
       indstat = 4 the problem has an unbounded solution;
       indstat = 5 input error detected;
       indstat = 6 the solution may have been obtained;  
   }
 
}
\author{ 

  R and Fortran interface, duals calculations: Joe Atwood

  MMLP code:
  
     WRITTEN BY ALFRED H. MORRIS JR.
     
     NAVAL SURFACE WEAPONS CENTER
     
     DAHLGREN, VIRGINIA
     
------------------------

     INITIAL VERSION  DEC  1977
     
     LAST UPDATE      OCT  1990
     
-------------------------

     Converted using F90 intrinsics by
     
        Alan Miller
        
        CSIRO Mathematical & Information Sciences
        
        CLAYTON, VICTORIA, AUSTRALIA 3169
        
     Latest revision - 5 February 1997
     
     obtained from: http://jblevins.org/mirror/amiller/


Site Statement: "This is an archived copy of the Fortran source code repository of Alan Miller previously located at http://users.bigpond.net.au/amiller/. 
It is hosted by Jason Blevins with permission. The site has been slightly reformatted, but the source code and descriptions below have not been modified.
All code written by Alan Miller is released into the public domain."     

Fortran linear programming code listing:

"smplx.f90 Linear programming using the simplex algorithm. This is a translation of the Fortran 66 program from the NSWC (Naval Surface Warfare Center) 
library written by Alfred Morris. There is also a simple test program t_smplx.f90. Needs the module constant.f90 which defines the precision and returns certain machine constants."


NOTE: Atwood modified Miller's code in August 2015 to correct solution error.   See example below:

}
\examples{
# not run
##################################################################################
#Example 1
##################################################################################
#Demonstrate potential error in original MM LP code
##################################################################################
#data(LP_DF)
#require(lpSolve)
#(obj2=as.vector(t(LP_DF[6,1:6])))
#(A2=as.matrix(LP_DF[1:5,1:6]))
#(rest2=as.vector(LP_DF$rest[1:5]))
#(rhs2=as.vector(LP_DF$rhs[1:5]))
#MMLP(objtype='min',obj=obj2,A=A2,rest=rest2,rhs=rhs2,MMLPV=1)$objval
#lp("min",obj2,A2,rest2,rhs2)
#MMLP(objtype='min',obj=obj2,A=A2,rest=rest2,rhs=rhs2,MMLPV=2)$objval
##################################################################################
#
#
# ###############################################################################
# Example 2
# ###############################################################################
# # Determine computation times required to solve the Wyndor example 100,000
# # times using loops with lpSolveAPI and MMLP and constrasting the 
# # results to solving the Wyndor problem 100,000 times within the MMLP fortran code  
# ###############################################################################
# rm(list=ls())
# ###############################################################################
# require(MMLPDEA)
# require(lpSolve)
# require(lpSolveAPI)
# ###############################################################################
# nsims=100000
# ###############################################################################
# #construct problem
# nr=3
# nc=2
# objtype='max'
# objmax=1
# obj=c(3,5)
# A=matrix(c(
#   c(1,0),
#   c(0,2),
#   c(3,2)
#  ),3,2,byrow=T)
# b=c(4,12,18)
# rest=c('<=','<=','<=')
# ############################################################################
# 
# 
# ###############################################################################
# #set up lpSolveAPI object
# LP_API=make.lp(nrow=nr,ncol=nc)
# 
# lp.control(LP_API,sense=objtype)
# 
# set.objfn(LP_API,obj)
# for(i in 1:nr){
#  set.row(LP_API,i,A[i,])
# }
# set.constr.type(LP_API,rest)
# set.rhs(LP_API,b)
# #################################################################
# 
# ################################################################
# #solve with lpSolve
# tmp=lp(objtype,obj,A,rest,b,compute.sens=TRUE)
# tmp$objval;tmp$solution;tmp$duals[1:nr]
# #[1] 36
# #[1] 2 6
# #[1] 0.0 1.5 1.0
# ################################################################
# #solve with lpSolveAPI
# (status=solve(LP_API))
# #[1] 0
# get.objective(LP_API);get.variables(LP_API);get.dual.solution(LP_API)[2:(nr+1)]
# #[1] 36
# #[1] 2 6
# #[1] 0.0 1.5 1.0
# ################################################################
# #solve with MMLP
# tmp2=MMLP(objtype=objtype,obj=obj,A=A,rest=rest,rhs=b)
# tmp2$objval;tmp2$xvals[1:nc];tmp2$duals
# #[1] 36
# #[1] 2 6
# #[1] 0.0 1.5 1.0
# ################################################################
# 
# 
# #################################################################
# # time to obtain nsims solutions
# #################################################################
# time0=seconds()
# ############################
# # lpSolveAPI with loops
# obj1=0
# for(j1 in 1:nsims){
#   set.objfn(LP_API,obj)
#   for(i in 1:nr){
#    set.row(LP_API,i,A[i,])
#   }
#   set.constr.type(LP_API,rest)
#   set.rhs(LP_API,b)
#   (status=solve(LP_API))
#   obj1[j1]=get.objective(LP_API)
# }# end loop
# ############################
# time1=seconds()
# ############################
# #remove lpSolveAPI object
# delete.lp(LP_API)
# ############################
# time2=seconds()
# ############################
# # MMLP with loops
# obj2=0
# for(j2 in 1:nsims){
#  tmp=MMLP(objtype=objtype,obj=obj,A=A,rest=rest,rhs=b)
#  obj2[j2]=tmp$objval
# }
# ############################
# time3=seconds()
# ############################
# # MMLP with internal loops
# tmp=MMLP(objtype=objtype,obj=obj,A=A,rest=rest,rhs=b,nsims=nsims)
# ############################
# time4=seconds()
# ########################################################
# 
# ########################################################
# time1-time0 # lpSolveAPI loop time
# #[1] 17.89
# time3-time2 # MMLP loop time
# #[1] 14.36
# time4-time3 # MMLP internal loop time
# #[1] 0.09
# ########################################################
# 

# ########################################################
#
#
#
#
# ###########################################################
# # Example 3: A more realistic example
# ###########################################################
# # A DEA example that computes the efficiency score for each 
# # of 10000 DMUs. The example uses traditional looping 
# # with both lpSolve and MMLP and constrasts the "R" loop 
# # times to the results of using fortran loops within DEAboot
# # (without running the bootstraps i.e. by setting nloop = 0) 
# # to obtain the efficiency score estimates for each DMU. 
# ###########################################################
# require(MMLPDEA)
# require(lpSolveAPI)
# ###########################################################
# set.seed(2015)
# ###########################################################
# nDMU=10000
# ###########################################################
# # Define Cobb-Douglas technology with CRS
# b1=0.5
# b2=1-b1
# ###########################################################
# # Generate "input levels" 
# x1=runif(nDMU,5,10)
# x2=runif(nDMU,5,10)
# # Generate "frontier" output levels
# y1=x1^b1*x2^(1-b1)
# # Generate "inverse output efficiency scores" 
# inv_eff0=seq(0.25,1.0,length.out=nDMU)
# # Contract output levels away from the efficient frontier
# y1=inv_eff0*y1
# eff0=1/inv_eff0
# # Put input and output quantities into matrices X and Y
# X=as.matrix(cbind(x1,x2))
# Y=as.matrix(y1)
# ###########################################################
# # set up output orientation model for DMU 1 and VRS 
# objtype='max'
# obj=c(rep(0,nDMU),1)
# 
# A=matrix(0,4,nDMU+1)
# A[1,1:nDMU]=t(Y); A[1,(nDMU+1)]=-Y[1,1] 
# A[2:3,1:nDMU]=t(X)
# A[4,1:nDMU]=1
# 
# b=c(0,X[1,],1)
# rest=c('>=','<=','<=','=')
# ###########################################################
# #set up lpSolveAPI object
# LP_API=make.lp(nrow=nrow(A),ncol=ncol(A))
# lp.control(LP_API,sense=objtype)
# set.objfn(LP_API,obj)
# for(i in 1:nrow(A)){
# set.row(LP_API,i,A[i,])
# }
# set.constr.type(LP_API,rest)
# set.rhs(LP_API,b)
# ##########################################################
# #solve with lpSolveAPI
# (status=solve(LP_API))
# #[1] 0
# get.objective(LP_API)
# #[1] 3.976337
# ##########################################################
# #solve with MMLP
# tmp2=MMLP(objtype=objtype,obj=obj,A=A,rest=rest,rhs=b)
# tmp2$objval
# #[1] 3.976337
# ################################################################
# 
# ################################################################
# # solve efficiency scores for all DMU's 
# time_API=0;time_MMLP=0
# effhat_API=0;effhat_MMLP=0
# 
# i=1
# for(i in 1:nDMU){
#  A[1,ncol(A)]=-Y[i,1] 
#  b=c(0,X[i,],1)
#  time0=seconds()
#  set.mat(LP_API,1,ncol(A),-Y[i,1])
#  set.rhs(LP_API,b)
#  status=solve(LP_API)
#  (effhat_API[i]=get.objective(LP_API)) 
#  time1=seconds()
#  time_API=time_API+(time1-time0)
# 
#  time0=seconds()
#  tmp2=MMLP(objtype=objtype,obj=obj,A=A,rest=rest,rhs=b)
#  (effhat_MMLP[i]=tmp2$objval)
#  time1=seconds()
#  time_MMLP=time_MMLP+(time1-time0)
# 
#  if(i%%100==0) plot(i,nDMU,main=paste('dmu, nDMU',i,nDMU))
# 
# } # end loop "for(i in 1:nDMU)"
# #############################################################
# # Compute eff scores using DEAboot with nboot=0 
# time0=seconds()
#  tmp=DEAboot(X,Y,orient='out',RTS='vrs',nboot=0)
# time1=seconds()
# time_DEAboot=time1-time0
# #############################################################
# summary(effhat_API-effhat_MMLP)
# #      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
# #-1.028e-09 -6.000e-13  0.000e+00  1.841e-10  7.000e-13  4.352e-07 
# summary(effhat_MMLP-tmp$effvals)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#      0       0       0       0       0       0 
# time_API;time_MMLP;time_DEAboot
# #[1] 33.3     # R looping time lpSolveAPI    (no bootstrapping)
# #[1] 12.22    # R looping time MMLP          (no bootstrapping)
# #[1] 6.63     # Fortran looping time DEAboot (no bootstrapping)
#############################################################
# 
# #############################################################
# # NOTE:These examples have been conducted with a dense primal
# # DEA problem.  In our experience MMLP is not time competitive 
# # with lpSolveAPI or other R lp packages when solving the 
# # sparse dual DEA problem with nDMU+1 constraints.
# #############################################################

}
