###############################################################
MMLP=function(objtype='max',obj,A,rest,rhs,itermax=1000,nsims=1,MMLPV=2){
 (nr=nrow(A))
 (nc=ncol(A))
 if(length(obj)!=nc|length(rest)!=nr|length(rhs)!=nr) stop("dimension error in obj,rest,or b")
 (objmax=ifelse(objtype=='max',1,0))
 restcodes=0
 for(j in 1:nr){
  if(rest[j]=='<='|rest[j]=='<') restcodes[j]=1
  if(rest[j]=='>='|rest[j]=='>') restcodes[j]=2
  if(rest[j]=='=='|rest[j]=='=') restcodes[j]=3
 }

objval=0
xvals=rep(0,nc+nr)
duals=rep(0,nr)
indstat=0

returned_data=.Fortran('MMLP',
        nsims=as.integer(nsims),
        nr=as.integer(nr),
        nc=as.integer(nc),
        objmax=as.integer(objmax),
        obj=as.double(obj),
        A=as.double(A),
        restcodes=as.integer(restcodes),
        rhs=as.double(rhs),
        itermax=as.integer(itermax),
        objval=as.double(objval),
        xvals=as.double(xvals),
        duals=as.double(duals),
        indstat=as.integer(indstat),
        MMLPV=as.integer(MMLPV),
        PACKAGE="MMLPDEA")
################################        
        objval=returned_data$objval
        xvals=returned_data$xvals
        duals=returned_data$duals
        indstat=returned_data$indstat
################################
list(objval=objval,xvals=xvals,duals=duals,indstat=indstat)
} # end function MMLP
#############################################################