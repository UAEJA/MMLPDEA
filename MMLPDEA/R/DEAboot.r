#############################################################################################
DEAboot=function(X,Y,orient='in',RTS='crs',nboot=250,bootlist=NULL,alpha=0.05,seedval=1001,MMLPV=2){
#############################################################################################
 if(!is.matrix(X)) X=as.matrix(X)
 if(!is.matrix(Y)) Y=as.matrix(Y)
 (nDMU=nrow(X));(nX=ncol(X));(nY=ncol(Y))

 if(is.null(bootlist)) bootlist=1:nDMU
 (nDMUboot=length(bootlist))

 if(orient=='in'|orient=='IN')    (orient=1)
 if(orient=='out'|orient=='OUT')  (orient=2)

 if(RTS=='crs'|RTS=='CRS')        RTS=3
 if(RTS=='vrs'|RTS=='VRS')        RTS=1
 if(RTS=='drs'|RTS=='DRS')        RTS=2
 if(RTS=='irs'|RTS=='IRS')        RTS=4

 if(orient!=1&orient!=2)           orient=1 
 if(RTS!=1&RTS!=2&RTS!=3&RTS!=4)   RTS=3
 
 h=0.0
 effvals=rep(0,nDMU);effstatus=effvals
 boot=matrix(0,nDMUboot,nboot);bootstatus=boot

effvals.bc=NA
bias=NA
var=NA
ci=NA
boot=boot
bootstatus=bootstatus
#############################################################################################
# R package
returned_data=.Fortran('DEAboot',
 orient=as.integer(orient),
 RTS=as.integer(RTS),
 nboot=as.integer(nboot),
 nDMU=as.integer(nDMU),
 nY=as.integer(nY),
 nX=as.integer(nX),
 nDMUboot=as.integer(nDMUboot),
 bootlist=as.integer(bootlist),
 Y=as.double(Y),
 X=as.double(X),
 h=as.double(h),
 effvals=as.double(effvals),
 effstatus=as.integer(effstatus),
 boot=as.double(boot),
 bootstatus=as.integer(bootstatus),
 seedval=as.integer(seedval),
 MMLPV=as.integer(MMLPV),
 PACKAGE="MMLPDEA")
#############################################        
 (h=returned_data$h)
 effvals=returned_data$effvals[bootlist]
 effstatus=returned_data$effstatus[bootlist]
 boot=matrix(returned_data$boot,nDMUboot,nboot)
 bootstatus=matrix(returned_data$bootstatus,nDMUboot,nboot)
 boot[bootstatus!=0]=NA
#############################################
if(nboot>0){ 
  bias=rowMeans(boot, na.rm=TRUE)-effvals
  effvals.bc=effvals-bias
  citmp=t(apply(effvals-boot,1,quantile,probs=c(0.5*alpha,1-0.5*alpha),type=9,na.rm=TRUE))
  CI <- effvals+citmp
  rm(citmp)
var <- apply(boot,1,var)
}
return(list(h=h,effvals=effvals,effvals.bc=effvals.bc,bias=bias,var=var,
boot=boot,alpha=alpha,CI=CI,effstatus=effstatus,bootstatus=bootstatus,seedval=seedval))
} # end function DEAboot
###########################################################        

