#############################################################################################
 DDEAboot=function(X,Y,orient='ddea',RTS='crs',nboot=250,bootlist=NULL,dx=NULL,dy=NULL,
 DX=NULL,DY=NULL,alpha=0.05,seedval=1001,MMLPV=2){
#############################################################################################
  if(!is.matrix(X))  X=as.matrix(X)
  if(!is.matrix(Y))  Y=as.matrix(Y)
  (nDMU=nrow(X));(nX=ncol(X));(nY=ncol(Y))
 
  if(is.null(bootlist)) bootlist=1:nDMU
  (nDMUboot=length(bootlist))
 
   if(!is.null(dx)) if(!is.matrix(dx)) dx=matrix(dx,nDMUboot,nX)
   if(!is.null(dy)) if(!is.matrix(dy)) dy=matrix(dy,nDMUboot,nY)
   if(!is.null(DX)) if(!is.matrix(DX)) DX=matrix(DX,nDMU,nX)
   if(!is.null(DY)) if(!is.matrix(DY)) DY=matrix(DY,nDMU,nY)
   

 ######################### 
  if(orient=='in'|orient=='IN')        orient=1
  if(orient=='out'|orient=='OUT')      orient=2
  if(orient=='inout'|orient=='INOUT')  orient=3
  if(orient=='ddea'|orient=='DDEA')    orient=4
  
  if(orient==4&(is.null(DX)|is.null(DY)))  orient=3
  orient
 
  if(RTS=='crs'|RTS=='CRS')        RTS=3
  if(RTS=='vrs'|RTS=='VRS')        RTS=1
  if(RTS=='drs'|RTS=='DRS')        RTS=2
  if(RTS=='irs'|RTS=='IRS')        RTS=4
 
  if(RTS!=1&RTS!=2&RTS!=3&RTS!=4)   RTS=3
 
 if(!is.null(dx)) if(ncol(dx)!=ncol(X))  stop('number of inputs  in dx do not match number of inputs in X') 
 if(!is.null(dy)) if(ncol(dy)!=ncol(Y))  stop('number of outputs  in dy do not match number of outputs in Y') 
 if(!is.null(dx)) if(nrow(dx)!=nDMUboot) stop('number of inputs  in dx do not match number of DMUs in bootlist') 
 if(!is.null(dy)) if(nrow(dy)!=nDMUboot) stop('number of outputs in dy do not match number of DMUs in bootlist') 

 
 if(!is.null(DX)) if(ncol(DX)!=ncol(X))  stop('number of inputs  in DX do not match number of inputs in X') 
 if(!is.null(DY)) if(ncol(DY)!=ncol(Y))  stop('number of outputs  in DY do not match number of outputs in Y') 
 if(!is.null(DX)) if(nrow(DX)!=nDMU) stop('number of inputs  in DX do not match number of DMUs') 
 if(!is.null(DY)) if(nrow(DY)!=nDMU) stop('number of outputs in DY do not match number of DMUs') 
 
  if(orient==1){
   DX=matrix(0,nDMU,nX)
   DY=matrix(0,nDMU,nY)
   for(i in 1:nDMU){
    DX[i,]=X[i,]
   }
  } # if(orient==1)
 
  if(orient==2){
   DX=matrix(0,nDMU,nX)
   DY=matrix(0,nDMU,nY)
   for(i in 1:nDMU){
    DY[i,]=Y[i,]
   }
  } # if(orient==2)
 
  if(orient==3){
   DX=matrix(0,nDMU,nX)
   DY=matrix(0,nDMU,nY)
   for(i in 1:nDMU){
    DX[i,]=X[i,]
    DY[i,]=Y[i,]
   }
  } # end if(orient==3)
 
 if(!is.null(dx)&!is.null(dy)) {
  for(i in 1:nDMUboot){
   (j=bootlist[i])
   DX[j,]=dx[i,]
   DY[j,]=dy[i,]
  }
 } # end if(!is.null(dx)&!is.null(dy))


 h=0.0
 effvals=rep(0,nDMU);effstatus=effvals
 boot=matrix(0,nDMUboot,nboot);bootstatus=boot
 
 effvals.bc=NA
 bias=NA
 var=NA
 CI=NA

  
 #############################################################################################
 # R package
 returned_data=.Fortran('DDEAboot',
         RTS=as.integer(RTS),
         nboot=as.integer(nboot),
         nDMU=as.integer(nDMU),
         nY=as.integer(nY),
         nX=as.integer(nX),
         nDMUboot=as.integer(nDMUboot),
         bootlist=as.integer(bootlist),
         Y=as.double(Y),
         X=as.double(X),
         DY=as.double(DY),
         DX=as.double(DX),
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
 boot[bootstatus<0]=NA
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
 } # end function DDEAboot
###########################################################
