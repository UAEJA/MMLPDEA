#############################################################################################
 DDEAnCm=function(X,Y,orient='ddea',RTS='crs',nboot=250,bootlist=NULL,DX=NULL,DY=NULL,mlist=NULL,seedval=1001,
 replaceum='F',MMLPV=2,alpha=0.05,CILag=1,plotum='F',plottxt=''){
#############################################################################################
  if(!is.matrix(X))  X=as.matrix(X)
  if(!is.matrix(Y))  Y=as.matrix(Y)
  (nDMU=nrow(X));(nX=ncol(X));(nY=ncol(Y))
 
  if(is.null(bootlist)) bootlist=1:nDMU
  (nDMUboot=length(bootlist))
 
   if(!is.null(DX)) if(!is.matrix(DX)) DX=matrix(DX,nDMUboot,nX)
   if(!is.null(DY)) if(!is.matrix(DY)) DY=matrix(DY,nDMUboot,nY)
   

 ######################### 
  if(is.null(mlist[1])){ 
   # compute mlist values
   mcells=20
   n2=nDMU
   (n1=floor(0.5*sqrt(nDMU)))
   (n2=floor(5*sqrt(nDMU)))
   if(n2>0.5*nDMU) n2=floor(0.5*nDMU)
 
   tmp=n2/n1
   (K=(n2/n1)^(1/(mcells-1)))
   (mlist=floor(n1*K^((1:mcells)-1)))
   
   (tmp=max(table(mlist)))

    if(tmp>=2)(mlist=floor(seq(n1,n2,length.out=mcells)))
    mlist
  
  }
  (mNUM=length(mlist))
 #########################
  (replaceum=ifelse(replaceum=='T',1,0))
 
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
 
 
 if(!is.null(DX)) if(ncol(DX)!=ncol(X))  stop('number of inputs  in DX do not match number of inputs in X') 
 if(!is.null(DY)) if(ncol(DY)!=ncol(Y))  stop('number of outputs  in DY do not match number of outputs in Y') 
 if(!is.null(DX)) if(nrow(DX)!=nDMUboot) stop('number of inputs  in DX do not match number of DMUs') 
 if(!is.null(DY)) if(nrow(DY)!=nDMUboot) stop('number of outputs in DY do not match number of DMUs') 
 
  if(orient==1){
   DX=matrix(0,nDMUboot,nX)
   DY=matrix(0,nDMUboot,nY)
   for(i in 1:nDMUboot){
    DX[i,]=X[bootlist[i],]
   }
  }
 
  if(orient==2){
   DX=matrix(0,nDMUboot,nX)
   DY=matrix(0,nDMUboot,nY)
   for(i in 1:nDMUboot){
    DY[i,]=Y[bootlist[i],]
   }
  }
 
  if(orient==3){
   DX=matrix(0,nDMUboot,nX)
   DY=matrix(0,nDMUboot,nY)
   for(i in 1:nDMUboot){
    DX[i,]=X[bootlist[i],]
    DY[i,]=Y[bootlist[i],]
   }
  }
 
 
  
  effvals=rep(0,nDMU);effstatus=effvals
  boot=array(0,dim=c(nDMUboot,mNUM,nboot));bootstatus=boot
  dim(boot)

 #############################################################################################
 # R package
 returned_data=.Fortran('DDEAnCm',
         RTS=as.integer(RTS),
         nDMUboot=as.integer(nDMUboot),
         bootlist=as.integer(bootlist),
         mNUM=as.integer(mNUM),
         mlist=as.integer(mlist),
         nboot=as.integer(nboot),
         replace=as.integer(replaceum),
         nDMU=as.integer(nDMU),
         nY=as.integer(nY),
         nX=as.integer(nX),
         Y=as.double(Y),
         X=as.double(X),
         DY=as.double(DY),
         DX=as.double(DX),
         effvals=as.double(effvals),
         effstatus=as.integer(effstatus),
         boot=as.double(boot),
         bootstatus=as.integer(bootstatus),
         seedval=as.integer(seedval),
         MMLPV=as.integer(MMLPV),
         PACKAGE="MMLPDEA")
 #############################################        
         mlist=returned_data$mlist
         effvals=returned_data$effvals[1:nDMUboot]
         effstatus=returned_data$effstatus[1:nDMUboot]
         boot=array(returned_data$boot,dim=c(nDMUboot,mNUM,nboot))
         bootstatus=array(returned_data$bootstatus,dim=c(nDMUboot,mNUM,nboot))
##############################################
CI=matrix(0,nDMUboot,2)
    bias=0
    effvals.bc=0

if(nboot>0){

if(plotum=='T'){
 par(mfrow=c(3,3),ask=F)
}

(n=nDMU)
(m=mlist)
(nM=length(m))

if(RTS!=3)(beta=2/(nX+nY+1))  # VRS
if(RTS==3)(beta=2/(nX+nY))    # CRS

jon=2
for(jon in 1:nDMUboot){
 (dmu=bootlist[jon])
 bootj=t(boot[jon,,])
 dim(bootj)

 if(plotum=='T') boxplot(bootj,main=paste('dmu',dmu,'boot boxplot'))

 S=0*bootj
 for(j in 1:nM) {
  S[,j]=((m[j]/n)^beta)*(bootj[,j]-effvals[jon])
 }
 if(plotum=='T') boxplot(S,main=paste('dmu',dmu,'STAT boxplot'))

#################################
 UL=apply(S,2,quantile,probs=(1-alpha/2))
 LL=apply(S,2,quantile,probs=(alpha/2))
 summary(UL)
 summary(LL)

 CL=effvals[jon]-UL
 CU=effvals[jon]-LL

 summary(CL)
 summary(CU)
 (ylim=range(c(CL,CU)))

 if(plotum=='T'){
  plot(m,CL,ylim=ylim,type='l',lty=2,col=3,main='Con ivals by m level',ylab='Eff')
  points(m,CU,type='l',lty=2,col=3)
#  abline(h=effvals[jon],col=2,lwd=2)
 }

 MCL=lagMat(CL,-CILag:CILag)
 MCU=lagMat(CU,-CILag:CILag)
 CLsd=apply(MCL,1,sd)
 CUsd=apply(MCU,1,sd)
 Csd=CLsd+CUsd
 (mpick=which(Csd==min(Csd,na.rm=TRUE)))
 (cl=CL[mpick[1]])
 (cu=CU[mpick[1]])

 if(plotum=='T'){
  plot(m,Csd,main=paste('CSD by m level',plottxt))
  abline(v=m[mpick[1]])
 }
 
 CI[jon,]=c(cl,cu)
 bias[jon]=mean(S[,mpick[1]])
 effvals.bc[jon]=effvals[jon]-bias[jon]


} # end loop for(jon in 1:nDMUboot)
} # end loop on if(nboot>0)
################################################## 
 if(orient==1){
  effvals=1-effvals
  effvals.bc=1-effvals.bc
  boot=1-boot
  bias=-1*bias
  CI=1-CI
  tmp=CI[,1]
  CI[,1]=CI[,2]
  CI[,2]=tmp
 }
 
if(orient==2){
 effvals=1+effvals
 effvals.bc=1+effvals.bc
 boot=1+boot
 CI=1+CI
}
################################################
if(plotum=='T'){
 par(mfrow=c(1,1),ask=F)
}
  return(list(effvals=effvals,effvals.bc=effvals.bc,bias=bias,
  mlist=mlist,boot=boot,mchosen=mpick[1],alpha=alpha,CI=CI,effstatus=effstatus,
  bootstatus=bootstatus,seedval=seedval))
 } # end function DDEAnCm
###########################################################
