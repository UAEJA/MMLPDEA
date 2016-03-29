#############################################################################################
RDDEAnCm=function(X,Y,orient='ddea',RTS='crs',nboot=250,bootlist=NULL,DX=NULL,DY=NULL,mlist=NULL,seedval=1001,
replaceum='F',alpha=0.05,CILag=1,plotum='F',plottxt=''){
#############################################################################################
  seedval0=seedval
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
   for(i in 1:bootlist){
    DX[i,]=X[bootlist[i],]
   }
  }
 
  if(orient==2){
   DX=matrix(0,nDMUboot,nX)
   DY=matrix(0,nDMUboot,nY)
   for(i in 1:bootlist){
    DY[i,]=Y[bootlist[i],]
   }
  }
 
  if(orient==3){
   DX=matrix(0,nDMUboot,nX)
   DY=matrix(0,nDMUboot,nY)
   for(i in 1:length(bootlist)){
    DX[i,]=X[bootlist[i],]
    DY[i,]=Y[bootlist[i],]
   }
  }
 
  
  effvals=rep(0,nDMUboot);effstatus=effvals
  boot=array(0,dim=c(nDMUboot,mNUM,nboot));bootstatus=boot
  dim(boot)


  nr=nY+nX+1
  nc=nDMU+1

  ka=nr
  m=nr
  n=nc
  mxiter=1000
  ind=0
  ibasis=0
  iter=0

# construct LP data for DEA model
 obj=rep(0,nc)
 A=matrix(0,nr,nc)
 rhs=rep(0,nr)
 cons=rep('>=',nr)
 obj[nc]=1.0

 objtype='max'

 for(i in 1:nY){
  A[i,1:nDMU]=Y[,i]
  A[i,nc]=-DY[1,i]
  cons[i]='>='
  rhs[i]=Y[1,i]   
 }

 for(i in 1:nX){
  i1=nY+i
  A[i1,1:nDMU]=X[,i]
  A[i1,nc]=DX[1,i]  
  cons[i1]='<='
  rhs[i1]=X[1,i]    
 }

 A[nr,1:nDMU]=1.0

 #set CRS as default  RTS=3
  cons[nr]= '>='   
  rhs[nr]=0.0


 if(RTS==1) {  #VRS
  cons[nr]= '='  
  rhs[nr]=1.0
 }    


 if(RTS==2) { #DRS
  cons[nr]= '<='  
  rhs[nr]=1.0
 }    


 if(RTS==4) { #IRS
  cons[nr]= '>='  
  rhs[nr]=1.0
 }    

 ##########################################################
 #estimate eff scores for each dmu in mlist
 ##########################################################
 #set up base lpSolveAPI object
  LP_API=make.lp(nrow=nrow(A),ncol=ncol(A))
  lp.control(LP_API,sense=objtype)
  set.objfn(LP_API,obj)
  for(i in 1:nrow(A)){
  set.row(LP_API,i,A[i,])
  }
  set.constr.type(LP_API,cons)
  set.rhs(LP_API,rhs)
 ###########################################################
 #solve with lpSolveAPI
 (status=solve(LP_API))
 get.objective(LP_API)
 ###########################################################
 # solve efficiency scores for all DMU's in bootlist
 effhat=0
 (i1=nY+1)
 (i2=i1+nX-1)

i=1
for(i in 1:nDMUboot){

 (ipick=bootlist[i])
 
 A[1:nY,nc]=-1*DY[ipick,1:nY] 
 A[i1:i2,nc]=DX[ipick,1:nX] 

 rhs[1:nY]=Y[ipick,1:nY]  
 rhs[i1:i2]=X[ipick,1:nX]  
 
  for(j in 1:nrow(A)){
  set.row(LP_API,j,A[j,])
  }

 set.rhs(LP_API,rhs)

 #solve with lpSolveAPI
 (effstatus[i]=solve(LP_API))
 (effvals[i]=get.objective(LP_API))

 } # end for(i in 1:nDMUboot)

#delete.lp(LP_API)




#########################################
jbcount=0
# debugumoff
if(nboot>0) {
# bootstrap section
 nvals=1:nDMU
#
jb=1
# debugum off
 for(jb in 1:nboot){
  if(jb%%10==0) print(paste('bootstrap rep',jb,'of',nboot))


  (seedval=newseed(seedval))
  
  jpick=nCm(nvals,nDMU,replaceum,seedval)  
 
jm=1
#debugoff
 for(jm in 1:mNUM){
   (nm=mlist[jm])
  nmp1=nm+1
  Am=matrix(0,m,nmp1)
  objm=rep(0,nmp1)
  objm[nmp1]=1

  Am[1:m,1:nm]=A[1:m,jpick[1:nm]]
  
   LP_API=make.lp(nrow=nrow(Am),ncol=ncol(Am))
   lp.control(LP_API,sense=objtype)
   set.objfn(LP_API,objm)
   set.constr.type(LP_API,cons)

 i=1
 #debugoff 
 for(i in 1:nDMUboot){
   im=bootlist[i]   
   i1=nY+1
   i2=i1+nX-1
   rhs[1:nY]=Y[im,1:nY]  
   rhs[i1:i2]=X[im,1:nX]  

   Am[i1:i2,nmp1]=DX[im,1:nX] 
   Am[1:nY,nmp1]=-DY[im,1:nY] 

   Am[i1:i2,1]=X[im,1:nX] 
   Am[1:nY,1]=Y[im,1:nY] 

   for(ir in 1:nrow(Am)){
    set.row(LP_API,ir,Am[ir,])
   }
   set.rhs(LP_API,rhs)
 ###########################################################
 #solve with lpSolveAPI
 (ind=solve(LP_API))
 (z=get.objective(LP_API))

   if(ind==0)  boot[i,jm,jb]=z
    if(z<0) {
     boot[i,jm,jb]=-1000
     bootstatus[i,jm,jb]=10
    }
    if(z>1e9) {
     boot[i,jm,jb]=-1000
     bootstatus[i,jm,jb]=10
    }


  } # end for(i in 1:nDMUboot)

} # end for(jm in 1:mNUM)

    
} # end for(jb in 1:nboot)    

} #end if(nboot>0) then


#######################################################################
    CI = matrix(0, nDMUboot, 2)
    bias=0
    effvals.bc=0
    
    if (nboot > 0) {
        if (plotum == "T") {
            par(mfrow = c(3, 3), ask = F)
        }
        (n = nDMU)
        (m = mlist)
        (nM = length(m))
        if (RTS != 3) 
            (beta = 2/(nX + nY + 1))
        if (RTS == 3) 
            (beta = 2/(nX + nY))
        jon = 2
        for (jon in 1:nDMUboot) {
            (dmu = bootlist[jon])
            bootj = t(boot[jon, , ])
            dim(bootj)
            if (plotum == "T") 
                boxplot(bootj, main = paste("dmu", dmu, "boot boxplot"))
            S = 0 * bootj
            for (j in 1:nM) {
                S[, j] = ((m[j]/n)^beta) * (bootj[, j] - effvals[jon])
            }
            if (plotum == "T") 
                boxplot(S, main = paste("dmu", dmu, "STAT boxplot"))
            UL = apply(S, 2, quantile, probs = (1 - alpha/2))
            LL = apply(S, 2, quantile, probs = (alpha/2))
            summary(UL)
            summary(LL)
            CL = effvals[jon] - UL
            CU = effvals[jon] - LL
            summary(CL)
            summary(CU)
            (ylim = range(c(CL, CU)))
            if (plotum == "T") {
                plot(m, CL, ylim = ylim, type = "l", lty = 2, 
                  col = 3, main = "Con ivals by m level", ylab = "Eff")
                points(m, CU, type = "l", lty = 2, col = 3)
                abline(h = effvals[jon], col = 2, lwd = 2)
            }
            MCL = lagMat(CL, -CILag:CILag)
            MCU = lagMat(CU, -CILag:CILag)
            CLsd = apply(MCL, 1, sd)
            CUsd = apply(MCU, 1, sd)
            Csd = CLsd + CUsd
            (mpick = which(Csd == min(Csd, na.rm = TRUE)))
            (cl = CL[mpick[1]])
            (cu = CU[mpick[1]])
            if (plotum == "T") {
                plot(m, Csd, main = paste("CSD by m level",plottxt))
            }
            CI[jon, ] = c(cl, cu)
            bias[jon]=mean(S[,mpick[1]])
            effvals.bc[jon]=effvals[jon]-bias[jon]
        }
    }

    if (orient == 1) {
        effvals=1-effvals
        effvals.bc=1-effvals.bc
        boot=1-boot
        bias=-1*bias
        CI=1-CI
        tmp=CI[,1]
        CI[,1]=CI[,2]
        CI[,2]=tmp        
    }

    if (orient == 2) {
        effvals=1+effvals
        effvals.bc=1+effvals.bc
        boot = 1 + boot
        CI = 1 + CI
    }
    if (plotum == "T") {
        par(mfrow = c(1, 1), ask = F)
    }
  return(list(effvals=effvals,effvals.bc=effvals.bc,bias=bias,
  mlist=mlist,boot=boot,mchosen=mpick[1],alpha=alpha,CI=CI,effstatus=effstatus,
  bootstatus=bootstatus,seedval=seedval))

} # end function RDDEAnCm
#########################################################################


