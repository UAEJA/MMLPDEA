####################################################################
# The following code has been modified from the code used in the 
# Benchmarking Package. We have noted where we modified the original
# code. 
####################################################################
###########################################################
dea.boot_DEAboot <- function(X,Y, NREP=200, EFF=NULL, RTS="vrs", 
       ORIENTATION="in", alpha=0.05, XREF=NULL, YREF=NULL, 
       EREF=NULL, DIRECT=NULL, TRANSPOSE=FALSE, LP=FALSE,
       printum=FALSE,printmod=5,saveum=FALSE,seedval=1001)
{
#############################################################
#(DEAboot modified)//
    time1=seconds()
    XREF=X
    YREF=Y
    xstar=NULL
    ystar=NULL
#(DEAboot modified)\\

   if ( !is.null(DIRECT) )
      stop("Use of DIRECT does not yet work in dea.boot")

   rts_ <- c("fdh","vrs","drs","crs","irs","irs","add")
   if ( is.numeric(RTS) )  {
      RTStemp <- rts_[1+RTS] # the first fdh is number 0
      RTS <- RTStemp
   }
   RTS <- tolower(RTS)
   rts <- which(RTS == rts_) -1 
   # if ( !(RTS %in% rts) ) 
 
   if ( rts < 1 || rts > 4 )
      stop(paste("Invalid value of RTS in call to boot:",RTS))


   orientation_ <- c("in-out","in","out","graph")
   if ( is.numeric(ORIENTATION) )  {
      ORIENTATION_ <- orientation[ORIENTATION+1]  # "in-out" er nr. 0
      ORIENTATION <- ORIENTATION_
   }
   ORIENTATION <- tolower(ORIENTATION)
   orientation <- which(ORIENTATION == orientation_) -1
   if ( orientation < 1 || 3 < orientation )
      stop("Invalid value of ORIENTATION in call to boot")

   # print(paste("rts =",rts,"   orientation=",orientation))

   if ( class(X)!="matrix" )
      stop("X is not a matrix")
   if ( class(Y)!="matrix" )
      stop("Y is not a matrix")
   if ( !is.null(XREF) && class(XREF)!="matrix" )
      stop("XREF is not a matrix")
   if ( !is.null(YREF) && class(YREF)!="matrix" )
      stop("YREF is not a matrix")

   if ( !is.null(EREF) && (is.null(XREF) || is.null(YREF)) )
      stop("When EREF is present then XREF and YREF must also be present") 

   .xyref.missing <- FALSE
   if ( missing(XREF) || is.null(XREF) )  {
      .xyref.missing <- TRUE
      XREF <- X
   }
   if ( missing(YREF) || is.null(YREF) )  {
      .xyref.missing <- TRUE && .xyref.missing
      YREF <- Y
   }
   
   if ( TRANSPOSE )  {
      X <- t(X)
      Y <- t(Y)
      XREF <- t(XREF)
      YREF <- t(YREF)
      if ( !is.null(DIRECT) & class(DIRECT)=="matrix" )
         DIRECT <- t(DIRECT)
   }
   orgKr <- dim(XREF)

   rNames <- rownames(XREF)
   if ( is.null(rNames) & !is.null(rownames(YREF)) )
      rNames <- rownames(YREF)

   m <- dim(X)[2]  # number of inputs
   n <- dim(Y)[2]  # number of outputs
   K <- dim(X)[1]  # number of units, firms, DMUs
   Ky <- dim(Y)[1]  
   Kr <- dim(XREF)[1] # number of units,firms in the reference technology

   if ( is.null(EFF) )
      eff <- dea(X,Y, RTS, ORIENTATION, XREF, YREF, FAST=TRUE)
#(DEAboot modified)//
#  else if ( class(EFF)=="Farrell" )
#     eff <- EFF$eff
#  else 
#     eff <- EFF
#(DEAboot modified)\\
   # if (LP) print(eff)

   if ( length(eff) != K ) 
      stop("The length of EFF must be the number of firms in X and Y")

   if ( !.xyref.missing && is.null(EREF) )
      EREF <- dea(XREF, YREF, RTS, ORIENTATION, FAST=TRUE)

   # Hvis XREF ikke er defineret i kald
   if ( !.xyref.missing && !is.null(EREF) )
      eff <- EREF
   # if (LP) print(eff)

   if (ORIENTATION=="out")  farrell<-TRUE else farrell<-FALSE


   # Beregn vinduesbredden til udglatning af efficiensers fordeling.
   # Lav det alene for efficencer på 1 eller over, dvs. for Farrell
   # output eller Shephard input orienterede efficiencer fordi det er
   # dem der samples på.
   # Fjern 1-erne saa de ikke dominerer og spejl efficiencer i 1,
   # Daraio and Simar (2007, 61) ligning (3.23) og (3.26)
   dist <- eff
   if ( !farrell )  dist <- 1/dist
#(DEAboot modified)\\
    if(printum==TRUE){
    print("Range of dist: ")
    print(range(dist))
}
#//(DEAboot modified)
#print(dist)
   # Behold efficiencer over 1, drop 1-erne når bredden beregnes
zeff <- eff[dist>1+1e-6]
   if ( length(zeff) == 0 )  {
      cat("No unit with efficiency different from 1.0000.\n", quote=FALSE)
      stop("The range of efficiencies is degenrate, 'dea.boot' stops-")
   }
   # Spejling om 1
neff <- c(zeff,2-zeff)
   # Ratio |adjust| som bredden skal ganges med fordi der er for mange
   # elementer i den spejlede vektor som båndbredden bregnes ud fra
   adjust <- sd(dist)/sd(neff) * (length(neff)/length(dist))^(1/5)
   # Ligning (3.28), (3.30) og (3.31) i Silverman (1986, 45--47)
   std <- sd(neff) 
   iqr <- IQR(neff)/1.349
   # Hvis bredden er for lille kan IQR vaere 0 og saa skal std bruges
   if ( iqr > 1e-6 && iqr < std  ) std <- iqr
   h0 <- .9 * std * length(neff)^(-1/5)
   # h0 <- 1.06 * std * length(neff)^(-1/5)
   h<-adjust*h0
if (LP) cat("Bandwidth =",h,"\n")

   boot <- matrix(NA, nrow=K, ncol=NREP)
#(DEAboot modified)//
    bcount=0
#    seedval=1001
    b=1
#(DEAboot modified)\\

if ( ORIENTATION == "in" )  {
   for ( b in 1:NREP )  {
            
      # if (LP) print(paste(b," -----"))

#(DEAboot modified)//
          #(estar <- dea.sample(eff, h, K))
    tmp <- dea.sample_DEAboot(eff, h, K,seedval)
    estar=tmp$estar
    seedval=tmp$seedval
#(DEAboot modified)\\

      # if (LP) print(estar)
      # if (LP) print(eff/estar)
      # estar/eff ganges på hver soejle i X; benytter at R har data
      # efter søjler, byrow=FALSE er default i R.
      xstar <- eff/estar * XREF

      boot[,b] <- dea(X,Y, RTS, ORIENTATION, XREF=xstar, YREF, FAST=TRUE)

#(DEAboot modified)//
    time2=seconds()
if(printum==TRUE&b%%printmod==0) print(paste(b,'time =',time2-time1))
#(DEAboot modified)\\

      # if (LP) print(boot[,b])
   }  # b in 1:NREP
} else if ( ORIENTATION == "out" )  {   # farrel er TRUE
   for ( b in 1:NREP )  {
      # Farrell output efficiencer
#(DEAboot modified)//
         #estar <- dea.sample(eff, h, K)
    tmp <- dea.sample_DEAboot(eff, h, K,seedval)
    estar=tmp$estar
    (seedval=tmp$seedval)
#(DEAboot modified)\\

      ystar <- eff/estar * YREF

      boot[,b] <- dea(X,Y, RTS, ORIENTATION, XREF, YREF=ystar, FAST=TRUE)

#(DEAboot modified)//
    time2=seconds()
if(printum==TRUE&b%%printmod==0) print(paste(b,'time =',time2-time1))
#(DEAboot modified)\\


   }  # b in 1:NREP
} else if ( ORIENTATION == "graph" )  {
   for ( b in 1:NREP )  {

#(DEAboot modified)//
    # estar <- dea.sample(eff, h, K)
    tmp <- dea.sample_DEAboot(eff, h, K,seedval)
    estar=tmp$estar
    seedval=tmp$seedval
#(DEAboot modified)\\

      xstar <- eff/estar * XREF
      ystar <- eff/estar * YREF

      boot[,b] <- dea(X,Y, RTS, ORIENTATION, XREF=xstar, YREF=ystar,FAST=TRUE)

#(DEAboot modified)//     
    time2=seconds()
if(printum==TRUE&b%%printmod==0) print(paste(b,'time =',time2-time1))
#(DEAboot modified)//
   }  # b in 1:NREP
} else {
   stop("Unknown ORIENTATION")
}


if ( ORIENTATION == "out" )  {  #farrel er TRUE
   # Efficiencer stoerre end 1
   bias <- rowMeans(boot, na.rm=TRUE) - eff
   eff.bc <- eff - bias

   ci_ <- t(apply(eff-boot,1, quantile, 
            probs=c(0.5*alpha, 1-0.5*alpha), type=9, na.rm=TRUE))
   ci <- eff + ci_

   var <- apply(boot,1,var)
   var0 <- NULL
} else {
   # Efficiencer er mindre end 1, men blev bootstrappet som var de
   # over 1, og der skal derfor regnes på dem som var de over 1
   bias <- 1/eff - rowMeans(1/boot, na.rm=TRUE)
   eff.bc <- eff - bias

   boot_ <- 1/boot - 1/eff
   ci_ <- 
     t(apply(boot_,1, quantile, 
       probs=c(0.5*alpha, 1-0.5*alpha), type=9, na.rm=TRUE))
   ci  <-  1/(1/eff - ci_)
   rm(boot_, ci_)

   # ci1 <- 1/(1/eff+t(apply(1/eff-1/boot,1, quantile, 
   #             probs=c(1-0.5*alpha, 0.5*alpha), type=9, na.rm=TRUE)))

   # boot_ <- boot - eff
   # ci_ <- 
   #   t(apply(boot_,1, quantile, 
   #     probs=c(0.5*alpha, 1-0.5*alpha), type=9, na.rm=TRUE))
   # ci0 <-  eff + ci_
   # rm(boot_, ci_)

   var0 <- apply(1/boot,1,var)*eff^2
   var <- apply(boot,1,var)
}

   # Sorter boot efficiencerne, det goer FEAR::boot.sw98; men er det
   # ikke tåbeligt at gøre det?
   # sboot <- t(apply(boot,1,sort))
   # boot <- sboot

   bb <- list(eff=eff, eff.bc=eff.bc,
           bias=bias,
           var=var, var0=var0,
           conf.int=ci,
           eref=EREF, h=h,boot=boot)

#(DEAboot modified)//     
    time2=seconds()
    if(printum==TRUE) print(paste('time =',time2-time1))
#(DEAboot modified)\\

 if(saveum!=FALSE) {
   bb <- list(eff=eff, eff.bc=eff.bc,
           bias=bias,
           var=var, var0=var0,
           conf.int=ci,
           eref=EREF, h=h,boot=boot,
           X=X,Y=Y,RTS=RTS,
           ORIENTATION=ORIENTATION,
           xstar=xstar,ystar=ystar
           )

 }
        return( bb )
}  # boot
#######################################################################

####################################################################
# following code has been modified from the code used in the 
# Benchmarking Package
####################################################################
# Bootstrap sample fra efficiencer over 1, Shephard input eller Farrell
# output
dea.sample_DEAboot <- function(e, h, K=NULL,seedval)  {
 # sample for Shaphard input efficiens
   if ( min(e) < 1-1e-6 )  {
      e <- 1/e   
      farrell <- TRUE
   } else {
      farrell <- FALSE
   }
   if ( is.null(K) )
      K <- length(e)
   # spejling i 1
   espejl <- c(e, 2-e)
#seedval=seedval+1
seedval=newseed(seedval)
beta <- sampleum(K,espejl,seedval)
#seedval=seedval+1
seedval=newseed(seedval)
z=normgen(K,seedval)
   etilde <- beta + h * z
   # Juster saa varians bliver rigtig, Daraio and Simar (2007, 62) [3]
   estar <- mean(beta) + (etilde-mean(beta)) / sqrt(1+h^2/var(espejl))
   # estar <- 1 + (etilde-1) / sqrt(1+h^2/var(espejl))
   # Spejl for at faa vaerdier storre end 1
   estar <- ifelse(estar < 1, 2-estar, estar)
   # estar[estar < 1] <- 2 - estar[estar < 1]
   if ( farrell )  {
      # print("Omsæt til Farrell")
      estar <- 1/estar
   }
   list(estar=estar,seedval=seedval)

}# end function dea.sample_DEAboot



##################################################################
sampleum=function(nobs,x,seedval=1001){
 u=ugen(nobs,seedval)
 n=length(x)
 pickum=floor(u*n)+1
 pickum[pickum>n]=n
 (samples=x[pickum])
} # end function sampleum
#################################################################
seconds=function(){
 time1=proc.time()
 as.numeric(time1[3])
} # end function seconds
################################################################
newseed=function(seedval){
u=ugen(1,seedval)
newseed=floor(u[1]*2147483645)
if(newseed==0) newseed=1
newseed
}
################################################################


