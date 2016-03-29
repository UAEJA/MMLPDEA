lagum=function(x,nlag=1) {
 nobx=length(x)
 if(nlag<0) x=rev(x)
 x0=rep(NA,abs(nlag))
 x1=x[1:(nobx-abs(nlag))]
 lagx=c(x0,x1)
 if(nlag<0){
  x=rev(x)
  lagx=rev(lagx)
 }
lagx
}

uplag=function(x,nlag=1){
 nobs=length(x)
 c(x[(nlag+1):nobs],rep(NA,nlag))
}

###########################################
# (x=1:10)
# [1] 1  2  3  4  5  6  7  8  9  10
#lagum(x,1)
# [1] NA  1  2  3  4  5  6  7  8  9
#lagum(x,3)
# [1] NA NA NA  1  2  3  4  5  6  7
#uplag(x)
# [1]  2  3  4  5  6  7  8  9 10 NA
#lagum(x,-1)
# [1]  2  3  4  5  6  7  8  9 10 NA
###########################################
