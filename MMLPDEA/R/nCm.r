##################################################
nCm=function(nvals=1:10,m=5,replaceum='F',seedval=1001){
##################################################
 n=length(nvals)
 if(m>n&replaceum!='T') replaceum='T'
 
 mvals=rep(0,m)
 ivals=nvals
 u=ugen(m,seedval)

 if(replaceum=='T') {
  pick=floor(u*n)+1
  pick[pick>n]=n
  mvals=ivals[pick]
 } #end if(replace=='T')

 if(replaceum!='T') {
  for(i in 1:m){
   nleft=n-i+1
   k=floor(u[i]*nleft)+1
   if(k>nleft) k=nleft
   mvals[i]=ivals[k]
   ivals[k]=ivals[nleft]
  }
 } #end if(replace!='T')
 mvals
} # end function nCm
###################################################

