ugen=function(n=1,seedval=1) {
#     x=rep(0.0,n) 
############################################################
# Dynamic loading
# If the library hasn't been loaded yet, load it

#        if (!is.loaded('ugen')) {
#            dyn.load('ugen.so')
#            }
#        returned_data=.Fortran('ugen', 
#                                 n=as.integer(n),
#                                 seedval=as.integer(seedval),
#                                 x=as.double(R))
#        dyn.unload('ugen.so')
#
############################################################

############################################################
# R package
        returned_data=.Fortran('ugen', 
                                 n=as.integer(n),
                                 seedval=as.integer(seedval),
                                 x=as.double(rep(0.0,n)),
                                 PACKAGE="MMLPDEA")
###########################################################        
        x=returned_data$x
        return(x)
        }
###########################################################        

