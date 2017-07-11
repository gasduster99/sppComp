rm(list=ls())


#https://stackoverflow.com/questions/15974643/how-to-deal-with-hdf5-files-in-r
library(rhdf5)

#
#MAKE DATA
#

#
set.seed(42)
#
m = 1000000
M = matrix(NA, nrow=m, ncol=10)
for(i in 1:10){
	M[,i] = rnorm(m, 0, 1)
}

#
#STORE DATA
#

#
h5createFile("hdf5Test6.h5")
h5createGroup("hdf5Test6.h5","testGroup")
h5write(M, "hdf5Test6.h5","testGroup/M")
