rm(list=ls())

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

write.csv(M, 'hdf5Test6.csv')

##
#h5createFile("hdf5Test.h5")
#h5createGroup("hdf5Test.h5","testGroup")
#h5write(M, "hdf5Test.h5","testGroup/M")
