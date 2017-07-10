rm(list=ls())

#
library(h5)

#
#MAKE DATA
#

#
set.seed(42)
#
m = 100000
M = matrix(NA, nrows=m, ncols=10)
for(i in 1:10){
	M[,i] = rnorm(m, 0, 1)
}

#
#STORE DATA
#

#
file = h5file("test.h5")
