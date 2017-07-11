rm(list=ls())

library(RJDBC)
library(RMySQL)

#
#MAKE DATA
#

#
set.seed(42)
#
m = 100000
M = matrix(NA, nrow=m, ncol=10)
for(i in 1:10){
	M[,i] = rnorm(m, 0, 1)
}

#
#STORE DATA
#

#
#connection
ch = dbConnect(MySQL(), user='nick', password='Nmfsswfsc2017', dbname='testdb')
#write table
dbWriteTable(ch, "hdf5Test", M)

##
##driver
#drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
##connection
#ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=Grunloh_db', 'nick.grunloh', 'Nmfsswfsc2017')
##write table
#dbWriteTable(ch, "hdf5Test", M)
