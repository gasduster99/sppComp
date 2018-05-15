suppressMessages(library(doParallel, quietly=FALSE))
suppressMessages(library(foreach, quietly=FALSE))
#
threads = 48
saveFile = '/tmp/test.csv' #RtmpZLgZmw/test.csv'
out =  matrix(rep(1, 10000), nrow=100, ncol=100)
#
write.table(out, saveFile, sep=',', quote=F, row.names=F, col.names=F)
registerDoParallel(cores=threads)
foreach( p=1:threads )%dopar%{
	while(T){ write.table(out, saveFile, sep=',', append=T, quote=F, row.names=F, col.names=F) }
}
