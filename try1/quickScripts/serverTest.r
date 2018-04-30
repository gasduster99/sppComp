saveFile = '/tmp/test.csv' #RtmpZLgZmw/test.csv'
out =  matrix(rep(1, 10000), nrow=100, ncol=100)
write.table(out, saveFile, sep=',', quote=F, row.names=F, col.names=F)
while(T){ write.table(out, saveFile, sep=',', append=T, quote=F, row.names=F, col.names=F) }
