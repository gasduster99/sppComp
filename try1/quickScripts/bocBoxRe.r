rm(list=ls())

library(HDInterval)

#
#
#

#
num = 15
sp = read.csv('/media/nick/extraBig/fullTimeComplete/83to90/MCAT250/Top/avgModel/OLA/TWL/1/1985/sppComp.csv')
sp = sp[,head(rev(order(colMeans(sp))), num)]
who = colnames(sp)
#
bbBox = matrix(NA, nrow=num, ncol=2)
colnames(bbBox) = c('pMean', 'pMedian')
rownames(bbBox) = who
bbHDI = list()
for(w in who){
	bbBox[w,1:2] = c(mean(sp[,w]), median(sp[,w]))
	spIntHDI = HDInterval:::hdi.density(density(sp[,w], from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
	bbHDI[[w]] = matrix(spIntHDI[,], ncol=2)
	colnames(bbHDI[[w]]) = c('begin', 'end')
}

#
#PLOT
#

pdf(sprintf('../pictures/bocBoxRe.pdf'), height=6, width=17)
par(cex=1.5)
plot(0, 0, ylim=c(0, 1), xlim=c(1, num), xlab='', ylab='Proportion', xaxt='n', main='')
axis(1, at=1:num, labels=who)
for(i in 1:num){
	#
	for(j in 1:dim(bbHDI[[who[i]]])[1]){
		segments(i, bbHDI[[who[i]]][j,'begin'], i, bbHDI[[who[i]]][j,'end'], lwd=4)
	}
	#
	points(i, bbBox[i, 'pMean'], pch=19)#, col='darkorange')
}
dev.off()

pdf(sprintf('../pictures/bocBoxReDots.pdf'), height=6, width=17)
par(cex=1.5)
plot(0, 0, ylim=c(0, 1), xlim=c(1, num), xlab='', ylab='Proportion', xaxt='n', main='')
axis(1, at=1:num, labels=who)
for(i in 1:num){
	##
	#for(j in 1:dim(bbHDI[[who[i]]])[1]){
	#	segments(i, bbHDI[[who[i]]][j,'begin'], i, bbHDI[[who[i]]][j,'end'], lwd=4)
	#}
	##
	points(i, bbBox[i, 'pMean'], pch=19)#, col='darkorange')
}
dev.off()

