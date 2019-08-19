rm(list=ls())

#
library(emdbook)

#
#
#

##
#pdf('heatPlot.pdf')
#curve3d(x*y, from=c(0,0), to=c(1,1), sys3d="image", n=700, main="f(x,y)=xy")
#lines(c(0,1), c(1,0), lwd=3)
#dev.off()
#
##
#pdf("constraint.pdf")
#curve(x*(1-x))
#dev.off()

#
pdf('253Pairs.pdf')
sp = read.csv("/media/nick/extraBig/25319781983M4/25319781983/MNT/TWL/1/1983/sppComp.csv")
pairs(sp[,c('BCAC', 'CLPR', 'WDOW', 'BANK', 'SNOS', 'CNRY', 'YTRK')])
dev.off()
