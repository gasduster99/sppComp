rm(list=ls())

#
library(vioplot)
library(RColorBrewer)

#
#
#


#MCAT250/Top/Space[14]/OLA/TWL/star/1990/sppComp.csv                                  vvvvvv avgModel
pathHead = '/home/nick/Documents/sppComp/inla/space_MCAT250_83to90HindTry/MCAT250/Top/Space1/'
pathTail = '/star/1990/sppComp.csv'
#
num = 5
reds = rev(head(tail(brewer.pal(9, 'YlOrRd'), 4), 3))
yels = rev(tail(head(brewer.pal(9, 'YlOrRd'), 4), 3))
grns = rev(brewer.pal(9, 'Greens')[6:8])
cols = cbind(reds, yels, grns)
#
spp = c('BANK', 'BLGL', 'BCAC', 'SNOS', 'CLPR')
ports = c('OSB', 'OLA', 'OSD')
gears = c('TWL', 'NET', 'HKL')
i = 1
for(p in ports){
	j = 1
	for(g in gears){
		#
		sp = read.csv(sprintf('%s%s/%s%s', pathHead, p, g, pathTail))
		sp = sp[,head(rev(order(colMeans(sp))), num)] 
		#
		pdf(sprintf('../pictures/vio%s%s.pdf', p, g), width=10)
		par(cex=1.5)
		vioplot(sp[,spp[1]], sp[,spp[2]], sp[,spp[3]], sp[,spp[4]], sp[,spp[5]], #sp[,6],
        		names=spp,#colnames(sp),
        		ylim=c(0, 1),
        		col=cols[j,i],
        		drawRect=F
		)
		dev.off()
		#
		j = j + 1
	}
	#
	i = i+1
}

