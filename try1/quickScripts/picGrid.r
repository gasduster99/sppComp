rm(list=ls())

#
library(vioplot)
library(RColorBrewer)

#
#
#

#
ws = c(0.98794663, 0.01205337)
ms = c('Space1', 'Space4')
#MCAT250/Top/Space[14]/OLA/TWL/star/1990/sppComp.csv                                  vvvvvv avgModel
pathHead = '/home/nick/Documents/sppComp/inla/space_MCAT250_83to90HindTry/MCAT250/Top/Space4/'
pathTail = '/qStar/yStar/sppComp.csv'#'/star/1990/sppComp.csv'
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
#
#M:6616
M=5000
ws = c(0.98794663, 0.01205337)
mws = M*ws
ms = c('Space1', 'Space4')
i = 1
for(p in ports){
	j = 1
	for(g in gears){	
		#MCAT250/Top/Space[14]/OLA/TWL/star/1990/sppComp.csv                                   vvvvvv avgModel
		pathHead = '/home/nick/Documents/sppComp/inla/space_MCAT250_83to90HindTry/MCAT250/Top/'#Space4/'
		pathTail = '/qStar/yStar/sppComp.csv'
		spAvg = c()
		for(im in 1:length(ms)){
			#
			sp = read.csv(sprintf('%s%s/%s/%s%s', pathHead, ms[im], p, g, pathTail))
			sp = sp[!is.na(sp[,1]),]
			sp[is.na(sp)] = 0
			spAvg = rbind(spAvg, sp[1:mws[im], head(rev(order(colMeans(sp))), num)])	
		}
		
		pdf(sprintf('../pictures/vioStarAvg%s%s.pdf', p, g), width=10)
		par(cex=1.5)
		vioplot(spAvg[,spp[1]], spAvg[,spp[2]], spAvg[,spp[3]], spAvg[,spp[4]], spAvg[,spp[5]], #sp[,6],
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
