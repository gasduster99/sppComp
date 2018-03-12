rm(list=ls())

#install.packages('TTR')
library(TTR)

#
#FUNCTIONS
#

#
plot.stacked <- function(x,y, ylab="", xlab="", ncol=1, xlim=range(x, na.rm=T), ylim=c(0, 1.2*max(rowSums(y), na.rm=T)), border = NULL, col=rainbow(length(y[1,]))){

    plot(x,y[,1], ylab=ylab, xlab=xlab, ylim=ylim, xaxs="i", yaxs="i", xlim=xlim, t="n")
    bottom=0*y[,1]
    for(i in 1:length(y[1,])){
        top=rowSums(as.matrix(y[,1:i]))
        polygon(c(x, rev(x)), c(top, rev(bottom)), border=border, col=col[i])
        bottom=top
    }
    abline(h=seq(0,200000, 10000), lty=3, col="grey")
    legend("topleft", rev(colnames(y)), ncol=ncol, inset = 0, fill=rev(col), bty="0", bg="white", cex=0.8, col=col)
    box()
}

#
#MAIN
#

#
landDat = read.csv('landings.csv')[,c(1, 3, 4, 5, 6, 8)]
colnames(landDat) = c('year', 'qtr', 'mcat', 'port', 'gear', 'weight')
#
landWeight = aggregate(landDat$weight, by=list(landDat$year), sum)
#proportion of landed weight sampled
#total landed weight
#
#!!!!Weight sampled, proportion by market category!!!!
#
sampDat = read.csv('samples.csv')[,c(1, 3, 4, 6, 7, 8)]
colnames(sampDat) = c('year', 'qtr', 'mcat', 'port', 'gear', 'count')
#
sampYMC = aggregate(sampDat$count, by=list(sampDat$year, sampDat$mcat), sum)
sampSum = aggregate(sampDat$count, by=list(sampDat$year), sum)
sampLen = aggregate(sampDat$count, by=list(sampDat$year), length)
sampAvg = cbind(sampLen[,1], sampSum$x/sampLen$x)
#
years = sampAvg[,1]
mcats = unique(sampYMC[,2])
y = matrix(0, nrow=length(years), ncol=length(mcats))
for(i in 1:length(years)){
	for(j in 1:length(mcats)){
		truth = sampYMC[,1]==years[i] & sampYMC[,2]==mcats[j]
		if( any(truth) ){ y[i, j]=sampYMC[truth,3] }
	}
}
#
en = 8
emaOut = SMA(sampAvg[,2], n=en)
fill = c(sampAvg[1,2])
for(i in 2:(en-1)){ fill=c(fill, SMA(sampAvg[,2], n=i)[i]) }
fill = c(fill, na.omit(emaOut)[1])
#
endFill = c(sampAvg[length(sampAvg[,2]),2])
for(i in 2:(en-1)){ endFill=c(SMA(sampAvg[,2], n=i)[length(sampAvg[,2])-i], endFill) }

#dev.new()
pdf('stratAvgSamp.pdf', width=9.5, height=3.5)
plot(sampAvg, xlab='Year', ylab='Per Stratum \nAverage Sample Size')
lines(sampAvg[,1], c(emaOut[floor(en/2):length(emaOut)], emaOut[1:floor(en/2)])[1:length(sampAvg[,1])], lwd=3)
lines(sampAvg[1:floor(en/2),1], fill[1:floor(en/2)], lwd=3)
lines(sampAvg[length(emaOut[floor(en/2):length(emaOut)]):length(sampAvg[,1])], endFill[floor(en/2):length(endFill)], lwd=3)
lines(sampAvg[floor(en/2):(floor(en/2)+1),1], c(fill[floor(en/2)], emaOut[en]), lwd=3)
dev.off()
##
#dev.new()
#plot.stacked(sampLen[,1], y)
##
#write.csv(t(y), file='y.csv', col.names=as.character(years), row.names=as.character(mcats))

