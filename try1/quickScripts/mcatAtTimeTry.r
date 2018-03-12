rm(list=ls())

#install.packages('TTR')
library(TTR)
#install.packages('qcc')
library(qcc)

#
#FUNCTIONS
#

#
getNums = function(X){
	#X: a vector of strings containing numbers
	#
	#value: a vector of numbers as extracted from X
	#
	out = unique(na.omit(as.numeric(unlist(strsplit(X, "[^0-9]+")))))
	#
	return(out)
}

#
#CLEAN DATA
#

#mcatAtTime.csv
data = read.csv('mcatAtTime.csv')
#cleanup the year naming 
colnames(data) = c(NA, getNums(colnames(data)))
#transpose to put the data in a more typical column major form
data = t(data)
#cleanup the mcat naming
colnames(data) = as.character(data[1,])
data = data[2:dim(data)[1],]

#
#NUMER MCAT 'TILL %
#

#
prop = 0.99 #1#0.95
#
years = as.numeric(rownames(data))
nYear = length(years)
#in every year get a sorted array of the mcat with the most catch
yearTops = list()
for(i in 1:nYear){
	#
	yearMcat = sort(na.omit(data[i,]))
	tot = sum(yearMcat)
	#
	yearTops[[i]] = numeric(0)
	runningP = 0
	j = 1
	while( runningP<=prop ){
		#	
		yearTops[[i]] = c(yearTops[[i]], yearMcat[j]/tot)
		runningP = runningP + yearMcat[j]/tot
		#names(which(data[i,]==max(yearMcat)))
		j = j+1
	}
}
names(yearTops) = as.factor(years)

##
#numMcat = unlist(lapply(yearTops, length))
#emaOut = EMA(numMcat, n=9)
#fill = c(numMcat[1], 
#	EMA(numMcat, n=2)[2], 
#	EMA(numMcat, n=3)[3], 
#	EMA(numMcat, n=4)[4], 
#	EMA(numMcat, n=5)[5], 
#	EMA(numMcat, n=6)[6],
#	EMA(numMcat, n=7)[7],
#	EMA(numMcat, n=8)[8],
#	na.omit(emaOut)[1]
#)
##dev.new()
##ewmaOut = ewma(numMcat)
##smaOut = SMA(numMcat)
##lmOut5 = lm(numMcat~poly(years, 5))
##
##pdf('nMcatsEMA.pdf', width=9.5, height=3.5)
##dev.new(width=10)
#plot(years, numMcat,
#	ylab = '# Market Categories',
#	xlab = 'Year'#,
#	#mgp=c(2,1,.5), 
#	#las=1
#)
#lines(years, emaOut, lwd=3)
##lines(years[1:9], rep(na.omit(emaOut)[1], 9))
#lines(years[1:9], fill, lwd=3)
##lines(years, smaOut, col='red')
##lines(years, predict(lmOut5))
##dev.off()

#
years = years[years>=1977]
numMcat = unlist(lapply(yearTops, length))
numMcat = numMcat[names(numMcat)%in%years]
emaOut = SMA(numMcat, n=8)
fill = c(numMcat[1],
        SMA(numMcat, n=2)[2],
        SMA(numMcat, n=3)[3],
        SMA(numMcat, n=4)[4]
)
        #EMA(numMcat, n=5)[5], 
        #EMA(numMcat, n=6)[6],
        #EMA(numMcat, n=7)[7],
        #EMA(numMcat, n=8)[8],
endFill = c(
        #na.omit(emaOut)[1],
        emaOut[length(emaOut)],
        #SMA(numMcat, n=4)[length(numMcat)-3],
        SMA(numMcat, n=3)[length(numMcat)-2],
        mean(c(numMcat[length(numMcat)-1], numMcat[length(numMcat)])),
        numMcat[length(numMcat)]
)
#dev.new()
#ewmaOut = ewma(numMcat)
#smaOut = SMA(numMcat)
#lmOut5 = lm(numMcat~poly(years, 5))
#
pdf('nMcatsEMA.pdf', width=9.5, height=3.5)
#dev.new(width=10)
plot(years, numMcat,
        ylab = '# Market Categories',
        xlab = 'Year'#,
        #mgp=c(2,1,.5), 
        #las=1
)
en=8
#c(emaOut[floor(en/2):length(emaOut)], emaOut[1:floor(en/2)])[1:length(sampAvg[,1])]
lines(years, c(emaOut[floor(en/2):length(emaOut)], emaOut[1:floor(en/2)])[1:length(years)], lwd=3)
#lines(years[1:9], rep(na.omit(emaOut)[1], 9))
lines(years[1:length(fill)], fill, lwd=3)
lines(years[length(emaOut[floor(en/2):length(emaOut)]):length(years)], endFill[1:length(endFill)], lwd=3)
lines(years[length(fill):(length(fill)+1)], c(fill[floor(en/2)], emaOut[en]), lwd=3)
#lines(years[], fill, lwd=3)
#lines(years, smaOut, col='red')
#lines(years, predict(lmOut5))
dev.off()
