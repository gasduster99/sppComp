rm(list=ls())

#
library(boot)
library(mclust)
library(tmvtnorm)
library(latex2exp)
library(RColorBrewer)
library(fitdistrplus)

#
#DATA
#

#NOTE: change years and port appropriatly (?spp?)
minYear = 1978 #1983 #
maxYear = 1982 #1990 #
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')
sppList = c('WDOW', 'BCAC', 'CLPR', 'BANK', 'YTRK', 'BLGL', 'DBRK', 'CNRY', 'SNOS', 'CWCD', 'POP', 'BRNZ', 'MXRF') #'CMEL')

#
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#SAMPLE
sam = read.table(sprintf('./dataMatters/data%dTo%d.csv', minYear, maxYear), sep=',', header=T, stringsAsFactors=F)
ss = aggregate(sam$weight, by=list(species=sam$species, portComplex=sam$portComplex, year=sam$year, qtr=sam$qtr, gearGroup=sam$gearGroup, marketCategory=sam$marketCategory, live=sam$live, sampleNumber=sam$sampleNumber, totalWeight=sam$totalWeight), FUN=sum)
colnames(ss) = c('species', 'portComplex', 'year', 'qtr', 'gearGroup', 'marketCategory', 'live', 'sampleNumber', 'totalWeight', 'weight')
tt = aggregate(ss$weight, by=list(portComplex=ss$portComplex, year=ss$year, qtr=ss$qtr, gearGroup=ss$gearGroup, marketCategory=ss$marketCategory, live=ss$live, sampleNumber=ss$sampleNumber, totalWeight=ss$totalWeight), FUN=sum)
colnames(tt) = c('portComplex', 'year', 'qtr', 'gearGroup', 'marketCategory', 'live', 'sampleNumber', 'totalWeight', 'weight')

#merge in the landings and focus on WDOW
dd = merge(ss, tt, by=c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live', 'sampleNumber', 'totalWeight'), all.x=T)
colnames(dd) = c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live', 'sampleNumber', 'lands', 'species', 'sampleWeight', 'sampleTotal')
dd$lands = dd$lands/2204.62
#dd = merge(dd, ll, by=c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live'), all.x=T)
#colnames(dd) = c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live', 'species', 'sampleWeight', 'sampleTotal', 'lands')
#dd = dd[!is.na(dd$lands),] 
#dd[,'bins'] = round(log(dd$lands,10))
dd = dd[dd$species=='WDOW',]

#
#CORRELATION
#

#
cols = brewer.pal(9, 'Set1')

#
reg = c('north', 'center', 'south')
ports = list(
	north = c('CRS', 'ERK', 'BRG', 'BDG'),
	center = c('OSF', 'MNT', 'MRO'),
	south = c('OSB', 'OLA', 'OSD') 
)
#
oneDensity = list()

##
#i = 1
#at = -2
#png(sprintf('%d%dJoint3.png', minYear, maxYear))
#for(r in reg){
#	#Trawl group
#	DD = dd[dd$portComplex%in%ports[[r]] & dd$gearGroup=='TWL',]	
#	if( nrow(DD) ){
#		#
#		isOne = DD$sampleWeight/DD$sampleTotal==1
#		jd = cbind(log(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
#		oneDensity[[r]] = density(log(DD$lands)[isOne], bw=0.55)
#		#
#		d = Mclust(jd, G=1, modelNames='VVV')
#		#
#		corTest = cor.test(jd[,1], jd[,2])
#		#
#		if(i!=1){ par(new=TRUE) }
#		plot(d, what="classification", 
#        	        xlab='log(Landings)', 
#        	        ylab='logit(Species Composition)', 
#        	        col=cols[i], 
#        	        xlim=c(-5, 5), 
#        	        ylim=c(-5, 5)
#        	) 
#        	mtext(sprintf('%d-%d', minYear, maxYear), cex=1.5, font=2, line=1)
#		mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*i), at=at, cex=1.2, col=cols[i])
#		#
#		i = i+1
#	}
#	
#	#not Trawl group
#        DD = dd[dd$portComplex%in%ports[[r]] & dd$gearGroup!='TWL',]
#        if( nrow(DD) ){ 
#        	#
#        	isOne = DD$sampleWeight/DD$sampleTotal==1
#        	jd = cbind(log(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
#        	oneDensity[[r]] = density(log(DD$lands)[isOne], bw=0.55)
#        	#
#        	d = Mclust(jd, G=1, modelNames='VVV')
#        	#
#        	corTest = cor.test(jd[,1], jd[,2])#
#		#
#		if(i!=1){ par(new=TRUE) }
#		plot(d, what="classification",  
#        	        xlab='log(Landings)', 
#        	        ylab='logit(Species Composition)', 
#        	        col=cols[i], 
#        	        xlim=c(-5, 5), 
#        	        ylim=c(-5, 5)
#        	) 
#        	mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*i), at=at, cex=1.2, col=cols[i])
#		i = i+1
#	}
#}
#dev.off()

at = -2
png(sprintf('%d%dJoint3Other.png', minYear, maxYear))
#All other
DD = dd[ dd$gearGroup=='TWL',]
#
isOne = DD$sampleWeight/DD$sampleTotal==1
jd = cbind(log(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
oneDensity[[1]] = density(log(DD$lands)[isOne], bw=0.55)
#
d = Mclust(jd, G=1, modelNames='VVV')
#
corTest = cor.test(jd[,1], jd[,2])
#if(i!=1){ par(new=TRUE) }
plot(d, what="classification",
        xlab='log(Landings)',
        ylab='logit(Species Composition)',
        col=cols[1],
        xlim=c(-5, 5),
        ylim=c(-5, 5)
)
mtext(sprintf('%d-%d', minYear, maxYear), cex=1.5, font=2, line=1)
mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*1), at=at, cex=1.2, col=cols[1])

#North Not Trawl group
DD = dd[dd$gearGroup!='TWL',]
#
isOne = DD$sampleWeight/DD$sampleTotal==1
jd = cbind(log(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
oneDensity[[2]] = density(log(DD$lands)[isOne], bw=0.55)
#
d = Mclust(jd, G=1, modelNames='VVV')
#
corTest = cor.test(jd[,1], jd[,2])#
#if(i!=1){ 
par(new=TRUE) 
plot(d, what="classification",
        xlab='log(Landings)',
        ylab='logit(Species Composition)',
        col=cols[2],
        xlim=c(-5, 5),
        ylim=c(-5, 5)
)
mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*2), at=at, cex=1.2, col=cols[2])
#
dev.off()








#
#JUNK YARD
#

##
#corr = cor(dd$lands, dd$sampleWeight/dd$sampleTotal)
##
##reg0 = glm(dd$sampleWeight/dd$sampleTotal~dd$lands, family="binomial")#, weights=dd$sampleTotal)
#reg1 = glm(dd$sampleWeight/dd$sampleTotal~dd$lands, family="binomial", weights=dd$sampleTotal)
#reg2 = glm(cbind(dd$sampleWeight, dd$sampleTotal-dd$sampleWeight)~dd$lands, family="binomial")



#
#joint = mle.tmvnorm( jd, 
#	start = list(
#		mu = colMeans(jd),
#		sigma = cov(jd)
#	)
#)
#m1 = fitdist(jd[,1], dist='norm')
#m2 = fitdist(jd[,2], dist='norm')
