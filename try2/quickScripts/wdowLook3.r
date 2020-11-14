rm(list=ls())

#
library(boot)
library(mclust)
library(tmvtnorm)
library(latex2exp)
library(HDInterval)
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
#dd[,'bins'] = round(log10(dd$lands,10))
dd = dd[dd$species=='WDOW',]
dd[dd$gearGroup=='MDT','gearGroup'] = 'TWL'

#
#CORRELATION
#

#
cols = brewer.pal(9, 'Set1')

#
reg = c('North  ', 'Center', 'South  ')
ports = list()
ports[["North  "]] = c('CRS', 'ERK', 'BRG', 'BDG')
ports[["Center"]]  = c('OSF', 'MNT', 'MRO')
ports[["South  "]]  = c('OSB', 'OLA', 'OSD') 
#
oneDensity = list()
margDensity = list()

#
#EJs IDEA
#

#
i = 1
at = -1
leg = c()
png(sprintf('%d%dJoint3.png', minYear, maxYear))
for(r in reg){
	#Trawl group
	DD = dd[dd$portComplex%in%ports[[r]] & dd$gearGroup=='TWL',]	
	if( nrow(DD) ){
		#
		isOne = DD$sampleWeight/DD$sampleTotal==1
		jd = cbind(log10(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
		if( any(isOne) ){ oneDensity[[i]] = density(log10(DD$lands)[isOne], bw=0.55) }
		margDensity[[i]] = density(log10(DD$lands), bw=0.55)
		#
		d = Mclust(jd, G=1, modelNames='VVV')
		#
		corTest = cor.test(jd[,1], jd[,2])
		#
		if(i!=1){ par(new=TRUE) }
		plot(d, what="classification", 
        	        xlab='log(Landings)', 
        	        ylab='logit(Species Composition)', 
        	        col=cols[i], 
        	        xlim=c(-2.5, 2.5), 
        	        ylim=c(-5, 5),
		        cex=-1,
			fillEllipse=T
        	)
        	mtext(sprintf('%d-%d', minYear, maxYear), cex=1.5, font=2, line=1)
		mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*i), at=at, cex=1.2, col=cols[i])
		#
		leg = c(leg, sprintf("%s TWL", r))
		#
		i = i+1
	}
	
	#not Trawl group
        DD = dd[dd$portComplex%in%ports[[r]] & dd$gearGroup!='TWL',]
        if( nrow(DD) ){ 
        	#
        	isOne = DD$sampleWeight/DD$sampleTotal==1
        	jd = cbind(log10(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
        	if( any(isOne) ){ oneDensity[[i]] = density(log10(DD$lands)[isOne], bw=0.55) }
		margDensity[[i]] = density(log10(DD$lands), bw=0.55)
        	#
        	d = Mclust(jd, G=1, modelNames='VVV')
        	#
        	corTest = cor.test(jd[,1], jd[,2])#
		#
		if(i!=1){ par(new=TRUE) }
		plot(d, what="classification",  
        	        xlab='log(Landings)', 
        	        ylab='logit(Species Composition)', 
        	        col=cols[i], 
        	        xlim=c(-2.5, 2.5), 
        	        ylim=c(-5, 5),
		        cex=-1,
			fillEllipse=T
        	) 
		leg = c(leg, sprintf("%s OTH", r))
        	mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*i), at=at, cex=1.2, col=cols[i])
		i = i+1
	}
}
legend('topright', legend=leg, fill=cols[1:length(leg)])
dev.off()

#
png(sprintf('%s%sAllDensity.png', minYear, maxYear))
plot(oneDensity[[1]],
     col=cols[1],
     lwd=3,
     main='p( log(Landings) | Species Composition=1 )',
     xlab='log(Landings)',
     xlim=c(-2.5, 2.5),
     ylim=c(0, max(sapply(oneDensity, function(x){x$y})))
)
for(i in 2:length(oneDensity)){
	if(!is.null(oneDensity[[i]])){
		lines(oneDensity[[i]],
			col=cols[i],
		        lwd=3,
		        xlim=c(-2.5, 2.5)
		)
	}
}
dev.off()

#
#OTHER IDEA
#

#
at = -1
png(sprintf('%d%dJoint3Other.png', minYear, maxYear))
#All other
DD = dd[ dd$gearGroup=='TWL',]
#
isOne = DD$sampleWeight/DD$sampleTotal==1
jd = cbind(log10(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
if( any(isOne) ){ oneDensity[[1]] = density(log10(DD$lands)[isOne], bw=0.55) }
margDensity[[1]] = density(log10(DD$lands), bw=0.55)
#
d = Mclust(jd, G=1, modelNames='VVV')
#
corTest = cor.test(jd[,1], jd[,2])
#if(i!=1){ par(new=TRUE) }
plot(d, what="classification",
        xlab='log(Landings)',
        ylab='logit(Species Composition)',
        col=cols[1],
        xlim=c(-2.5, 2.5),
        ylim=c(-5, 5),
	cex=-1,
	fillEllipse=T
)
legend('topright', legend=c('TWL', 'OTH'), fill=cols[1:2])
mtext(sprintf('%d-%d', minYear, maxYear), cex=1.5, font=2, line=1)
mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*1), at=at, cex=1.2, col=cols[1])

#North Not Trawl group
DD = dd[dd$gearGroup!='TWL',]
#
isOne = DD$sampleWeight/DD$sampleTotal==1
jd = cbind(log10(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
if( any(isOne) ){ oneDensity[[2]] = density(log10(DD$lands)[isOne], bw=0.55) }
margDensity[[2]] = density(log10(DD$lands), bw=0.55)
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
        xlim=c(-2.5, 2.5),
        ylim=c(-5, 5),
	cex=-1,
	fillEllipse=T
)
mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*2), at=at, cex=1.2, col=cols[2])
#
dev.off()

#
png(sprintf('%s%sOneDensity.png', minYear, maxYear))
plot(oneDensity[[1]], 
     col=cols[1], 
     lwd=3, 
     main='p( log(Landings) | Species Composition=1 )', 
     xlab='log(Landings)',
     xlim=c(-2.5, 2.5),
     ylim=c(0, max(c(oneDensity[[1]]$y, oneDensity[[2]]$y)))
)
if( any(isOne) ){
	lines(oneDensity[[2]],
     		col=cols[2],
     		lwd=3,
		xlim=c(-2.5, 2.5)
	)
}
dev.off()

#
#MCLUST IDEA
#

#
isOne = dd$sampleWeight/dd$sampleTotal==1
jd = cbind(log10(dd$lands), logit(dd$sampleWeight/dd$sampleTotal))[!isOne,]
mod = Mclust(jd, G=1:10, modelNames='VVV')
png(sprintf("%s%sJointFit.png", minYear, maxYear))
plot(mod, what="classification",
        xlab='log(Landings)',
        ylab='logit(Species Composition)',
        col=cols,
        xlim=c(-2.5, 2.5),
        ylim=c(-5, 5),
        cex=-1,
        fillEllipse=T
)
#
tabs = list()
mtext(sprintf('%d-%d Mixture Fit', minYear, maxYear), cex=1.5, font=2, line=1)
for(i in 1:ncol(mod$parameters$mean)){
	who = mod$classification==i
	corTest = cor.test(jd[who,1], jd[who,2])
	mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*i), at=at, cex=1.2, col=cols[i])
	#contingency table , 'marketCategory'
	tabs[[i]] = table(dd[who,c('portComplex', 'gearGroup')])
	tabs[[i]] = tabs[[i]][match(portGold, rownames(tabs[[i]])),]
}
dev.off()
#

#
#LANDING BINS
#

#
oneCI = list()

#
fudge = -0
levels = c('Low Landings', 'Medium Landings', 'Hi Landings')
thresh = quantile(dd$lands, probs=c(2/5, 4/5))
#thresh = quantile(dd$lands, probs=c(1/5, 2/5, 3/5, 4/5))
#thresh = quantile(dd$lands, probs=c(1/4, 2/4, 3/4))
#thresh = quantile(dd$lands, probs=c(1/3, 2/3))
#thresh = quantile(dd$lands, probs=c(0.2, 1-0.2))
nameStr = paste(round(thresh), collapse='-')
thresh = c(0.1, thresh, max(dd$lands))
#thresh = c(0.1, 2.821348, 16.496721, max(dd$lands))
png(sprintf('%s%sJointLand%s.png', minYear, maxYear, nameStr))
for(i in 1:(length(thresh)-1)){
	#
	#DD = dd[log10(dd$lands)<thresh[i+1] & log10(dd$lands)>=thresh[i],]
	DD = dd[dd$lands<thresh[i+1] & dd$lands>=thresh[i],]
	#	
        isOne = DD$sampleWeight/DD$sampleTotal==1
        jd = cbind(log10(DD$lands), logit(DD$sampleWeight/DD$sampleTotal))[!isOne,]
        if( any(isOne) ){ 
		#
		oneDensity[[i]] = density(log10(DD$lands)[isOne], from=log10(thresh[i])*(1-fudge), to=log10(thresh[i+1])*(1+fudge))#, bw=0.25)
		if( oneDensity[[i]]$bw<0.1 ){ oneDensity[[i]] = density(log10(DD$lands)[isOne], from=log10(thresh[i])*(1-fudge), to=log10(thresh[i+1])*(1+fudge), bw=0.25) }
		#
		me = sd(log10(DD$lands[isOne])) 
		center = mean(log10(DD$lands[isOne]))
		#chol(d$parameters$variance$Sigma)[1,1]
		#oneSD = pnorm(1.25)-pnorm(-1.25)
		#oneCI[[i]] = hdi(oneDensity[[i]], credMass=oneSD) #quantile( log10(DD$lands)[isOne], c((1-oneSD)/2, oneSD+(1-oneSD)/2) )
	}
	margDensity[[i]] = density(log10(DD$lands), bw=0.25)
        #
        d = Mclust(jd, G=1, modelNames='VVV')
        #
        corTest = cor.test(jd[,1], jd[,2])
        #
        if(i!=1){ par(new=TRUE) }
        plot(d, what="classification",
                xlab='log(Landings)',
                ylab='logit(Species Composition)',
                col=cols[i],
                xlim=c(-2.5, 2.5),
                ylim=c(-5, 5),
                cex=-1,
                fillEllipse=T
        )
	top = chol(d$parameters$variance$Sigma)[2,2]+d$parameters$mean[2]
	#me = sd(log10(DD$lands)) #chol(d$parameters$variance$Sigma)[1,1]
	#center = mean(log10(DD$lands))	
	hm = length(oneDensity[[i]]$y)
	inInt = 10^(oneDensity[[i]]$x)<thresh[i+1] & 10^(oneDensity[[i]]$x)>=thresh[i]
	#
	#segments(oneCI[[i]][1], top, oneCI[[i]][2], top, col=cols[i], lwd=3)
	#segments(center-me, top, center+me, top, col=cols[i], lwd=3)	
	lines(oneDensity[[i]]$x[inInt], (top+oneDensity[[i]]$y/2)[inInt], col=cols[i], lwd=3)
	mtext(sprintf('%d-%d', minYear, maxYear), cex=1.5, font=2, line=1)
        mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-(2+2*i), at=at, cex=1.2, col=cols[i])
        #leg = c(leg, sprintf("%s", levels[i]))
}
dev.off()

#
png(sprintf('%s%sLandDensity%s.png', minYear, maxYear, nameStr))
#dev.new()
plot(oneDensity[[1]],
     col=cols[1],
     lwd=3,
     main='p( log(Landings) | Species Composition=1 )',
     xlab='log(Landings)',
     xlim=c(-2.5, 2.5),
     ylim=c(0, max(sapply(oneDensity, function(x){x$y})))
)
lines(margDensity[[1]],
        col=cols[1],
        lwd=3,
        xlim=c(-2.5, 2.5),
	lty=2
)
for(i in 2:length(oneDensity)){
        if(!is.null(oneDensity[[i]])){
                lines(oneDensity[[i]],
                        col=cols[i],
                        lwd=3,
                        xlim=c(-2.5, 2.5)
                )
		lines(margDensity[[i]],
                        col=cols[i],
                        lwd=3,
                        xlim=c(-2.5, 2.5),
			lty=2
                )
               
        }                       
}                               
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
