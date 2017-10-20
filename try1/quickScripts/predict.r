rm(list=ls())

#This script assumes the .../avgModel/ directory has been successfully built. 

#
library(coda)
library(foreach)
library(HDInterval)
library(doParallel)
library(KernSmooth)

#
#FUNCTIONS
#

#

#
#MAIN
#

#
prob = 0.99
mcat = 250
dataName = "data83To90.csv"
#these data do not include the implied zeros
dat = read.csv(dataName, stringsAsFactors=F)
dat = dat[dat$marketCategory==mcat,]

#65, 37, avgModel
avgPath = sprintf("./MCAT%d/Top/avgModel/", mcat) #"./MCAT%d/Top/Space37/", mcat)#sprintf("./MCAT%d/Top/Space12/", mcat) #
dynPath = avgPath
#
ports = list.dirs(dynPath, recursive=F)
ports = unlist(lapply(strsplit(ports, '//'), function(x){x[2]}))
dynPath  = sprintf("%s%s/", dynPath, ports[1])
# 
gears = list.dirs(dynPath, recursive=F); 
gears = unlist(lapply(strsplit(gears, '//'), function(x){x[2]}))
dynPath = sprintf("%s%s/", dynPath, gears[1])
#
qtrs = list.dirs(dynPath, recursive=F); 
qtrs = unlist(lapply(strsplit(qtrs, '//'), function(x){x[2]}))
dynPath = sprintf("%s%s/", dynPath, qtrs[1])
#
years = list.dirs(dynPath, recursive=F); 
years = unlist(lapply(strsplit(years, '//'), function(x){x[2]}))

##
#gridSize = 10^5
#end = 1
#preds = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), predHDI=numeric(), predCI=numeric(), stringsAsFactors=F)
#for(p in ports){#[6]){ 
registerDoParallel(cores=length(ports))
preds = foreach( p=ports )%dopar%{
	#
	end  = 1
	pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), weightPredHDI=numeric(), weightPredCI=numeric(), propPredHDI=numeric(), propPredCI=numeric(), n=numeric(), stringsAsFactors=F)
	#
	for(g in gears){#[1]){
	for(q in qtrs ){#[1] ){
	for(y in years){#[1]){
		#
		lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPred.csv', avgPath, p, g, q, y))
		sp = read.csv(sprintf('%s/%s/%s/%s/%s/sppComp.csv', avgPath, p, g, q, y))
		W = dat[dat$portComplex==p & dat$gearGroup==g & dat$qtr==q & dat$year==y,]
		#avoid the case where no data exists
		if( dim(W)[1]>0 ){
			#
			clustSize = aggregate(W$weight, by=list(W$sampleNumber, W$clusterNumber), FUN=sum)
			colnames(clustSize) = c('sampleNumber', 'clusterNumber', 'size')
			#
			for(s in colnames(sp)){
				#print(s)
				#spIntHDI = hdi(sp[,s], credMass=prob)	
				#spIntHDI = hdi(density(sp[,s], from=0, to=1), credMass=prob, allowSplit=T)
				spIntHDI = HDInterval:::hdi.density(bkde(sp[,s], range.x=c(0,1), canonical=T), credMass=prob, allowSplit=T)
				spIntCI = t(quantile(sp[,s], c((1-prob)/2, prob+(1-prob)/2)))
				#
				#lpIntHDI = HDInterval:::hdi.density(bkde(lp[,s], range.x=c(0,max(lp[,s])), canonical=T), credMass=prob, allowSplit=T)
				lpIntHDI = hdi(density(lp[,s], from=0, to=max(lp[,s])), credMass=prob, allowSplit=T)
				lpIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
				#
				WS = W[W$species==s,]
				#print( gridSize )
				if( dim(WS)[1]>0 ){
					#make comps
					cp = c()
					wp = c()
					for(sam in WS$sampleNumber){
						for(clust in WS[WS$sampleNumber==sam, 'clusterNumber']){	
								cp = c(cp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight']/clustSize[clustSize$sampleNumber==sam & clustSize$clusterNumber==clust,'size'])
								wp = c(wp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight'])
							}
					}
					#spHDI
					inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other intervals
					for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
					cpHdiMean = mean(inOut)
					#
					#spCI
                                        inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other intervals
                                        for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
                                        cpCiMean = mean(inOut)
					#
					#lpHDI
					inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
					for(i in 1:dim(lpIntHDI)[1]){ inOut=findInterval(wp, lpIntHDI[i,])==1 | inOut }
					wpHdiMean = mean(inOut)
					#wpHdiMean = 0
					##
					#lpCI
					inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
					for(i in 1:dim(lpIntCI)[1]){ inOut=findInterval(wp, lpIntCI[i,])==1 | inOut }
					wpCiMean = mean(inOut)
					#
					pred[end,] = c(p, g, q, y, s, wpHdiMean, wpCiMean, cpHdiMean, cpCiMean, length(cp))
					end = end+1
				}
			}
		}
	}}}
	return(pred)
}
#reduce preds
preds = do.call(rbind, preds)
preds$weightPredCI  = as.numeric(preds$weightPredCI) 
preds$weightPredHDI = as.numeric(preds$weightPredHDI)
preds$propPredCI    = as.numeric(preds$propPredCI) 
preds$propPredHDI   = as.numeric(preds$propPredHDI)
preds$n = as.numeric(preds$n)
#average prediction accuracy
weightAccCI = sum(preds$weightPredCI*preds$n)/sum(preds$n)
weightAccHDI = sum(preds$weightPredHDI*preds$n)/sum(preds$n)
propAccCI   = sum(preds$propPredCI*preds$n)/sum(preds$n)
propAccHDI  = sum(preds$propPredHDI*preds$n)/sum(preds$n)



