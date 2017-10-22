#
#FUNCTIONS
#

#
cleanZero = function( mcat, cores ){
	#mcat: a path to the relavent mcat 
	#cores: threads parallel

	#
	mcatNum = strsplit(strsplit(mcat, '/')[[1]][7], 'T')[[1]][2]
	load(sprintf('%sTop/Space274/Space274.RData', mcat))
	dataClean$weights = as.numeric(dataClean$weights)
	dataClean$clustSize = as.numeric(dataClean$clustSize)
	#
        sppEff = unique(dataClean$species)
        end = length(dataClean$sampleNumber)
	
        ##
	#uid = unique(dataClean$sampleNumber)
	#U = length(uid)
	#bounds = seq(1, U, ceiling(length(uid)/cores))	
	#luid = split(uid, bounds)
	#dataClean = mclapply( luid, FUN = function(vid){
	#	#
        #	addd = numeric(0)
	#	for(id in vid){
	#		#
        #		wid = which(dataClean$sampleNumber==id)
	#		#
        #		off  = dataClean$clustSize[wid[1]]
        #		port = dataClean$portComplex[wid[1]]
        #		gear = dataClean$gearGroup[wid[1]]
        #		year = dataClean$year[wid[1]]
        #		qtr  = dataClean$qtr[wid[1]]
	#		#       #       
        #		for(sn in sppEff[!sppEff%in%dataClean$species[wid]]){
        #		        #
        #		        end = end + 1
        #		        #
	#			add = matrix(NA, ncol=10, nrow=1)
        #                	colnames(add) = c('sampleNumber', 'species', 'year', 'qtr', 'portComplex', 'gearGroup', 'mcat', 'isLive', 'weights', 'clustSize')
        #                	add[,'sampleNumber'] = id
        #                	add[,'species'] = sn
        #                	add[,'year'] = year
        #                	add[,'qtr']  = qtr
        #                	add[,'portComplex'] = port
        #                	add[,'gearGroup'] = gear
        #                	add[,'mcat'] = mcatNum
        #                	add[,'isLive'] = 'N'
        #                	add[,'weights'] = 0
        #                	add[,'clustSize'] = off
        #		        #dataClean[end,] = c(id, sn, year, qtr, port, gear, mcatNum, 'N', 0, off)
        #			addd = rbind(addd, add)
	#		}
	#	}
	#	return(addd)
	#}, mc.cores=cores)
	#dataClean = as.data.frame(do.call(rbind, dataClean))
	
        #fill in zero data (see ../../source/tempMakeDesign.r)
        for(id in unique(dataClean$sampleNumber)){
               #
               wid = which(dataClean$sampleNumber==id)
               #
               off  = dataClean$clustSize[wid[1]]
               port = dataClean$portComplex[wid[1]]
               gear = dataClean$gearGroup[wid[1]]
               year = dataClean$year[wid[1]]
               qtr  = dataClean$qtr[wid[1]]
               #       
               for(sn in sppEff[!sppEff%in%dataClean$species[wid]]){
                       #
                       end = end + 1
                       #
                       dataClean[end,] = c(id, sn, year, qtr, port, gear, mcatNum, 'N', 0, off)        
               }
        }
	#

	return( dataClean )
}


#
predPerf = function(datMcatFill, prob, avgPath, threads, adj){
	#
	#end = 1
	#preds = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPredHDI=numeric(), propPredCI=numeric(), n=numeric())	
	#for(p in portGold){ #
	registerDoParallel(cores=threads)
	preds = foreach( p=portGold )%dopar%{
		#
		end  = 1
		pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPredHDI=numeric(), propPredCI=numeric(), n=numeric())
		#
		for(g in gearGold){
		for(q in qtrGold ){
		for(y in yearGold){
			#print(c(p, q, y))
			#
			#lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPred.csv', avgPath, p, g, q, y), stringsAsFactors=F)
			sp = read.csv(sprintf('%s/%s/%s/%s/%s/sppComp.csv', avgPath, p, g, q, y), stringsAsFactors=F)
			W = datMcatFill[datMcatFill$portComplex==p & datMcatFill$gearGroup==g & datMcatFill$qtr==q & datMcatFill$year==y,]
			W$weights = as.numeric(W$weights)
			W$clustSize = as.numeric(W$clustSize)
			#l = c(as.numeric(W$clustSizes), W$clustSizes)
			#print(W)
			#print(W$clustSize)
			#print(as.numeric(W$clustSize))
			#print(dim(W))
			#if( dim(W)[1]!=0 ){ print(l) }	
			#avoid the case where no data exists
			if( dim(W)[1]>0 ){
				#print(colnames(sp))
				#print(head(W))
				#, W$clusterNumber
				#clustSize = aggregate(W$weight, by=list(W$sampleNumber), FUN=sum)
				#colnames(clustSize) = c('sampleNumber', 'clusterNumber', 'size')
				#
				for(s in colnames(sp)){
					#
					##print(s)
	#				#spIntHDI = hdi(sp[,s], credMass=prob)  
					spIntHDI = hdi(density(sp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
               	               		#spIntHDI = HDInterval:::hdi.density(bkde(sp[,s], range.x=c(0,1), canonical=T), credMass=prob, allowSplit=T)
               	               		spIntCI = t(quantile(sp[,s], c((1-prob)/2, prob+(1-prob)/2)))
        #                       		##
        #                       		##lpIntHDI = HDInterval:::hdi.density(bkde(lp[,s], range.x=c(0,max(lp[,s])), canonical=T), credMass=prob, allowSplit=T)
        #                       		#lpIntHDI = hdi(density(lp[,s], from=0, to=max(lp[,s])), credMass=prob, allowSplit=T)
        #                       		#lpIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
	#				##
					WS = W[W$species==s,]
	#				#print(WS)
	#				#print(dim(WS))
	#				#print(dim(WS)[1]>0)
					if( dim(WS)[1]>0 ){
						#make comps
						cp = WS$weights/WS$clustSize
						##print(WS$weights)
						##print(WS$clustSize)
						##print(WS$weights/WS$clustSize)
						
		#				#
		#			        #cp = c()
		#			        #wp = c()
		#			        #for(sam in WS$sampleNumber){
		#			        #        for(clust in WS[WS$sampleNumber==sam, 'clusterNumber']){
		#			        #        	#cp = c(cp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight']/clustSize[clustSize$sampleNumber==sam & clustSize$clusterNumber==clust,'size'])
		#			        #        	#wp = c(wp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight'])
		#				#		cp = c(cp, WS[WS$sampleNumber==sam, 'weight']/WS[WS$sampleNumber==sam, 'clustSize'])
		#				#		print(WS[WS$sampleNumber==sam, 'weight']/WS[WS$sampleNumber==sam, 'clustSize'])
		#				#	}
		#			        #}
		#				#print(cp)
		#				#print(wp)
					        #spHDI
					        inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other intervals
					        for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
					        cpHdiMean = mean(inOut)
					        #
					        #spCI
					        inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other intervals
					        for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
					        cpCiMean = mean(inOut)
					        ##
		#			        ##lpHDI
		#			        #inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
		#			        #for(i in 1:dim(lpIntHDI)[1]){ inOut=findInterval(wp, lpIntHDI[i,])==1 | inOut }
		#			        #wpHdiMean = mean(inOut)
		#			        #wpHdiMean = 0
		#			        ##
		#			        ##lpCI
		#			        #inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
		#			        #for(i in 1:dim(lpIntCI)[1]){ inOut=findInterval(wp, lpIntCI[i,])==1 | inOut }
		#			        #wpCiMean = mean(inOut)
		#			        #
		#			        #, wpHdiMean, wpCiMean
					        pred[end,] = c(as.character(p), as.character(g), q, y, as.character(s), cpHdiMean, cpCiMean, length(cp))
					        end = end+1
					}
				}
			}
		}}}
		##
		#print(end)
		#print(pred)
		return(pred)
	}
	#reduce preds
	preds = do.call(rbind, preds)
	#
	preds$propPredCI = as.numeric(preds$propPredCI)
	preds$propPredHDI = as.numeric(preds$propPredHDI)
	preds$n = as.numeric(preds$n)
	#
	propAccCI = sum(preds$propPredCI*preds$n)/sum(preds$n)
	propAccHDI = sum(preds$propPredHDI*preds$n)/sum(preds$n)
	#
	out = list(preds=preds, ciAvgAcc=propAccCI, hdiAvgAcc=propAccHDI)
	return(out)
}

###fillZero test
##mcatNum = as.integer(strsplit(nameSplit[7], 'T')[[1]][2])
##datMcat = dat[dat$marketCategory==mcatNum,]
#datMcatFill = cleanZero( mcat )
##
#avgPath = sprintf('%sTop/avgModel/', mcat)
#preds = predPerf(datMcatFill, 0.99, avgPath, 4)
##reduce preds
#preds = do.call(rbind, preds)
###preds$weightPredCI  = as.numeric(preds$weightPredCI)
###preds$weightPredHDI = as.numeric(preds$weightPredHDI)
#preds$propPredCI    = as.numeric(preds$propPredCI)
#preds$propPredHDI   = as.numeric(preds$propPredHDI)
#preds$n = as.numeric(preds$n)
##average prediction accuracy
###weightAccCI = sum(preds$weightPredCI*preds$n)/sum(preds$n)
###weightAccHDI = sum(preds$weightPredHDI*preds$n)/sum(preds$n)
#propAccCI   = sum(preds$propPredCI*preds$n)/sum(preds$n)
#propAccHDI  = sum(preds$propPredHDI*preds$n)/sum(preds$n)






#f
##prediction (see ../../soon(avgPath, threads){ict.r)
#avgPath = sprintf('%sTop/avgModel/', mcatPlace)
#registerDoParallel(cores=length(portGold))#4)#
#for(p in portGold){ #
##preds = foreach( p=portGold )%dopar%{
#       #
#       end  = 1
#       #, weightPredHDI=numeric(), weightPredCI=numeric(),
#       pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPredHDI=numeric(), propPredCI=numeric(), n=numeric(), stringsAsFactors=F)
#       #
#       for(g in gearGold){#[1]){
#       for(q in qtrGold ){#[1] ){
#       for(y in yearGold){#[1]){
#               #
#               #lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPred.csv', avgPath, p, g, q, y))
#               sp = read.csv(sprintf('%s/%s/%s/%s/%s/sppComp.csv', avgPath, p, g, q, y))
#               W = datMcat[datMcat$portComplex==p & datMcat$gearGroup==g & datMcat$qtr==q & datMcat$year==y,]
#               #avoid the case where no data exists
#               if( dim(W)[1]>0 ){
#                       #
#                       clustSize = aggregate(W$weight, by=list(W$sampleNumber, W$clusterNumber), FUN=sum)
#                       colnames(clustSize) = c('sampleNumber', 'clusterNumber', 'size')
#                       #
#                       for(s in colnames(sp)){
#                               print(s)
#                               #spIntHDI = hdi(sp[,s], credMass=prob)  
#                               spIntHDI = hdi(density(sp[,s], from=0, to=1), credMass=prob, allowSplit=T)
#                               #spIntHDI = HDInterval:::hdi.density(bkde(sp[,s], range.x=c(0,1), canonical=T), credMass=prob, allowSplit=T)
#                               spIntCI = t(quantile(sp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#                               ##
#                               ##lpIntHDI = HDInterval:::hdi.density(bkde(lp[,s], range.x=c(0,max(lp[,s])), canonical=T), credMass=prob, allowSplit=T)
#                               #lpIntHDI = hdi(density(lp[,s], from=0, to=max(lp[,s])), credMass=prob, allowSplit=T)
#                               #lpIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#                               #
#                               WS = W[W$species==s,]
#                               #print( gridSize )
#                               if( dim(WS)[1]>0 ){
#                                       #make comps
#                                       cp = c()
#                                       wp = c()
#                                       for(sam in WS$sampleNumber){
#                                               for(clust in WS[WS$sampleNumber==sam, 'clusterNumber']){
#                                                               cp = c(cp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight']/clustSize[clustSize$sampleNumber==sam & clustSize$clusterNumber==clust,'size'])
#                                                               wp = c(wp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight'])
#                                                       }
#                                       }
#                                       #spHDI
#                                       inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other intervals
#                                       for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
#                                       cpHdiMean = mean(inOut)
#                                       #
#                                       #spCI
#                                       inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other intervals
#                                       for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
#                                       cpCiMean = mean(inOut)
#                                       ##
#                                        ##lpHDI
#                                        #inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
#                                        #for(i in 1:dim(lpIntHDI)[1]){ inOut=findInterval(wp, lpIntHDI[i,])==1 | inOut }
#                                        #wpHdiMean = mean(inOut)
#                                        #wpHdiMean = 0
#                                        ##
#                                        ##lpCI
#                                        #inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
#                                        #for(i in 1:dim(lpIntCI)[1]){ inOut=findInterval(wp, lpIntCI[i,])==1 | inOut }
#                                        #wpCiMean = mean(inOut)
#                                       #
#                                        #, wpHdiMean, wpCiMean
#                                        pred[end,] = c(p, g, q, y, s, cpHdiMean, cpCiMean, length(cp))
#                                        end = end+1
#                                }
#                        }
#                }
#        }}}
#       ##
#        #return(pred)
#} 



