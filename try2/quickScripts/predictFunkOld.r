#
#DEPENDENCIES
#

#
suppressMessages(library(HDInterval, quietly=FALSE))
suppressMessages(library(doParallel, quietly=FALSE))
suppressMessages(library(foreach, quietly=FALSE))

#
#FUNCTIONS
#

#
predPerf = function(fillD, portGold, gearGold, yearGold, qtrGold, prob, avgPath, threads, adj=1){
	#fillD		: the result of makeD(...)
	#portGold	: a list of gold standard ports
	#gearGold	: a list of gold standard gears
	#yearGold	: a list of gold standard years
	#qtrGold	: a list of gold standard qtrs
	#prob		: level of prediction in (0, 1)
	#threads	: threads for parallelzing
	#adj		: tune the density estimation
	#
	#value		: a data.frame of predictive coverages
	writeLines('predPerf...\n')
	
	#
	registerDoParallel(cores=threads)
	preds = foreach( p=portGold )%dopar%{
		#
		end = 1
		pred = list() 
		#
		for(g in gearGold){
		for(q in qtrGold ){
		for(y in yearGold){
			#
			sp = read.csv(sprintf('%s/%s/%s/%s/%s/sppComp.csv', avgPath, p, g, q, y), stringsAsFactors=F)
			W = fillD[fillD$port==p & fillD$gear==g & fillD$qtr==q & fillD$year==y,]
			W$weight = as.numeric(W$weight)
			W$aggClustSize = as.numeric(W$aggClustSize)
			#avoid the case where no data exists
			if( dim(W)[1]>0 ){
				#
				for(s in colnames(sp)){
					# 
					spIntHDI = hdi(density(sp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
					#
					WS = W[W$species==s,]
					if( dim(WS)[1]>0 ){
						#make comps
						cp = WS$weight/WS$aggClustSize
						#cp = c()
					        #wp = c()
					        #for(sam in WS$sampleNumber){
					        #        for(clust in WS[WS$sampleNumber==sam, 'clusterNumber']){
					        #        	#cp = c(cp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight']/clustSize[clustSize$sampleNumber==sam & clustSize$clusterNumber==clust,'size'])
					        #        	#wp = c(wp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight'])
						#		cp = c(cp, WS[WS$sampleNumber==sam, 'weight']/WS[WS$sampleNumber==sam, 'clustSize'])
						#		print(WS[WS$sampleNumber==sam, 'weight']/WS[WS$sampleNumber==sam, 'clustSize'])
						#	}
					        #}
						#print(cp)
						#print(wp)
					        #spHDI
					        inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other intervals
					        for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
						cpHdiMean = mean(inOut)
						#
					        pred$port[end] 	  = as.character(p)
						pred$gear[end] 	  = as.character(g)
						pred$qtr[end] 	  = q
						pred$year[end]	  = y
						pred$species[end] = as.character(s)
						pred$iidn[end]	  = length(cp)
						pred$n[end]	  = length(unique(WS$id))
						pred$landing[end] = WS[1,'landing']
						pred$coverage[end]= cpHdiMean
						#
						end = end + 1
					}
				}
			}
		}}}
		#
		return( as.data.frame(pred) )
	}
	#reduce preds
	preds = do.call(rbind, preds)
	preds = as.data.frame(preds)
	colnames(preds) = c('port', 'gear', 'qtr', 'year', 'species', 'iidn', 'landing', 'coverage')
	#
	#preds$coverage = as.numeric(preds$coverage)
	#preds$n = as.numeric(preds$n)
	#
	return(preds)	
}

#
tuneDensity = function(){
	#
	
}

#
aggPerf = function(preds, byNames){ #by, byNames=names(by)){ #whichPredSpp=numeric(0), whichBySpp=numeric(0)){
	#preds		: the predictive performance data structure returned by predPerf
	#by		: a list as given in the aggregate function; if names are provided in the 'by' parameter the output will also contain column names
	#byNames	: the names of the by list
	#whichPredSpp 	: whcih column of 'preds' is species 
	#whichBySpp	: whcih column of 'by' is species
	#
	#value: an aggregated version of preds
	writeLines('aggPerf...\n')
	
	#
	by = preds[which(colnames(preds)%in%byNames)]
	#print(head(by))
	#
	sppAgg = aggregate(preds$landing, by=by, FUN=sum)
	#print(head(sppAgg))
	##aggregate landings
	#sppUnique = preds
	#colnames(sppUnique)[colnames(sppUnique)=='landing'] = 'x'	
	##first unique across species if it is beting summed over
	#if( !('species'%in%byNames) ){ sppUnique=aggregate(preds$landing, by=preds[-which(colnames(preds)=='species')], FUN=unique) }	
	##next sum across what ever is left
	#sppAgg = aggregate(sppUnique$x, by=sppUnique[which(colnames(sppUnique)%in%byNames)], FUN=sum)
	#aggregate coverage
	nCover = aggregate(preds$coverage*preds$iidn, by=by, FUN=sum)
	#aggregate n
	nAgg   = aggregate(preds$iidn, by=by, FUN=sum)
	#
	c = dim(nCover)[2]
	nCover[,c] = nCover[,c]/nAgg[,c]
	#
	out = merge(nAgg, sppAgg, by=colnames(sppAgg)[-c])#, nCover, by=colnames(nCover)[-c])	
	out = merge(out, nCover, by=colnames(nCover)[-c])	
	colnames(out) = c(colnames(out)[1:(dim(out)[2]-3)], 'iidn', 'landing', 'coverage')	
	#
	return( out )
}

#
plotPerf = function(preds, level, 
	llv=c(0.05),# 0.02), 
	col = c('red')# 'blue') 
	#lwd = c(2, 3, 2)
	){
	#preds	: the predictive performance data structure returned by predPerf
	#level	: a reference level comparing predictions	
	#
	#value: a series of page sized plots
	writeLines('plotPerf...\n')
	
	#
	r = dim(preds)[1]
	c = dim(preds)[2]
	#
	cexs = preds$landing/mean(preds$landing)
	cols = rep('black', r)
	cols[abs(preds$coverage[]-level)>llv] = col
	#
	par(mar=c(5.1,4*(c-2),4.1,2.1))
	plot(preds$coverage, r:1,
		pch  = 19,
		cex  = cexs,
		col  = cols,
		xlim = c(0, 1), #c(max(0, min(level+llv-0.01, preds$coverage)), min(1, max(level+llv+0.01, preds$coverage))), 
		yaxt = 'n', 
		ann  = F, 
		axes = F
	)
	#
	if( any(abs(level-c(0, 0.5, 1))<0.15) ){
		axis(side=1, 
			at = round(c(
				0, #quantile(preds$coverage, 0.01), 
				0.5,
				#max(0, min(1, level)), 
				1 #quantile(preds$coverage, 0.99)
			), 2)
		)
	}else{
		axis(side=1, 
			at = round(c(
				0, #quantile(preds$coverage, 0.01), 
				0.5,
				max(0, min(1, level)), 
				1 #quantile(preds$coverage, 0.99)
			), 2)
		)
	}
	#
	for(i in 1:r){ 	segments(level, i, rev(preds$coverage)[i], i, col=rev(cols)[i]) }
	#
	abline( v  = level, 
               col = 'black',
               lwd = 2
        )
	## 
	#abline( v = sapply(llv+level, FUN=function(x){min(max(x, 0), 1)}), 
	#	col = col,
	#	lwd = lwd
	#)
	##
	for(i in 1:(c-2)){
		#column header
		text( y = 0,
			x = 0.05 - ((c-2)^2*0.05) + ((i-1)*0.05*(c-2)),
			labels = colnames(preds)[i], 
			srt = 0, 
			pos = 2, 
			xpd = TRUE
		)
		#column entries
		text( y = r:1, 
			0.05 - ((c-2)^2*0.05) + ((i-1)*0.05*(c-2)), 
			labels = preds[,i], 
			srt = 0, 
			pos = 2, 
			xpd = TRUE
		)
	}
}


##
#postOpt = function(adj, mcat){
#        #	
#	writeLines(sprintf('%s:', adj))
#	adj = max(0.001, adj)
#	#
#        probs = c(0.68, 0.95, 0.99)
#        registerDoParallel(cores=threads)
#        #
#        sqErrs = c()
#        rates = c()
#        for(prob in probs){
#                preds = foreach( p=ports )%dopar%{
#                #for(p in ports){
#                        #
#                        end  = 1
#                        pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
#                        #
#                        for(g in gears){
#                        for(q in qtrs ){
#                        for(y in years){
#                                #
#                                dp = nowComps[nowComps[,'mcat']==mcat & nowComps[,'year']==y & nowComps[,'qtr']==q & nowComps[,'gear']==g & nowComps[,'port']==p,]
#                                donSpp = dp[,'species'] #<<<< I WAS HERE
#                                dp = as.numeric(dp[,'comp'])
#                                names(dp) = donSpp
#                                #
#                                lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y))
#                                lp = lp[!is.na(lp[,1]),]
#                                #boxplot(lp, ylim=c(0, 0.3))
#                                for(s in donSpp[donSpp%in%colnames(lp)]){ #NOTE: donSpp){ some species that are not in my numbers
#                                        #
#                                        spIntHDI = hdi(density(lp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
#                                        spIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#                                        #
#                                        #spHDI
#                                        inOut = 0
#                                        for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(dp[s], spIntHDI[i,], rightmost.closed=T)==1 | inOut }
#                                        #inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other interva
#                                        #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
#                                        cpHdiMean = mean(inOut)
#                                        #
#                                        #spCI
#                                        inOut = 0
#                                        for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(dp[s], spIntCI[i,], rightmost.closed=T)==1 | inOut }
#                                        #inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other interval
#                                        #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
#                                        cpCiMean = mean(inOut)
#                                        #
#                                        pred[end,] = c(as.character(p), as.character(g), q, y, as.character(s), cpHdiMean, cpCiMean)
#                                        end = end+1
#                                }
#                        }}}
#                        return(pred)
#                        #preds = rbind(preds, pred)
#                }
#                #reduce preds
#                preds = do.call(rbind, preds)
#                preds$propPostHDI = as.numeric(preds$propPostHDI)
#                preds$propPostCI = as.numeric(preds$propPostCI)
#                #
#                sqErrs = c(sqErrs, (prob-mean(preds$propPostHDI))^2)
#                rates = c(rates, mean(preds$propPostHDI))
#        }
#        # 
#        writeLines(sprintf('c(%s, %s, %s)', rates[1], rates[2], rates[3]))
#        writeLines('')
#        #
#        return( mean(c(sqErrs, sqErrs[1])))  #)))#
#}












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




#
#JUNK
#

##
#cleanZeroTry = function( mcat, cores ){
#	#mcat: a path to the relavent mcat 
#	#cores: threads parallel
#
#	#
#	mcatNum = strsplit(strsplit(mcat, '/')[[1]][7], 'T')[[1]][2]
#	load(sprintf('%sTop/Space274/Space274.RData', mcat))
#	dataClean$weights = as.numeric(dataClean$weights)
#	dataClean$clustSize = as.numeric(dataClean$clustSize)
#	#
#        sppEff = unique(dataClean$species)
#        end = length(dataClean$sampleNumber)
#		
#        #fill in zero data (see ../../source/tempMakeDesign.r)
#        for(id in unique(dataClean$sampleNumber)){
#               #
#               wid = which(dataClean$sampleNumber==id)
#               #
#               off  = dataClean$clustSize[wid[1]]
#               port = dataClean$portComplex[wid[1]]
#               gear = dataClean$gearGroup[wid[1]]
#               year = dataClean$year[wid[1]]
#               qtr  = dataClean$qtr[wid[1]]
#               #       
#               for(sn in sppEff[!sppEff%in%dataClean$species[wid]]){
#                       #
#                       end = end + 1
#                       #
#                       dataClean[end,] = c(id, sn, year, qtr, port, gear, mcatNum, 'N', 0, off)        
#               }
#        }
#	#
#
#	return( dataClean )
#}
#
##
#cleanZero = function( mcat, cores ){
#	#mcat: a path to the relavent mcat 
#	#cores: threads parallel
#
#	#
#	mcatNum = strsplit(strsplit(mcat, '/')[[1]][7], 'T')[[1]][2]
#	load(sprintf('%sTop/Space274/Space274.RData', mcat))
#	dataClean$weights = as.numeric(dataClean$weights)
#	dataClean$clustSize = as.numeric(dataClean$clustSize)
#	#
#        sppEff = unique(dataClean$species)
#        end = length(dataClean$sampleNumber)
#	
#        ##
#	#uid = unique(dataClean$sampleNumber)
#	#U = length(uid)
#	#bounds = seq(1, U, ceiling(length(uid)/cores))	
#	#luid = split(uid, bounds)
#	#dataClean = mclapply( luid, FUN = function(vid){
#	#	#
#        #	addd = numeric(0)
#	#	for(id in vid){
#	#		#
#        #		wid = which(dataClean$sampleNumber==id)
#	#		#
#        #		off  = dataClean$clustSize[wid[1]]
#        #		port = dataClean$portComplex[wid[1]]
#        #		gear = dataClean$gearGroup[wid[1]]
#        #		year = dataClean$year[wid[1]]
#        #		qtr  = dataClean$qtr[wid[1]]
#	#		#       #       
#        #		for(sn in sppEff[!sppEff%in%dataClean$species[wid]]){
#        #		        #
#        #		        end = end + 1
#        #		        #
#	#			add = matrix(NA, ncol=10, nrow=1)
#        #                	colnames(add) = c('sampleNumber', 'species', 'year', 'qtr', 'portComplex', 'gearGroup', 'mcat', 'isLive', 'weights', 'clustSize')
#        #                	add[,'sampleNumber'] = id
#        #                	add[,'species'] = sn
#        #                	add[,'year'] = year
#        #                	add[,'qtr']  = qtr
#        #                	add[,'portComplex'] = port
#        #                	add[,'gearGroup'] = gear
#        #                	add[,'mcat'] = mcatNum
#        #                	add[,'isLive'] = 'N'
#        #                	add[,'weights'] = 0
#        #                	add[,'clustSize'] = off
#        #		        #dataClean[end,] = c(id, sn, year, qtr, port, gear, mcatNum, 'N', 0, off)
#        #			addd = rbind(addd, add)
#	#		}
#	#	}
#	#	return(addd)
#	#}, mc.cores=cores)
#	#dataClean = as.data.frame(do.call(rbind, dataClean))
#	
#        #fill in zero data (see ../../source/tempMakeDesign.r)
#        for(id in unique(dataClean$sampleNumber)){
#               #
#               wid = which(dataClean$sampleNumber==id)
#               #
#               off  = dataClean$clustSize[wid[1]]
#               port = dataClean$portComplex[wid[1]]
#               gear = dataClean$gearGroup[wid[1]]
#               year = dataClean$year[wid[1]]
#               qtr  = dataClean$qtr[wid[1]]
#               #       
#               for(sn in sppEff[!sppEff%in%dataClean$species[wid]]){
#                       #
#                       end = end + 1
#                       #
#                       dataClean[end,] = c(id, sn, year, qtr, port, gear, mcatNum, 'N', 0, off)        
#               }
#        }
#	#
#
#	return( dataClean )
#}


