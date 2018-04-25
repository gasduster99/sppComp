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
predTune = function(fillD, portGold, gearGold, yearGold, qtrGold, prob, avgPath,
	levels=c(0.5, 0.68), #c(0.2, 0.50, 0.68, 0.9, 0.95), 
	saveFile='tuned.log', #'tuned.hand',
	startFile='tuned.hand'
	){
	#
	writeLines('\tTuning...\n')	
	
	#
	out = data.frame(
		mcat = unique(fillD$mcat),
		minYear = min(yearGold),
		maxYear = max(yearGold),
		adj = 4/12
	)

	#
	if( file.exists(saveFile) ){
		#
		log = read.csv(saveFile)
		log = log[
			out$mcat==log$mcat 		&
			out$minYear==log$minYear 	&
			out$maxYear==log$maxYear,
		]
		#
		if( out$minYear%in%log$minYear & out$maxYear%in%log$maxYear & out$mcat%in%log$mcat ){
			#
			#out$adj = log$adj
			return(log$adj)
		}
	}
	
	#
	lower = 0
	upper = 10
	if( file.exists(startFile) ){
		#
		log = read.csv(startFile)
		#
		adjs = log$adj[
                	out$mcat==log$mcat              |
                        out$minYear==log$minYear        |
                        out$maxYear==log$maxYear
                ]
		#lower = min(adjs)
		#upper = max(adjs)
		#
		log = log[
			out$mcat==log$mcat 		&
			out$minYear==log$minYear 	&
			out$maxYear==log$maxYear,
		]
		#
		if( out$minYear%in%log$minYear & out$maxYear%in%log$maxYear & out$mcat%in%log$mcat ){
			#
			out$adj = log$adj
			#return(log$adj)
		}
	}

	#
	f = function(adj){
		#
		registerDoParallel(cores=length(levels))
		ppSS = foreach( l=levels )%dopar%{
			pp = predPerf(fillD, portGold, gearGold, yearGold, qtrGold, prob, avgPath, threads=1, adjHard=adj)
			return( abs(sum(pp$coverage*pp$n)/sum(pp$n) - l) )
		}
		ppSS = do.call(rbind, ppSS)
		return( mean(ppSS) )
	}
	#
	#out$adj = optimize(f, c(0,1))$minimum #, tol=10^-5
	#out$adj = optim(out$adj, f, method="L-BFGS-B", lower=lower, upper=upper)$par
	#optOut = optim(4/12, f, method="L-BFGS-B", lower=0, upper=10)
	#print(optOut)
	#out$adj = optOut$par
	#if( optOut$convergence ){ optOut=optim(runif(1, 0, 1), f, method="L-BFGS-B", lower=0, upper=10) }
	gaThread = floor(48/length(levels)) #16
	gaOut = ga("real-valued", function(x){-f(x)}, 
		min = lower, 
		max = upper, 
		popSize = gaThread,
		run = 10,
		maxiter = 10,
		suggestions = out$adj,
		parallel = gaThread#, #16,
		#monitor = F	
	)	
	print(summary(gaOut))
	print(gaOut@solution)
	out$adj = gaOut@solution[1,]

	#
	if( file.exists(saveFile) ){ write.table(out, saveFile, sep=',', append=T, quote=F, row.names=F, col.names=F)
	} else{ write.table(out, saveFile, sep=',', append=T, quote=F, row.names=F) }

	return( out$adj )
}

#
predPerf = function(fillD, portGold, gearGold, yearGold, qtrGold, prob, avgPath, threads=10, adjHard=numeric(0)){
	#fillD		: the result of makeD(...)
	#portGold	: a list of gold standard ports
	#gearGold	: a list of gold standard gears
	#yearGold	: a list of gold standard years
	#qtrGold	: a list of gold standard qtrs
	#prob		: level of prediction in (0, 1)
	#threads	: threads for parallelzing
	#adjHard	: hard tune density estimation
	#
	#value		: a data.frame of predictive coverages	
	
	#adjHard allows the user to define adj for tuning purposes; else the auto tuning code runs
	if( length(adjHard)>0 ){ 
		adj = adjHard 
	} else{ 
		writeLines('Predictive Performance...')
		adj = predTune(fillD, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) 
	};

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
			W$nBB = as.numeric(W$nBB)
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
						cp = WS$weight/WS$nBB
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
						pred$n[end]	  = length(cp)
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
	colnames(preds) = c('port', 'gear', 'qtr', 'year', 'species', 'n', 'landing', 'coverage')
	#
	#preds$coverage = as.numeric(preds$coverage)
	#preds$n = as.numeric(preds$n)
	#
	return(preds)	
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
	#writeLines('aggPerf...\n')
	
	#
	by = preds[which(colnames(preds)%in%byNames)]	
	#aggregate landings
	sppAgg = aggregate(preds$landing, by=by, FUN=sum)
	if( 'species' %in% byNames ){ 
		#
		if( length(byNames)==1 ){ 
			sppAgg$x = sum(sppAgg$x)
		}else{
			sppTotal = aggregate(sppAgg$x, by=sppAgg[-which(colnames(sppAgg)%in%c('species', 'x'))], FUN=sum)	
			sppAgg = merge( sppAgg[-which(colnames(sppAgg)%in%c('x'))], sppTotal, by=byNames[-which(byNames%in%'species')], all.x=T)
		}
	}
	#aggregate coverage
	nCover = aggregate(preds$coverage*preds$n, by=by, FUN=sum)
	#aggregate n (counting spp)
	nAgg   = aggregate(preds$n, by=by, FUN=sum)	
	#
	c = dim(nCover)[2]
	nCover[,c] = nCover[,c]/nAgg[,c]
	#if spp not in by discount spp from the final report of n
	if( !('species' %in% byNames) ){	
		nAgg$x = nAgg$x/length(unique(preds$species))
	}
	#
	out = merge(nAgg, sppAgg, by=colnames(sppAgg)[-c])#, nCover, by=colnames(nCover)[-c])	
	out = merge(out, nCover, by=colnames(nCover)[-c])	
	colnames(out) = c(colnames(out)[1:(dim(out)[2]-3)], 'n', 'landing', 'coverage')	
	#
	return( out )
}

#
plotPerf = function(preds, level, 
	llv=c(0.05),# 0.02), 
	col=c('red'),# 'blue') 
	save=F,
	saveString=''
	#lwd = c(2, 3, 2)
	){
	#preds	: the predictive performance data structure returned by predPerf
	#level	: a reference level comparing predictions	
	#
	#value: a series of page sized plots

	#
	scale = 20#18.5	
	#
	perPage = 20
	R = dim(preds)[1]
	c = dim(preds)[2]
	#
	cexs = preds$landing/mean(preds$landing)
	cols = rep('black', R)
	cols[abs(preds$coverage[]-level)>llv] = col
	#
	formString = sprintf('%%s/%%s-%1.2f-Diagnostic-%%%dd%%s.pdf', round(level, 2), nchar(as.character(ceiling(R/perPage))) )
	#print(formString)
	for( pg in 1:ceiling(R/perPage) ){
		#details to subset preds to a page
		rows = ((pg-1)*(perPage)+1):((pg)*(perPage))
		pp = preds[rows,]
		r = dim(pp)[1]
		#
		if( save ){ 
			path = paste(colnames(pp)[1:(c-3)], collapse='-')
			dir.create(path, showWarnings = FALSE)
			pdf( sprintf(formString, path, path, pg, saveString),
				width  = 8.5/(8.5+11)*scale,
				height = 11/(8.5+11)*scale	
			)
		} else{	dev.new(width=8.5/(8.5+11)*scale, height=11/(8.5+11)*scale) }
		#
		par(mar=c(5.1,4.2*(c-2),4.1,2.1))
		plot(pp$coverage, r:1,
			pch  = 19,
			cex  = cexs[rows],
			col  = cols[rows],
			xlim = c(0, 1), #c(max(0, min(level+llv-0.01, pp$coverage)), min(1, max(level+llv+0.01, pp$coverage))), 
			yaxt = 'n', 
			ann  = F, 
			axes = F
		)
		#
		if( any(abs(level-c(0, 0.5, 1))<0.15) ){
			axis(side=1, 
				at = round(c(
					0, #quantile(pp$coverage, 0.01), 
					0.5,
					#max(0, min(1, level)), 
					1 #quantile(pp$coverage, 0.99)
				), 2)
			)
		}else{
			axis(side=1, 
				at = round(c(
					0, #quantile(pp$coverage, 0.01), 
					0.5,
					max(0, min(1, level)), 
					1 #quantile(pp$coverage, 0.99)
				), 2)
			)
		}
		#
		for(i in 1:r){ 	segments(level, i, rev(pp$coverage)[i], i, col=rev(cols[rows])[i]) }
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
				labels = colnames(pp)[i], 
				srt = 0, 
				pos = 2, 
				xpd = TRUE
			)
			#column entries
			text( y = r:1, 
				0.05 - ((c-2)^2*0.05) + ((i-1)*0.05*(c-2)), 
				labels = pp[,i], 
				srt = 0, 
				pos = 2, 
				xpd = TRUE
			)
		}
		#
		if( save ){ dev.off() }
	}
	#
	if( save ){
		# 
		system(sprintf('pdftk %s/%s-%1.2f-Diagnostic-*%s.pdf output %s/%s-%1.2f-Diagnostic%s.pdf', path, path, round(level,2), saveString, path, path, round(level,2), saveString)) 
		#system(sprintf('convert %s/%s-Diagnostic.pdf %s/%s-Diagnostic.png', path, path, path, path))
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

#
	#sppUnique = preds
	#colnames(sppUnique)[colnames(sppUnique)=='landing'] = 'x'	
	##first unique across species if it is beting summed over
	#if( !('species'%in%byNames) ){ sppUnique=aggregate(preds$landing, by=preds[-which(colnames(preds)=='species')], FUN=unique) }	
	##next sum across what ever is left
	#sppAgg = aggregate(sppUnique$x, by=sppUnique[which(colnames(sppUnique)%in%byNames)], FUN=sum)





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


