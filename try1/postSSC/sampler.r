#to be run from the model file
writeLines('\nRunning Sampler\n...')
suppressMessages(library(boot, quietly=TRUE))
#
S = length(sppGold)
#
hypeSamples = inla.hyperpar.sample(M, model) #out)
write.csv(format(hypeSamples, scientific=T, digits=22), file="./nuPost.csv", row.names=F, quote=F)
#

#"FUNCTION" HEADER
n = M
result = model #out
intern = FALSE
use.improved.mean = TRUE
add.names = TRUE
seed = 0L
#
stopifnot(!missing(result) && any(class(result) == "inla"))
if( is.null(result$misc$configs) ){
    stop("You need an inla-object computed with option 'control.compute=list(config = TRUE)'.")
}
#
n = as.integer(n)
stopifnot(is.integer(n) && n > 0L)
cs = result$misc$configs
ld = numeric(cs$nconfig)
for( i in 1:cs$nconfig ){
    ld[i] = cs$config[[i]]$log.posterior
}
#
p = exp(ld - max(ld))
idx = sample(1:cs$nconfig, n, prob = p, replace = TRUE)
idx = sort(idx)
n.idx = numeric(cs$nconfig)
n.idx[] = 0
for( i in 1:cs$nconfig ){
    n.idx[i] = sum(idx == i)
}
#
for( k in 1:cs$nconfig ){
        if( n.idx[k]>0 ){
		#sample at kth hyperpar location
                xx = inla.qsample(n = n.idx[k], Q = cs$config[[k]]$Q,
                        mu = INLA:::inla.ifelse(use.improved.mean, cs$config[[k]]$improved.mean, cs$config[[k]]$mean),
                        constr = cs$constr,
			logdens = FALSE,
                        #logdens = TRUE,
                        seed = seed
		)
		#rho = cs$config[[k]]$theta[1]
		rho = do.call( result$misc$from.theta[[1]], args=list(cs$config[[k]]$theta[1]) ) 
		names(rho) = c('rho')
		#memmoize the index
		#if( k==1 ){
			#
			memmo = foreach( p=portGold )%dopar%{
			    #
			    path = sprintf('./%s/', p);
			    dir.create(path)
			    ##
			    #memOut = c()
				for(g in gearGold){ path = sprintf('./%s/%s/', p, g); dir.create(path)
				for(q in qtrGold ){ path = sprintf('./%s/%s/%s/', p, g, q); dir.create(path)
				for(y in yearGold){ path = sprintf('./%s/%s/%s/%s/', p, g, q, y); dir.create(path)
				#for(q in 'qStar'){ path = sprintf('./%s/%s/%s/', p, g, q); dir.create(path)
				#for(y in 'yStar'){ path = sprintf('./%s/%s/%s/%s/', p, g, q, y); dir.create(path)
					#
					not = which(D$year==y & D$qtr==q)
				        #
				        portWhereStr = grep(p, D$port[not])
				        portWhereAll = grep(1, D$port[not])
				        #
				        gearWhereStr = grep(g, D$gear[not])
				        gearWhereAll = grep(1, D$gear[not])
				        ##
				        #qtrWhereStr  = grep(q, D$qtr[not])
				        #qtrWhereAll  = grep(1, D$qtr[not])
					##
					#whichNotQtr  = c(qtrWhereStr , qtrWhereAll )
					whichNotPort = c(portWhereStr, portWhereAll)
					whichNotGear = c(gearWhereStr, gearWhereAll)
					#int = intersect(intersect(whichNotPort, whichNotGear), whichNotQtr)
					int = intersect(whichNotPort, whichNotGear)
					#
					MYes = 0 #the number of accepted draws
					MTot = 0 #total number of draws so far
					MZer = 0 #total number of draws with sum zero so far
					MHat = n.idx[k] #the number of draws to go get now
					#
					if( k==1 ){
						write.table(t(sppGold), file=sprintf("%slpPred.csv",  path), row.names=F, col.names=F, quote=F, sep=',')
						write.table(t(sppGold), file=sprintf("%slpPost.csv",  path), row.names=F, col.names=F, quote=F, sep=',')
        			        	write.table(t(sppGold), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=F, quote=F, sep=',')
					}
					#
					wFlag = 1
					while( MYes<n.idx[k] ){
						#
						sMup = matrix(NA, nrow=MHat, ncol=S)
						sppT = matrix(NA, nrow=MHat, ncol=S)
						for(s in 1:S){
						        #
						        sppWhereStr = grep(sppGold[s], D$spp[not])
						        sppWhereAll = grep(1, D$spp[not])
						        #
						        whichNotSpp = c(sppWhereStr, sppWhereAll)
						        whichNot = intersect(int, whichNotSpp)[1]
							#		   #NOTE: sort out correct index for xx
							sMup[,s] = inv.logit(xx[not[whichNot],])
							alp = sMup[,s]*(1-rho)/rho
							bet = (1-sMup[,s])/rho
							#				#NOTE:correct D$off index
        						sppT[,s] = rbinom(MHat, size=D$off[not[whichNot]], prob=rbeta(MHat, alp, bet))	
							##			   #NOTE: sort out correct index
							#mem = matrix(c(p, g, q, y, s, not[whichNot]), ncol=6)
							##
							#if( wFlag ){
							#	#
							#	if( s==S ){ wFlag=0 }
							#	#
							#	colnames(mem) = c('port', 'gear', 'qtr', 'year', 'spp', 'xxIndex')
							#	memOut = rbind(memOut, mem)
							#}
						}
						#transform sppComps
						sums = rowSums(sppT)
						ss   = sums==0
						sppP = sppT[!ss,]/sums[!ss]
						#
						tThing = head(sppT[!ss,], MHat-MYes)
						mThing = head(sMup[!ss,], MHat-MYes)
						pThing = head(sppP      , MHat-MYes)
						write.table(format(tThing, scientific=T, digits=6), file=sprintf("%slpPred.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
						write.table(format(mThing, scientific=T, digits=6), file=sprintf("%slpPost.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
						write.table(format(pThing, scientific=T, digits=6), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=F, quote=F, append=T, sep=',')
						#
						MYes = MYes + sum(!ss)
						MTot = MTot + MHat
						MZer = MZer + sum(ss)
						pZer = MZer / MTot
						#MHat = M-MYes
						#MHat = max(MHat + ceiling(MHat*pZer), 100)
						##
						#print(c(M, MYes, pZer, MHat))
					}
					#
					met = t(c(MTot, pZer))
					colnames(met) = c('Total', 'Prop Zero')
					write.csv(met, file=sprintf("%szeroSum.test", path), row.names=F, quote=F)
				}}}
				#
				#return( memOut )
				return(0)
			}
			##reduce memmo
			#memmo = do.call(rbind, memmo)	
		#}else{
		#	#
		#	#for(p in portGold){ path = sprintf('./%s/', p);
		#	foreach( p=portGold )%dopar%{
                #                for(g in gearGold){ path = sprintf('./%s/%s/', p, g); 
                #                for(q in qtrGold ){ path = sprintf('./%s/%s/%s/', p, g, q); 
                #                for(y in yearGold){ path = sprintf('./%s/%s/%s/%s/', p, g, q, y);
		#			#
                #                        MYes = 0 #the number of accepted draws
                #                        MTot = 0 #total number of draws so far
                #                        MZer = 0 #total number of draws with sum zero so far
                #                        MHat = n.idx[k] #the number of draws to go get now
		#			#
                #                        while( MYes<n.idx[k] ){
                #                                #
                #                                sppT = matrix(NA, nrow=MHat, ncol=S)
                #                                for(s in 1:S){
                #                                        #sort out correct index for xx and D$off                  
		#					memory = as.integer(memmo[ memmo[,'port']==p & memmo[,'gear']==g & memmo[,'year']==y & memmo[,'qtr']==q & memmo[,'spp']==s, 6 ])
		#					#		   
                #                                        mup = inv.logit(xx[memory,])
                #                                        alp = mup*(1-rho)/rho
                #                                        bet = (1-mup)/rho	
                #                                        #
                #                                        sppT[,s] = rbinom(MHat, size=D$off[memory], prob=rbeta(MHat, alp, bet))	
                #                                }
                #                                #transform sppComps
                #                                sums = rowSums(sppT)
                #                                ss   = sums==0
                #                                sppP = sppT[!ss,]/sums[!ss]
                #                                #
		#				#print()
		#				#print( dim(sppT[!ss,]) )
		#				#print( dim(mup[!ss,]) )
		#				#print( dim(sppP) )
		#				##
		#				tThing = head(sppT[!ss,], MHat-MYes)
		#				#print(dim(tThing))
		#				#mThing = head( mup[!ss,], MHat-MYes)
                #                                #print(dim(mThing))
		#				pThing = head(sppP      , MHat-MYes)
		#				#print(dim(pThing))
		#				#
                #                                write.table(format(tThing, scientific=T, digits=22), file=sprintf("%slpPred.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
                #                                #write.table(format(mThing, scientific=T, digits=22), file=sprintf("%slpPost.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
		#				write.table(format(pThing, scientific=T, digits=22), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=F, quote=F, append=T, sep=',')
                #                                ##
                #                                MYes = MYes + sum(!ss)
                #                                MTot = MTot + MHat
                #                                MZer = MZer + sum(ss)
                #                                pZer = MZer / MTot
                #                                #MHat = M-MYes
                #                                #MHat = max(MHat + ceiling(MHat*pZer), 100)
                #                                ##
                #                                #print(c(M, MYes, pZer, MHat))
                #                        }
		#			##
                #                        #met = t(c(MTot, pZer))
                #                        #colnames(met) = c('Total', 'Prop Zero')
                #                        #write.csv(met, file=sprintf("%szeroSum.test", path), row.names=F, quote=F)
		#		}}}	
		#	}
		#}
	}
}
























#
#BONE YARD
#

#sums[ss] = 1 #NOTE: NULL and fill in with additional samples
			#sppP = sppP/sums
			##record sum(sums==0)
			#write(sprintf("%d", sum(ss)), file=sprintf("%szeroSum.test", path))
			##write to csv
			#write.csv(format(sppT, scientific=T, digits=22), file=sprintf("%slpPost.csv", path), row.names=F, quote=F)
			#write.csv(format(sppP, scientific=T, digits=22), file=sprintf("%ssppComp.csv", path), row.names=F, quote=F)
			##NOTE: check to make sure format isn't breaking model averaging step

##
#modelPredict(
#	portGold, gearGold, yearGold, qtrGold, sppEff, 
#	postSamples[,1], 	#pSamp$size, 
#	pOut,			#pSamp$lpPost,
#	hypeSamples, 		#pSamp$hpPost, 
#	colnames(hypeSamples),	#colnames(pSamp$hpPost), 
#	base[m]
#)









##
#out = list(
#        mlik   = out$mlik[1],
#        waic   = out$waic$waic,
#        dic    = out$dic$dic,
#        #mse   = getMse(dataClean)
#        time   = out$cpu.used["Total"]/60/60/24,
#        group  = list(port=portEff, gear=gearEff, qtr=qtrEff),
#        size   = postSamples[,1], #cbind(postSamples[,1], hypeSamples[,1]),
#        lpPost = pOut,
#        hpPost = hypeSamples
#)

































#rm(list=ls())
#
##
#suppressMessages(library(loo, quietly=TRUE))
#suppressMessages(library(Rcpp, quietly=TRUE))
#suppressMessages(library(INLA, quietly=TRUE))
#suppressMessages(library(gtools, quietly=TRUE))
##suppressMessages(library(Matrix, quietly=TRUE))
##suppressMessages(library(foreach, quietly=TRUE))
#suppressMessages(library(parallel, quietly=TRUE))
##suppressMessages(library(doParallel, quietly=TRUE))
#
##
##source('avgFunc.r')
#sourceCpp('modelFunc.cpp')
#
##
##FUNCTIONS
##
#
##
#getPred = function(p, g, y, q, s){
#        #p: a string with a port name
#        #g: a string with a gear name
#        #y: a string with a year name
#        #q: a string with a quarter name
#        #s: a string with a species name
#
#        #take the location of the first observation (linear predictors at a given
#        here = which(
#                D$port == p &
#                D$gear == g &
#                D$year == y &
#                D$qtr  == q &
#                D$spp  == s
#        )[1]
#        #
#        #return( inla.tmarginal(exp, out$marginals.linear.predictor[[here]]) )
#        return( out$marginals.linear.predictor[[here]] )
#}
#
##
#getMse = function(dataClean){
#	#
#	ports = unique(dataClean$portComplex)
#        flag = length(ports)==1
#	#
#	gears = unique(dataClean$gearGroup)
#        flagGear = length(gears)==1
#	#
#	qtrs = unique(dataClean$qtr)
#        flagQtr = length(qtrs)==1
#	#
#	mse = 0
#        n = length(dataClean$weight)
#        for(j in 1:n){
#                #
#                if(  flag & !flagGear & !flagQtr ){
#			#print('flag & !flagGear & !flagQtr')
#                        predDist = getPred(1, dataClean$gearGroup[j], as.character(dataClean$year[j]), as.character(dataClean$qtr[j]), dataClean$species[j])
#                }
#		if( !flag &  flagGear & !flagQtr ){
#			#print('!flag &  flagGear & !flagQtr')
#			predDist = getPred(dataClean$portComplex[j], 1, as.character(dataClean$year[j]), as.character(dataClean$qtr[j]), dataClean$species[j])
#		}
#		if( !flag & !flagGear &  flagQtr ){
#			#print('!flag & !flagGear &  flagQtr')
#			#predDist = getPred(dataClean$portComplex[j], dataClean$gearGroup[j], as.character(dataClean$year[j]), 1, dataClean$species[j])
#			predDist = getPred(dataClean$portComplex[j], dataClean$gearGroup[j], as.character(dataClean$year[j]), "1/2/3/4", dataClean$species[j])
#		} 
#		if(  flag &  flagGear & !flagQtr ){
#			#print('flag &  flagGear & !flagQtr')
#			predDist = getPred(1, 1, as.character(dataClean$year[j]), as.character(dataClean$qtr[j]), dataClean$species[j])
#		}	
#		if(  flag & !flagGear &  flagQtr ){
#			#print('flag & !flagGear &  flagQtr')
#			#predDist = getPred(1, dataClean$gearGroup[j], as.character(dataClean$year[j]), 1, dataClean$species[j])
#			predDist = getPred(1, dataClean$gearGroup[j], as.character(dataClean$year[j]), "1/2/3/4", dataClean$species[j])
#		}
#		if( !flag &  flagGear &  flagQtr ){
#			#print('!flag &  flagGear &  flagQtr')
#			#predDist = getPred(dataClean$portComplex[j], 1, as.character(dataClean$year[j]), 1, dataClean$species[j])
#			predDist = getPred(dataClean$portComplex[j], 1, as.character(dataClean$year[j]), "1/2/3/4", dataClean$species[j])
#		}
#		if(  flag &  flagGear &  flagQtr ){
#			#print('flag &  flagGear &  flagQtr')
#			#predDist = getPred(1, 1, as.character(dataClean$year[j]), 1, dataClean$species[j])
#			predDist = getPred(1, 1, as.character(dataClean$year[j]), "1/2/3/4", dataClean$species[j])
#		}
#		if( !flag & !flagGear & !flagQtr ){
#			#print('!flag & !flagGear & !flagQtr')
#                        predDist = getPred(dataClean$portComplex[j], dataClean$gearGroup[j], as.character(dataClean$year[j]), as.character(dataClean$qtr[j]), dataClean$species[j])
#                }
#                #
#                e = inla.emarginal(exp, predDist) 
#                mse = mse + (e-dataClean$weight[j])^2
#        }
#	mse = mse/n
#	#
#	return(mse)
#}
#
##
#toThreads = function(call, nCore){
#	#call: an inla call
#	#
#	#value:
#	#an inla call with updated nCore
#	# 
#	comSplit  = unlist(strsplit(out$call, ','))
#	equSplit = strsplit(comSplit, '=')
#	#
#	thredPlace = grep('num.threads', comSplit)
#	old = equSplit[[thredPlace]][2]
#	out = gsub(old, sprintf(' %d)', nCore), out$call)
#	#
#	return( out )
#}
#
##
#modelSample = function(P, G, Y, Q, S, model, M, cores){
#        #P: a vector of port name strings
#        #G: a vector of gear name strings  
#        #Y: a vector of year name strings 
#        #Q: a vector of quarter name strings
#        #S: a vector of species name strings    
#        #model: string to define the model to be sample from  
#	#M: number of samples to take from posterior
#	#cores: number of cores to use in parallelization
#        #
#        #value:
#        #a list containing
#	#	[[1]]: marginal likelyhood
#	#	[[2]]: WAIC
#	#	[[3]]: DIC
#	#	[[4]]: computation time
#	#	[[5]]: a list of the super groupings
#	#		[[1]]: portG
#	#		[[2]]: gearG
#	#		[[3]]: qtrG
#	#	[[6]]: a vector of NB size posterior samples 
#	#	[[7]]: an ordered matrix of linear predictor posterior samples to use for prediction
#	#	[[8]]: a matrix of hyper-parameter posterior samples
#
#	#
#        load(model)
#	out$call = toThreads(out$call, cores)
#	#
#	postSamples = suppressWarnings(inla.posterior.sample(M, out)) #determine the correct scale hyper.user.scale=TRUE
#	hypeSamples = inla.hyperpar.sample(M,out)
#	#
#	hpNames = names(postSamples[[1]][[1]])
#	lNames 	= rownames(postSamples[[1]][[2]])
#	ldNames = names(postSamples[[1]][[3]])
#	#
#	postSamples = t(matrix(unlist(postSamples), ncol=M))
#	#
#	colnames(hypeSamples)[1] = 'size'
#	colnames(postSamples)    = c(hpNames, lNames, ldNames)	
#	#big gains can be had by parallelizing these loops, but the indexing issue/shared memory options create a nightmare
#	i = 1
#	pOut = matrix(NA, nrow=M, ncol=length(P)*length(G)*length(Y)*length(Q)*length(S))
#	for(p in P){ 
#        for(g in G){
#	for(q in Q){
#        for(y in Y){  
#        for(s in S){
#                #
#		not = which(D$year==y)
#                #
#                portWhereStr = grep(p, D$port[not])
#                portWhereAll = grep(1, D$port[not])
#                #
#                gearWhereStr = grep(g, D$gear[not])
#                gearWhereAll = grep(1, D$gear[not])
#                #
#                qtrWhereStr = grep(q, D$qtr[not])
#                qtrWhereAll = grep(1, D$qtr[not])
#		#
#		sppWhereStr = grep(s, D$spp[not])
#                sppWhereAll = grep(1, D$spp[not])
#                #
#		whichNotSpp  = c(sppWhereStr , sppWhereAll )
#                whichNotQtr  = c(qtrWhereStr , qtrWhereAll )
#                whichNotPort = c(portWhereStr, portWhereAll)
#                whichNotGear = c(gearWhereStr, gearWhereAll)
#                #
#		#whichNot  = intersect(intersect(whichNotPort, whichNotGear), whichNotQtr)[1]
#		whichNot  = intersect(intersect(intersect(whichNotPort, whichNotGear), whichNotQtr), whichNotSpp)[1]
#		pOut[, i] = exp(postSamples[, length(hpNames)+not[whichNot]])
#		#
#		i = i+1
#        }}}}}
#	#
#	out = list( 
#		mlik   = out$mlik[1],
#		waic   = out$waic$waic,
#        	dic    = out$dic$dic,
#        	#mse   = getMse(dataClean)
#        	time   = out$cpu.used["Total"]/60/60/24,
#		group  = list(port=portEff, gear=gearEff, qtr=qtrEff),
#		size   = postSamples[,1], #cbind(postSamples[,1], hypeSamples[,1]),
#		lpPost = pOut,
#		hpPost = hypeSamples
#	)
#        #
#        return( out )
#}
#
##
#goodRatios = function( loggedStuff ){
#        #loggedStuff: the log(x) of the values (x) to compute the following ratio
#        #       r_i = x_i/sum(x)
#
#        #
#        c = max(loggedStuff)
#        stand = exp(loggedStuff-c)
#        sStand = sum(stand)
#        out = stand/sStand
#        #
#        return( out )
#}
#
###
##modelClear = function(name, portGold){
##	for(pg in portGold){
##		system('rm -r %s*/%s', name, pg)
##	}
##}
#
##
##MAIN
##assume all of the samples for a single model fit in memory
#
##
##set.seed(24)
#M     = 10^4
#cores = detectCores() #4
##
#name = <<name>>
#gob  = sprintf("../%s*/*.RData", name) #'../Space*/*.RData'#'../SpaceBottom[12345]/*.RData'
#glob = mixedsort(Sys.glob(gob))
#nl   = length(glob)
#base = unlist(strsplit(glob, '/'))[seq(2, nl*3, 3)]
##
#load(glob[length(glob)])
##
#writeLines('\nModel Prediction:')
#modelInit(portGold, gearGold, yearGold, qtrGold, base)
#pb = txtProgressBar(min=0, max=nl)
##
#logmlik = matrix(NA, nl, 1)
#waic = matrix(NA, nl, 1)
#for(m in 1:nl){
#	#
#	pSamp = modelSample(
#		portGold, gearGold, as.character(yearGold), as.character(qtrGold), 
#		sppEff, glob[m], M, cores
#	)
#	#
#	logmlik[m] = pSamp$mlik
#	waic[m] = pSamp$waic
#	#
#	modelPredict(
#		portGold, gearGold, yearGold, qtrGold, sppEff, pSamp$size, pSamp$lpPost, 
#		pSamp$hpPost, colnames(pSamp$hpPost), base[m]
#	)	
#	setTxtProgressBar(pb, m)
#}
##
#ws = goodRatios(logmlik)
#ms = round(ws*M)
##
#rownames(logmlik) = base
#rownames(waic)    = base 
#rownames(ws)      = base
##strata to consider in model averaging (in order of strata tree)
#strata = apply(apply(expand.grid(portGold, gearGold, qtrGold, yearGold), 2, as.character), 1, paste, collapse='/')
##
#writeLines('\n\nModel Averaging...')
#modelAverage(ms, base, strata)
##clear out model selection sample files
#for(pg in portGold){ system(sprintf('rm -r ../%s*/%s', name, pg)) }










##sourceCpp('modelFunc.cpp')
###modelPredict(
	#	portGold, gearGold, yearGold, qtrGold, sppEff, 
	#	pSamp$size, pSamp$lpPost, pSamp$hpPost, base[m] 
	#)
##
##
#
##
##descMSE  = sort( cache$mse[!is.na(cache$mse) ])
#descDIC  = sort( cache$dic[!is.na(cache$dic) ])
#descWAIC = sort(cache$waic[1:nl][!is.na(cache$waic)])
#descMILK = sort(cache$mlik[!is.na(cache$mlik)])
##
###bestiMSE  = order( cache$mse[!is.na(cache$mse)] )
#bestiDIC  = order( cache$dic[!is.na(cache$dic)] )
#bestiWAIC = order(cache$waic[1:nl][!is.na(cache$waic)])
#bestiMILK = order(cache$mlik[!is.na(cache$mlik)])
##
##bestSGMSE  = list()
#bestSGDIC  = list()
#bestSGWAIC = list()
#bestSGMILK = list() 
#for(i in 1:nl){
#        #
#        #bestSGMSE[[i]]  = list()
#        bestSGDIC[[i]]  = list()
#        bestSGWAIC[[i]] = list()
#	bestSGMILK[[i]] = list()
#        #
#        #bestSGMSE[[i]]$pg = cache$portG[[bestiMSE[i]]]
#	#bestSGMSE[[i]]$gg = cache$gearG[[bestiMSE[i]]]
#        #bestSGMSE[[i]]$qg =  cache$qtrG[[bestiMSE[i]]]
#        #
#        bestSGDIC[[i]]$pg = cache$portG[[bestiDIC[i]]]
#        bestSGDIC[[i]]$gg = cache$gearG[[bestiDIC[i]]]
#	bestSGDIC[[i]]$qg =  cache$qtrG[[bestiDIC[i]]]
#        #
#        bestSGWAIC[[i]]$pg = cache$portG[[bestiWAIC[i]]]
#        bestSGWAIC[[i]]$gg = cache$gearG[[bestiWAIC[i]]]
#	bestSGWAIC[[i]]$qg =  cache$qtrG[[bestiWAIC[i]]]
#	#
#        bestSGMILK[[i]]$pg = cache$portG[[bestiMILK[i]]]
#        bestSGMILK[[i]]$gg = cache$gearG[[bestiMILK[i]]]
#	bestSGMILK[[i]]$qg =  cache$qtrG[[bestiMILK[i]]]
#}

#
#save.image('postProcess.RData')
#load('test.RData')














#
#sl = length(strata)
#
#registerDoParallel(cores/2)
#foreach(is = 1:sl) %dopar% strataAverage(ws, strata[is], base, cores/2)
#writeLines('\nModel Averaging:')
#pb = txtProgressBar(min=0, max=sl)
#for(is in 1:sl){
#	av = strataAverage(ws, strata[is], base, cores/2) 
#        setTxtProgressBar(pb, is)
#}































##
#toSpline = function(samp){
#	#samp: a sample to be converted into a splined density
#	#
#	dens = density(samp)
#	out = cbind(dens$x, dens$y)#inla.smargtinal
#	#
#	return(out)
#}

##
#sppAvg = function(w, s, sppComp){
#	#s: a species code strings
#	#mods: a model strings
#	#sppComp: sppComp predictive samples from each model in a particular strata
#	#
#	if( any(w==1) ){
#		here = which(w==1)[1]
#                return( (toSpline(sppComp[[here]][,s])) )
#	}else{
#		mods = names(sppComp)
#		splMod = list(); 
#		for(m in 1:length(mods)){ 
#			splMod[[mods[m]]] = toSpline(sppComp[[m]][,s])
#		}
#		#margAvg is a bad egg => look at better ways to do the averaging
#		#consider a sampling approach
#		return( (margAvg(splMod, w)) )
#	}
#}
#
##
#strataAverage = function(weights, strata, mods, cores){
#	#weights: weights to use in model averaging
#	#strata: the current strata to average
#	#
#	
#	#
#	registerDoParallel(cores)
#	#
#	sppComp = list()
#	for(m in 1:length(mods)){ sppComp[[mods[m]]]=read.csv(sprintf("../%s/%s/sppComp.csv", mods[m], strata)) }
#	#
#	spp = names(sppComp[[1]])
#	avgComp = foreach(s=spp) %dopar% sppAvg(weights, s, sppComp) 
#	names(avgComp) = spp
#	#
#	return(avgComp)
#}




#library(Rdsm, quietly=TRUE)
#library(maps, quietly=TRUE)
#library(parallel, quietly=TRUE)
#library(intervals, quietly=TRUE)
#library(RColorBrewer, quietly=TRUE)

##
##determine the correct scale hyper.user.scale=TRUE
#postSamples = inla.posterior.sample(M, out)
#hypeSamples = inla.hyperpar.sample(M,out)
#hpNames = names(postSamples[[1]][[1]])
#lNames = rownames(postSamples[[1]][[2]])
#ldNames = names(postSamples[[1]][[3]])
	#postSamples = t(matrix(unlist(postSamples), ncol=M))
#colnames(postSamples) = c(hpNames, lNames, ldNames)
#postSamples[,hpNames] = hypeSamples

#
#modelAverage(models, ws)
#Order Appropriate postSamples
#Sample predictives in parrallel, weight and pi
#Save csv files to model directories
#
#Average models
#	samples
#	bma package (unreliable)
#Samples
#	weights determine the proportion of samples comming from each model

#
#cache = makeCacheAll(
#	portGold, 
#	gearGold, 
#	as.character(yearGold), 
#	as.character(qtrGold), 
#	sppEff,
#	gob,
#	M
#)
#names(cache$mlik) = base
#names(cache$waic) = base
#names(cache$dic)  = base
##names(cache$mse)  = base
#names(cache$time) = base
##
#nD = length(D$weight)
#x = matrix(NA, nrow=M, ncol=nD)
##
#w = goodRatios(cache$mlik) #rep(1/nl, nl) #
#check = which(w==1); 
#if( length(check)>1 ){ 
#	check = check[1]
#	warning(sprintf('Degenerate Model Draw: %s Used', strsplit(glob[check], '/')[[1]][2])) 
#}
#avgFlag = length(check)>0
##
#sizeSam = cacheSampSize(cache)
#names(sizeSam) = base
#postAvg = list()
#for(i in 1:nl){ 
#	#
#	postAvg[[i]] = list()
#	postAvg[[i]][['size']] = toSpline(sizeSam[[i]]) 
#}
##
#if( avgFlag ){
#	##
#	#pb = txtProgressBar(min=0, max=nl*length(portGold)*length(gearGold)*length(yearGold)*length(qtrGold)*length(sppGold)+1)
#}else{
#	##
#	#pb = txtProgressBar(min=0, max=2*nl*length(portGold)*length(gearGold)*length(yearGold)*length(qtrGold)*length(sppGold)+1) 
#	#
#	sizeMarg = list()
#	for(i in 1:nl){ sizeMarg[[i]]=postAvg[[i]][['size']] }
#	cache$parPost[['Average']][['size']] = inla.smarginal(margAvg(sizeMarg, w))
#}
#
##
#writeLines('\nModel Averaging...')
##setTxtProgressBar(pb, 1)
##
#i = 1
##
#qtrGold  = as.character(qtrGold)
#yearGold = as.character(yearGold)
##
#predAvg = list()
##postAvg = list()
#for(m in 1:nl){
#	#
#	predAvg[[m]] = list()
#	#postAvg[[m]] = list()
#	cache$piPred[[m]] = list()
#	for(p in portGold){
#		# 	
#		predAvg[[m]][[p]] = list()
#		postAvg[[m]][[p]] = list()
#		cache$piPred[[m]][[p]] = list()
#		for(g in gearGold){ 
#			#
#			predAvg[[m]][[p]][[g]] = list()
#			postAvg[[m]][[p]][[g]] = list()
#			cache$piPred[[m]][[p]][[g]] = list()
#			for(y in yearGold){
#				# 
#				predAvg[[m]][[p]][[g]][[y]] = list()
#				postAvg[[m]][[p]][[g]][[y]] = list()
#				cache$piPred[[m]][[p]][[g]][[y]] = list();   
#				for(q in qtrGold ){
#					#
#					predAvg[[m]][[p]][[g]][[y]][[q]] = list()
#					postAvg[[m]][[p]][[g]][[y]][[q]] = list()
#					#
#					sppMat = matrix(NA, nrow=M, ncol=nSpp)
#					colnames(sppMat) = sppGold
#					for(s in sppGold){				
#        					#
#        					avgSpp = cacheSampAll(p, g, y, q, s, cache)
#        					mu = exp( avgSpp[[m]] )	
#						#
#						sppMat[,s] = rnbinom(M, size=sizeSam[[m]], mu=mu)
#						#predAvg[[m]][[p]][[g]][[y]][[q]][[s]] = toSpline(sppMat[,s])
#						#postAvg[[m]][[p]][[g]][[y]][[q]][[s]] = toSpline(mu)
#						##	
#						#i  = i+1
#						#setTxtProgressBar(pb, i)
#					}
#					cache$piPred[[m]][[p]][[g]][[y]][[q]] = sppMat/rowSums(sppMat)
#				}
#			}
#		}
#	}
#}
#
##
##avgWaic = waic(x)
##
#if( avgFlag ){ 
#	#
#	cache$waic[['Average']]    = cache$waic[[check]]
#	cache$piPred[['Average']]  = predAvg[[check]]
#	#cache$parPost[['Average']] = postAvg[[check]]
#}else{
#	#
#	for(p in portGold){
#	for(g in gearGold){
#	for(y in yearGold){
#	for(q in qtrGold ){
#	for(s in sppGold ){				
#	       	#
#		predMarg = list()
#		#postMarg = list()
#		for(m in 1:nl){
#			#
#			predMarg[[m]] = toSpline(cache$piPred[[m]][[p]][[g]][[y]][[q]][[s]]) #predAvg[[m]][[p]][[g]][[y]][[q]][[s]]
#			#postMarg[[m]] = postAvg[[m]][[p]][[g]][[y]][[q]][[s]]
#			##
#			#i  = i+1
#                	#setTxtProgressBar(pb, i)
#		}
#		#
#		cache$piPred[['Average']][[p]][[g]][[y]][[q]][[s]] = inla.smarginal(margAvg(predMarg, w))	
#		#cache$parPost[['Average']][[p]][[g]][[y]][[q]][[s]] = inla.smarginal(margAvg(postMarg, w))
#		##
#                #where = which(
#                #        D$port == p     &
#                #        D$gear == g     &
#                #        D$year == y     &
#                #        D$qtr  == q     &
#                #        D$spp  == s
#                #)
#                #for(w in where){ x[,w]=dnbinom(D$weight[w], sizeAvgPost, mu=muAvgPost, log=T) }
#		#GET SAMPLES FROM JOINT POSTERIOR OF AVERAGE MODEL
#		#i  = i+1
#		#setTxtProgressBar(pb, i)
#	}}}}}
#}
#names(cache$piPred) = c(base, 'Average')
##names(cache$parPost) = c(base, 'Average')
#
##add a separate lp script
##get port ordering here and save cache.
#
##
##bestn = num
##
##descMSE  = sort( cache$mse[!is.na(cache$mse) ])
#descDIC  = sort( cache$dic[!is.na(cache$dic) ])
#descWAIC = sort(cache$waic[1:nl][!is.na(cache$waic)])
#descMILK = sort(cache$mlik[!is.na(cache$mlik)])
##
##bestiMSE  = order( cache$mse[!is.na(cache$mse)] )
#bestiDIC  = order( cache$dic[!is.na(cache$dic)] )
#bestiWAIC = order(cache$waic[1:nl][!is.na(cache$waic)])
#bestiMILK = order(cache$mlik[!is.na(cache$mlik)])
##
##bestSGMSE  = list()
#bestSGDIC  = list()
#bestSGWAIC = list()
#bestSGMILK = list() 
#for(i in 1:nl){
#        #
#        #bestSGMSE[[i]]  = list()
#        bestSGDIC[[i]]  = list()
#        bestSGWAIC[[i]] = list()
#	bestSGMILK[[i]] = list()
#        #
#        #bestSGMSE[[i]]$pg = cache$portG[[bestiMSE[i]]]
#	#bestSGMSE[[i]]$gg = cache$gearG[[bestiMSE[i]]]
#        #bestSGMSE[[i]]$qg =  cache$qtrG[[bestiMSE[i]]]
#        #
#        bestSGDIC[[i]]$pg = cache$portG[[bestiDIC[i]]]
#        bestSGDIC[[i]]$gg = cache$gearG[[bestiDIC[i]]]
#	bestSGDIC[[i]]$qg =  cache$qtrG[[bestiDIC[i]]]
#        #
#        bestSGWAIC[[i]]$pg = cache$portG[[bestiWAIC[i]]]
#        bestSGWAIC[[i]]$gg = cache$gearG[[bestiWAIC[i]]]
#	bestSGWAIC[[i]]$qg =  cache$qtrG[[bestiWAIC[i]]]
#	#
#        bestSGMILK[[i]]$pg = cache$portG[[bestiMILK[i]]]
#        bestSGMILK[[i]]$gg = cache$gearG[[bestiMILK[i]]]
#	bestSGMILK[[i]]$qg =  cache$qtrG[[bestiMILK[i]]]
#}
#
##
##SAVE
##
#
#save.image('postProcess.RData')
#
#
#
###
###PROCESS
###
##
###
##doneFile = 'doneJobs.txt'
##doneStr = readChar(doneFile, file.info(doneFile)$size)
###doneStr = sprintf("SpaceMntFull%d", 1:512)
##exStr = c("")
##doneStr = doneStr[!doneStr%in%exStr]
###
##l = list.dirs()
##l = l[grepl('Space', l)] #choose the correct string to search the directory
##nl = length(l)
###
##outs = list()
##DIC = matrix(NA, nrow=nl, ncol=1)
##mLike = matrix(NA, nrow=nl, ncol=1)
##times = matrix(NA, nrow=nl, ncol=1)
##mses = times
##WAIC = times
###
##portGroups = list()
##gearGroups = list()
##qtrGroups = list()
###
##for( i in seq(1, nl) ){
##	#
##	parts = strsplit(l[i], '/')[[1]]
##	n = tail(parts, 1)	
##	#
##	print(n)	
##	if( grepl(sprintf("%s", n), doneStr) ){ #n %in% doneStr ){ #
##		#
##		load(sprintf('./%s/%s.RData', n, n))
##		#
##		#outs[[i]] = out
##		DIC[i] = out$dic$dic
##		WAIC[i] = out$waic$waic
##		mLike[i] = out$mlik[1]
##		times[i] = out$cpu.used["Total"]/60/60/24
##		mses[i] = getMse(dataClean)
##		#
##                portGroups[[i]] = portEff
##                gearGroups[[i]] = gearEff
##		qtrGroups[[i]] = qtrEff
##	}
##	print(i)
##	writeLines('\n')
##}
##
###
###GET PARTITIONS
###
##
###set namespace to the sparse model to get eff ordering
##num = length(WAIC)
##nameType = gsub("\\d", "", l[1])
##last = sprintf('%s%s', nameType, num)
##last = substr(last, 3, nchar(last))
##load(sprintf('./%s/%s.RData', last, last))
##texts = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
##lss = ls()
##
###
##bestn = num
###
##descMSE = sort(mses[!is.na(mses)])
##descDIC = sort(DIC[!is.na(DIC)])
##descWAIC = sort(WAIC[!is.na(WAIC)])
###
##bestiMSE = which(mses%in%head(descMSE, bestn))
##bestiDIC = which(DIC%in%head(descDIC, bestn))
##bestiWAIC = which(WAIC%in%head(descWAIC, bestn))
###
##bestMSE = sort(mses[bestiMSE])
##bestDIC = sort(DIC[bestiDIC])
##bestWAIC = sort(WAIC[bestiWAIC])
###
##bestIMSE = bestiMSE[order(mses[bestiMSE])]
##bestIDIC = bestiDIC[order(DIC[bestiDIC])]
##bestIWAIC = bestiWAIC[order(WAIC[bestiWAIC])]
###
##bestLMSE = l[bestIMSE]
##bestLDIC = l[bestIDIC]
##bestLWAIC = l[bestIWAIC]
###
##llIMSE = as.numeric(gsub("\\D", "", bestLMSE)) 
##llIDIC = as.numeric(gsub("\\D", "", bestLDIC))
##llIWAIC = as.numeric(gsub("\\D", "", bestLWAIC))
###
##bestSGMSE = list()#matrix(NA, nrow=nl, ncol=2)
##bestSGDIC = list()
##bestSGWAIC = list()
##for(i in 1:nl){
##        #
##        bestSGMSE[[i]] = list()
##        bestSGDIC[[i]] = list()
##        bestSGWAIC[[i]] = list()
##        #
##        bestSGMSE[[i]]$pg = portGroups[[order(mses[bestiMSE])[i]]]
##        bestSGMSE[[i]]$qg = qtrGroups[[order(mses[bestiMSE])[i]]]
##        #
##        bestSGDIC[[i]]$pg = portGroups[[order(DIC[bestiDIC])[i]]]
##        bestSGDIC[[i]]$gg = gearGroups[[order(DIC[bestiDIC])[i]]]
##	bestSGDIC[[i]]$qg = qtrGroups[[order(DIC[bestiDIC])[i]]]
##        #
##        bestSGWAIC[[i]]$pg = portGroups[[order(WAIC[bestiWAIC])[i]]]
##        bestSGWAIC[[i]]$gg = gearGroups[[order(WAIC[bestiWAIC])[i]]]
##	bestSGWAIC[[i]]$qg = qtrGroups[[order(WAIC[bestiWAIC])[i]]]
##}
###
##BF = getBF(mLike)
##
###
###SAVE
###
##
###
##save(bestDIC, bestWAIC, 		#bestMSE, 
##	bestIDIC, bestIWAIC, 		#bestIMSE, 
##	bestLDIC, bestLWAIC, 		#bestLMSE, 
##	bestSGDIC, bestSGWAIC, 		#bestSGMSE, 
##	BF, DIC, WAIC, times, mLike, 	# mses, outs
##        list = lss,
##        file = sprintf('postProcess.RData')
##)
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
#
###
##nome = unlist(lapply(bestSG, paste, collapse = '\n'))
##names(bestDIC) = nome
###
###pdf('mntFullDICBar.pdf', width=30)
###dev.new()
##pltThing = head(bestDIC, 9)
##M = max(pltThing)
##m = min(pltThing)
##dm = (M-m)/4
##barplot( pltThing,
##        ylim=c(m-dm, M+dm),
##        ylab='DIC',
##	axisnames=F,
##        main=expression(kappa*'=0'), #sprintf('%d-Way Best', (!not3Way)+2),
##	col=brewer.pal(length(pltThing),"Blues"),
##	cex.main=2,
##        cex.axis=1.5,
##        cex.lab=1.5
##)
###
##dev.off()
#
###
##texts = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
###
##groups = strsplit(names(pltThing), '\n')
##num = max(unlist(lapply(groups, length)))
##cols = brewer.pal(max(num, 3),"Set1")
##colAss = matrix("white", nrow=length(texts), ncol=1)
##j=1
##for(g in groups){
##        #	
##        pc = strsplit(g, '/')
##	#
##	print(g)
##	print(pc)
##	print('')
##	#
##        i=1
##        for(pp in pc){
##                #
##                ww = which(texts%in%pp)
##                colAss[ww] = cols[i]
##                #
##                i=i+1
##        }
##        ##
##        #pdf(sprintf('mntFullMap%s.pdf', j), width=7, height=15)
##        ##dev.new(width=7, height=15)
##        #map(database="state", regions="california",
##        #        projection="gilbert",
##        #        orientation=c(120, 0, 240),
##        #)
##        ##
##        #top = 0.98
##        #bot=0.12
##        ##
##        #mtext(texts,
##        #        side=2,
##        #        adj=seq(top, bot, -(top-bot)/(length(texts)-1)),
##        #        cex=2.5,
##        #        col=colAss
##        #)
##        #dev.off()
##        j=j+1
##}
#
#
#
###pdf(sprintf('worst%d.pdf', (!not3Way)+2), width=13)
##dev.new()
##pltThing = tail(bestDIC, 5)
##M = max(pltThing)
##m = min(pltThing)
##dm = (M-m)/4
##barplot( pltThing,
##        ylim=c(m-dm, M+dm),
##        ylab='DIC',
##        main=expression(kappa*'=0'),#sprintf('%d-Way Worst', (!not3Way)+2),
##	col=rev(brewer.pal(length(pltThing),"Set1")),
##	cex.main=1.5	
##)
#
##pdf('mntLowTimesSkp2.pdf')
##hist(times,
##        xlab='Days Per Model',
##        main="2-Way Intercation Models; K=5",#expression('South of MNT (K=5, '*k
##       xlim=c(0, 1)
##)
##abline(v=mean(times), col='red', lty=5)
##dev.off()
#
##
#makeCacheAll = function(P, G, Y, Q, S, gobbler, M){
#        #P: a vector of port name strings
#        #G: a vector of gear name strings  
#        #YE a vector of year name strings 
#        #Q: a vector of quarter name strings
#        #S: a vector of species name strings    
#        #gobbler: string to define the models to be averaged together based on unix wildcard standards  
#	#M: number of samples to use in building posterior
#        #
#        #value:
#        #a cache as a list[[modle]][[port]][[gear]][[qtr]][[spp]][[year]]
#        #where model is the model number as implied by the return order of gobbler
#
#        #
#        writeLines('\nCaching Samples:')
#        fileOut = fileApply( function(n){
#                #
#                load(n)
#		out$call = toThreads(out$call, 24)
#		#
#		postSamples = inla.posterior.sample(M, out) #determine the correct scale hyper.user.scale=TRUE
#		hypeSamples = inla.hyperpar.sample(M,out)
#		#
#		hpNames = names(postSamples[[1]][[1]])
#		lNames 	= rownames(postSamples[[1]][[2]])
#		ldNames = names(postSamples[[1]][[3]])
#		#
#		postSamples = t(matrix(unlist(postSamples), ncol=M))
#		colnames(hypeSamples)[1] = 'size'#FIX HERE
#		#
#		colnames(postSamples) = c(hpNames, lNames, ldNames)
#		#postSamples[,hpNames] = hypeSamples #gives better distribution, but not exactly a full posterior sample
#                #
#		lp = list( size=postSamples[,hpNames[1]] ) #toSpline(postSamples[,hpNames[1]]) )
#                for(p in P){ lp[[p]] = list()
#                for(g in G){ lp[[p]][[g]] = list()
#                for(y in Y){ lp[[p]][[g]][[y]] = list()
#                for(q in Q){ lp[[p]][[g]][[y]][[q]] = list()
#                for(s in S){ lp[[p]][[g]][[y]][[q]][[s]] = list()
#                        #
#			not = which(D$year==y & D$spp==s)
#                        #
#                        portWhereStr = grep(p, D$port[not])
#                        portWhereAll = grep(1, D$port[not])
#                        #
#                        gearWhereStr = grep(g, D$gear[not])
#                        gearWhereAll = grep(1, D$gear[not])
#                        #
#                        qtrWhereStr = grep(q, D$qtr[not])
#                        qtrWhereAll = grep(1, D$qtr[not])
#                        #
#                        whichNotQtr  = c(qtrWhereStr , qtrWhereAll )
#                        whichNotPort = c(portWhereStr, portWhereAll)
#                        whichNotGear = c(gearWhereStr, gearWhereAll)
#                        #
#			whichNot = intersect(intersect(whichNotPort, whichNotGear), whichNotQtr)[1]
#                        #
#                        lp[[p]][[g]][[y]][[q]][[s]] = postSamples[, length(hpNames)+not[whichNot]] #toSpline(postSamples[, length(hpNames)+not[whichNot]])
#                }}}}}
#		#
#                mlik = out$mlik[1]
#		waic = out$waic$waic
#                dic  = out$dic$dic
#                #mse  = getMse(dataClean)
#                time = out$cpu.used["Total"]/60/60/24
#		#
#		#group = list(port=portEff, gear=gearEff, qtr=qtrEff)
#		portG = portEff
#                gearG = gearEff
#		qtrG  = qtrEff
#                #
#                outt = list(
#			mlik  	= mlik,	
#                        waic  	= waic,
#                        dic   	= dic,
#                        #mse    = mse,
#                        time  	= time,
#			portG 	= list(portG),
#			gearG 	= list(gearG),
#			qtrG  	= list(qtrG),
#			hypSamp = list(hypeSamples),
#                        parPost	= list(lp)	
#                )
#                }, gobbler, c() )
#        #
#        return( fileOut )
#}
#
##
#cacheSampAll = function(p, g, y, q, s, cash){
#        #p: port string for recalling from cache
#        #g: gear string for recalling from cache
#        #y: year string for recalling from cache
#        #q: qtr string for recalling form cache
#        #s: spp string for recalling from cache
#        #cash: the cache to recall id from
#        #
#        #value:
#        #an inla marginal of the BMA lp posterior
#
#        #
#        nModels = length(cash$parPost)
#        margs = list()
#        for(i in 1:nModels){
#                margs[[i]] = cash$parPost[[i]][[p]][[g]][[y]][[q]][[s]]
#        }
#        #
#        return( margs )
#}
#
##
#cacheSampSize = function(cash){
#        #cash: the cache to recall id from
# 
#	#
#        nModels = length(cash$parPost)
#        margs = list()
#        for(i in 1:nModels){
#                margs[[i]] = cash$hypSamp[[i]][,'size'] #parPost[[i]][['size']]
#        }
#        #
#        return( margs )
#}

#
#pOut = parReord(P, G, Y, Q, S, D, postSamples, hpNames, reord)
	#use parapplyMPI with reorder
	#pOut = mclapply( S, reord, P=P, G=G, Y=Y, Q=Q, D=D, postSamples=postSamples, hpNames=hpNames, mc.cores=detectCores() )

#reord = function(s, P, G, Y, Q, D, postSamples, hpNames){
#	#
#        not = which(D$year==y & D$spp==s)
#        #
#        portWhereStr = grep(p, D$port[not])
#        portWhereAll = grep(1, D$port[not])
#        #
#        gearWhereStr = grep(g, D$gear[not])
#        gearWhereAll = grep(1, D$gear[not])
#        #
#        qtrWhereStr = grep(q, D$qtr[not])
#        qtrWhereAll = grep(1, D$qtr[not])
#        #
#        whichNotQtr  = c(qtrWhereStr , qtrWhereAll )
#        whichNotPort = c(portWhereStr, portWhereAll)
#        whichNotGear = c(gearWhereStr, gearWhereAll)
#        #
#        whichNot  = intersect(intersect(whichNotPort, whichNotGear), whichNotQtr)[1]
#	#
#        return( exp(postSamples[, length(hpNames)+not[whichNot]]) )
#}
##
#getPred = function(p, g, y, q, s){
#        #p: a string with a port name
#        #g: a string with a gear name
#        #y: a string with a year name
#        #q: a string with a quarter name
#        #s: a string with a species name
#
#        #take the location of the first observation (linear predictors at a given
#        here = which(
#                D$port == p &
#                D$gear == g &
#                D$year == y &
#                D$qtr  == q &
#                D$spp  == s
#        )[1]
#        #
#        #return( inla.tmarginal(exp, out$marginals.linear.predictor[[here]]) )
#        return( out$marginals.linear.predictor[[here]] )
#}
#



