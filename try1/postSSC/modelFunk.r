#
#DEPENDENCIES
#

#
suppressMessages(library(boot, quietly=TRUE))
suppressMessages(library(INLA, quietly=TRUE))
suppressMessages(library(foreach, quietly=TRUE))
suppressMessages(library(doParallel, quietly=TRUE))

#
#FUNCTIONS
#

#noninformative model computed by emperical bayes
ebModel = function(formula, data, starter=NULL, cores=NULL){
        #warning('warn')
        writeLines('Running EB Model...\n')
        out = inla(formula, family="betabinomial", data=data, Ntrials=data$nBB, num.threads=cores,
                control.inla=list(
                        int.strategy='eb',
                        tolerance=1e-10,
                        h=1e-10,
                        lincomb.derived.only=F
                ),
                control.compute=list(
                        config=T,
                        mlik=T,
                        dic=T,
                        waic=T
                ),
                #control.fixed = list(
                #        expand.factor.strategy='inla'
                #),
                control.mode = list(
                        result=starter,
                        restart=T
                )
        )
        #
        if( out$mode$mode.status>0 ){ warning('Hessian Not Positive Definite') }
        #
        return( out )
}

#noninformative model
coldModel = function(formula, data, starter=NULL, cores=NULL){       
	#warning('warn')
        writeLines('Running Model...\n')
        out = inla(formula, family="betabinomial", data=data, Ntrials=data$nBB, num.threads=cores,
                control.inla=list(
                        int.strategy='ccd',
                        tolerance=1e-10,
                        h=1e-10,
                        lincomb.derived.only=F
                ),
                control.compute=list(
                        config=T,
                        mlik=T,
                        dic=T,
                        waic=T
                ),
                #control.fixed = list(
                #        expand.factor.strategy='inla'
                #),
                control.mode = list(
                        result=starter,
                        restart=T
                )
        )
        #
        if( out$mode$mode.status>0 ){ warning('Hessian Not Positive Definite') }
        #
        return( out )
}

#run model with easier prior information (i.e. add hotness to the diagonal of the precision matrix)
hotModel = function(formula, data, hotness, starter=NULL, cores=NULL){
        writeLines(sprintf('Running Hot(%d) Model...\n', hotness))
        out = inla(formula, family="betabinomial", data=data, Ntrials=data$nBB, num.threads=cores,
                control.inla=list(
                        int.strategy='eb',      #'ccd',#eb#https://groups.google.com/forum/#!topic/r-inla-discussion-group/uf2ZGh4jmWc
                        tolerance=1e-10,
                        diagonal=hotness,       #https://groups.google.com/forum/#!topic/r-inla-discussion-group/uf2ZGh4jmWc
                        h=1e-10,
                        lincomb.derived.only=F,
                        strategy='gaussian'     #https://groups.google.com/forum/#!topic/r-inla-discussion-group/uf2ZGh4jmWc
                ),
                #control.fixed = list(
                #        expand.factor.strategy='inla'
                #),
                control.mode = list(
                        result=starter,
                        restart=T
                )
        )
        #
        return( out )
}

#run the inla.hyperpar function
hyperModel = function(out){
        writeLines('Running Hyperparameter Model...\n')
        #improve estimates for hyperparameters 
        out = inla.hyperpar(out,
                dz=1,           #default=0.75
                diff.logdens=1, #default=10
                restart=T
        )
}


#for end-user use above functions are for use in constructing this function
runModel = function(formula, data, cores){
	#formula: an R formula to pass to inla
	#data	: a dataframe for regression
	#cores	: number of threads to run inla with
	
	#
	#Try model
	out = tryCatch({
	        out = coldModel(formula, data, cores=cores)
	        out = hyperModel(out)
	#if fail try warming up into the model 
	}, warning=function(war){#consider building a recursive call to hot model with adaptivly decreasing hotness
	        print(war)
	        out = hotModel(formula, data, 1000, cores=cores)
	        out = hotModel(formula, data, 100, starter=out, cores=cores)
	        out = hotModel(formula, data, 10, starter=out, cores=cores)
	        out = hotModel(formula, data, 2, starter=out, cores=cores)
	        out = hotModel(formula, data, 1, starter=out, cores=cores)
	        out = coldModel(formula, data, starter=out, cores=cores)
	        ##
	        #if( out$mode$mode.status>0 ){ stop('Hessian Not Positive Definite After Basic Warmup') }
	        ##
	        out = hyperModel(out)
	        #
	        return(out)
	#if fail try more warming up into the model
	}, error=function(err){
	        print(err)
	        #try more warm up
	        out = tryCatch({
	                out = hotModel(formula, data, 1000, cores=cores)
	                out = hotModel(formula, data, 100, starter=out, cores=cores)
	                out = hotModel(formula, data, 10, starter=out, cores=cores)
	                out = hotModel(formula, data, 5, starter=out, cores=cores)
	                out = hotModel(formula, data, 2, starter=out, cores=cores)
	                out = hotModel(formula, data, 1, starter=out, cores=cores)
	                out = coldModel(formula, data, starter=out, cores=cores)
	                out = hyperModel(out)
	        }, error=function(err){
	                print(err)
	                #if fail try a more robust integration strategy
	                out = tryCatch({
	                        out = hotModel(formula, data, 1000, cores=cores)
	                        out = hotModel(formula, data, 100, starter=out, cores=cores)
	                        out = hotModel(formula, data, 10, starter=out, cores=cores)
	                        out = hotModel(formula, data, 5, starter=out, cores=cores)
	                        out = hotModel(formula, data, 2, starter=out, cores=cores)
	                        out = hotModel(formula, data, 1, starter=out, cores=cores)
	                        out = ebModel(formula, data, starter=out, cores=cores)
	                        #out = hyperModel(out)  
			##if still fail give up 
	                #}, error=function(err){
	                #        metrics = t(c(NA, NA, NA, NA))
	                #        colnames(metrics) = c('mlik', 'waic', 'dic', 'time')
	                #        write.csv(format(metrics, scientific=T, digits=22), file="./metrics.csv", row.names=F, quote=F)
	                #        #
	                #        out = NA
	                #        return(out)
	                }
	                )
			#
	                return(out)
	        }
	        )
	        #
	        return(out)
	}
	)
	#
	return(out)
}

#
sampler = function(model, portGold, gearGold, qtrGold, yearGold, D, M=10^4, cores=10, samplePath='./'){
	#model  	: a model object returned by one of the above modeling functions
	#portGold	: a list of gold standard ports
        #gearGold       : a list of gold standard gears
        #yearGold       : a list of gold standard years
        #qtrGold        : a list of gold standard qtrs
	#D		: the data structure used to fit model [model]
	#M		: the number of posterio samples
	#cores		: number of parallel cores to use
	#samplePath	: where to save samples for later retrieval
	#
	#value		: samples are saved into the file system at [path], NULL returned into memory.
	
	#
	writeLines('\nRunning Sampler...')
	#
	dir.create(samplePath)	
	#make D index string
	d = dim(D)[1]
	nd = nchar(as.character(d))
	dStr = sprintf('Predictor:%%0%dd', nd)
	#sample #NOTE: split up samples upto M is neccessary
	postSamples = inla.posterior.sample(M, fit)
	hypeSamples = inla.hyperpar.sample(M, fit)
	#
	S = length(sppGold)
	rho = hypeSamples[,1]
	#
	#for(p in portGold){ #path = sprintf('./%s/%s/', samplePath, p); dir.create(path)
	registerDoParallel(cores=cores)
	foreach( p=portGold )%dopar%{
		path = sprintf('./%s/%s/', samplePath, p); dir.create(path)
		for(g in gearGold){ path = sprintf('./%s/%s/%s/', samplePath, p, g); dir.create(path)
		for(q in qtrGold ){ path = sprintf('./%s/%s/%s/%s/', samplePath, p, g, q); dir.create(path)
		for(y in yearGold){ path = sprintf('./%s/%s/%s/%s/%s/', samplePath, p, g, q, y); dir.create(path)
			#
			write.table(t(as.character(sppGold)), file=sprintf("%slpPred.csv",  path), row.names=F, col.names=F, quote=F, sep=',')
		        write.table(t(as.character(sppGold)), file=sprintf("%slpPost.csv",  path), row.names=F, col.names=F, quote=F, sep=',')
		        write.table(t(as.character(sppGold)), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=F, quote=F, sep=',')
			#
			pgqyWhere = which(
                               D$port==p &
                               D$gear==g &
                               D$year==y &
                               D$qtr==q  
                        )
			pgqyD = D[pgqyWhere,]
			#
			MZer = 0
			MTot = 0
			MYes = 0
			while( MYes<M ){
                        	#
                        	sMup = matrix(NA, nrow=M, ncol=S)
                        	sppT = matrix(NA, nrow=M, ncol=S)
                        	for(s in 1:S){
					#
					sWhere = which( pgqyD$species==sppGold[s] )
					where = pgqyWhere[sWhere][1]
					##
                        		#where = which(
                        		#        D$port==p &
                        		#        D$gear==g &
                        		#        D$year==y &
                        		#        D$qtr==q  &
					#	D$species==sppGold[s]
                        		#)[1]
					#
                        		sMup[,s] = sapply(postSamples, function(logSam){
                        		        #
                        		        idxStr = sprintf(dStr, where)
                        		        sam = inv.logit( logSam[['latent']][idxStr,] )
                        		        #
                        		        return( sam )
                        		}) 
                        	        alp = sMup[,s]*(1-rho)/rho
                        	        bet = (1-sMup[,s])/rho	
					#
					pp = rbeta(M, alp, bet)
                        		sppT[,s] = rbinom(M, size=D$nBB[where], prob=pp)
                        	}
                        	#transform sppComps
                        	sums = rowSums(sppT)
                        	ss   = sums==0
                        	sppP = sppT[!ss,]/sums[!ss]
                        	#
                        	tThing = head(sppT[!ss,], M-MYes)
                        	mThing = head(sMup[!ss,], M-MYes)
                        	pThing = head(sppP      , M-MYes)
                        	#
				write.table(format(tThing, scientific=T, digits=6), file=sprintf("%slpPred.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
                        	write.table(format(mThing, scientific=T, digits=6), file=sprintf("%slpPost.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
                        	write.table(format(pThing, scientific=T, digits=6), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=F, quote=F, append=T, sep=',')
                        	#
                        	MYes = MYes + sum(!ss)
                        	MTot = MTot + M
                        	MZer = MZer + sum(ss)
                        	pZer = MZer / MTot
                        	#MHat = M-MYes
                        	#MHat = max(MHat + ceiling(MHat*pZer), 100)
                        	##
                        	#print(c(M, MYes, pZer))
                        }
			#
                        met = t(c(MTot, pZer))
                        colnames(met) = c('Total', 'Prop Zero')
                        write.csv(met, file=sprintf("%szeroSum.test", path), row.names=F, quote=F)	
		}}}
	}
	#
	return(NULL)
}


##
			#where = which(
			#	D$port==p &
			#	D$gear==g &
			#	D$year==y &
			#	D$qtr==q
			#)
			##
			#mup   = sapply(postSamples, function(logSam){
                        #	#
                        #	idxStr = sprintf(dStr, where)
                        #	sam = inv.logit( logSam[['latent']][idxStr,] )
                        #	#
                        #	return( sam )
                      	#})
			##
			#alpha = mup*(1-rho)/rho
                        #beta  = (1-mup)/rho
                        ##
                        #pp = rbeta(M, alpha, beta)
			#rbinom(M, size=D$nBB[where], prob=pp)

#
#                        mup   = sapply(postSamples, function(logSam){
#                                #
#                                idxStr = sprintf(dStr, where)
#                                sam = inv.logit( logSam[['latent']][idxStr,] )
#                                #
#                                return( sam )
#                        })
#
##
#sampleModel = function(model, portGold, gearGold, qtrGold, yearGold, D, M=10^4, cores=48, samplePath='./'){ #, inMemory=T){
#	#model	: a model object returned by one of the above modeling functions
#	#M	: number of posterior samples
#	#path	: a file path where to built the samples file tree
#	#
#	#value	: samples are saved into the file system at [path], NULL returned into memory.
#
#	#inMemory:a flag indicating whether to save samples on disk (for memory intensive problems) -or- hold samples in memory (faster)   
#	#
#	#value: if( inMemory==True ){ returns a samples object into memory }
#	#	if( inMemory!=True }{ returns a vector of file paths indicating where samples have been saved }
#
#	##simple inMemory==T case 
#	#if( inMemory ){
#	#	#
#	#	postQY = inla.posterior.sample(M, out)
#	#}	
#
#	#we start here
#	wd = getwd()
#	#new directory
#	dir.create(samplePath, showWarnings = FALSE)
#	#got there
#	setwd(samplePath)
#	#
#	registerDoParallel(cores=cores)
#
#	#
#	#SAMPLE
#	#
#
#	#to be run from the model file
#	writeLines('\nRunning Sampler\n...')
#	suppressMessages(library(boot, quietly=TRUE))
#	#
#	#M = 10^4
#	#if( !all(is.na(out)) ){
#	#
#	S = length(sppGold)
#	##NOTE: moved to model file
#	#metrics = t(c(out$mlik[1], out$waic$waic, out$dic$dic, out$cpu.used['Total']))
#	#colnames(metrics) = c('mlik', 'waic', 'dic', 'time')
#	#write.csv(format(metrics, scientific=T, digits=22), file="./metrics.csv", row.names=F, quote=F)
#	#
#	hypeSamples = inla.hyperpar.sample(M, model) #out)
#	write.csv(format(hypeSamples, scientific=T, digits=22), file="./nuPost.csv", row.names=F, quote=F)
#	#
#	
#	#"FUNCTION" HEADER
#	n = M
#	result = model #out
#	intern = FALSE
#	use.improved.mean = TRUE
#	add.names = TRUE
#	seed = 0L
#	#
#	stopifnot(!missing(result) && any(class(result) == "inla"))
#	if( is.null(result$misc$configs) ){
#	    stop("You need an inla-object computed with option 'control.compute=list(config = TRUE)'.")
#	}
#	#
#	n = as.integer(n)
#	stopifnot(is.integer(n) && n > 0L)
#	cs = result$misc$configs
#	ld = numeric(cs$nconfig)
#	for( i in 1:cs$nconfig ){
#	    ld[i] = cs$config[[i]]$log.posterior
#	}
#	#
#	p = exp(ld - max(ld))
#	idx = sample(1:cs$nconfig, n, prob = p, replace = TRUE)
#	idx = sort(idx)
#	n.idx = numeric(cs$nconfig)
#	n.idx[] = 0
#	for( i in 1:cs$nconfig ){
#	    n.idx[i] = sum(idx == i)
#	}
#	#
#	for( k in 1:cs$nconfig ){
#	        if( n.idx[k]>0 ){
#	                #sample at kth hyperpar location
#	                xx = inla.qsample(n = n.idx[k], Q = cs$config[[k]]$Q,
#	                        mu = INLA:::inla.ifelse(use.improved.mean, cs$config[[k]]$improved.mean, cs$config[[k]]$mean),
#	                        constr = cs$constr,
#	                        logdens = FALSE,
#	                        #logdens = TRUE,
#	                        seed = seed
#	                )
#	                #rho = cs$config[[k]]$theta[1]
#	                rho = do.call( result$misc$from.theta[[1]], args=list(cs$config[[k]]$theta[1]) )
#	                names(rho) = c('rho')
#	                #memmoize the index
#	                if( k==1 ){
#	                        #
#				memmo = list()
#				for(p in portGold){ path = sprintf('./%s/', p);
#	                        #memmo = foreach( p=portGold )%dopar%{
#	                            #
#	                            path = sprintf('./%s/', p);
#	                            dir.create(path)
#	                            #
#	                            memOut = c()
#	                                for(g in gearGold){ path = sprintf('./%s/%s/', p, g); dir.create(path)
#	                                for(q in qtrGold ){ path = sprintf('./%s/%s/%s/', p, g, q); dir.create(path)
#	                                for(y in yearGold){ path = sprintf('./%s/%s/%s/%s/', p, g, q, y); dir.create(path)
#	                                        #
#	                                        not = which(D$year==y & D$qtr==q)
#	                                        #
#	                                        portWhereStr = grep(p, D$port[not])
#	                                        portWhereAll = grep(1, D$port[not])
#	                                        #
#	                                        gearWhereStr = grep(g, D$gear[not])
#	                                        gearWhereAll = grep(1, D$gear[not])
#	                                        ##
#	                                        #qtrWhereStr  = grep(q, D$qtr[not])
#	                                        #qtrWhereAll  = grep(1, D$qtr[not])
#	                                        ##
#	                                        #whichNotQtr  = c(qtrWhereStr , qtrWhereAll )
#	                                        whichNotPort = c(portWhereStr, portWhereAll)
#	                                        whichNotGear = c(gearWhereStr, gearWhereAll)
#	                                        #int = intersect(intersect(whichNotPort, whichNotGear), whichNotQtr)
#	                                        int = intersect(whichNotPort, whichNotGear)
#	                                        #
#	                                        MYes = 0 #the number of accepted draws
#	                                        MTot = 0 #total number of draws so far
#	                                        MZer = 0 #total number of draws with sum zero so far
#	                                        MHat = n.idx[k] #the number of draws to go get now
#	                                        #
#	                                        write.table(t(sppGold), file=sprintf("%slpPred.csv",  path), row.names=F, col.names=F, quote=F, sep=',')
#	                                        write.table(t(sppGold), file=sprintf("%slpPost.csv",  path), row.names=F, col.names=F, quote=F, sep=',')
#	                                        write.table(t(sppGold), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=F, quote=F, sep=',')
#	                                        #
#	                                        wFlag = 1
#	                                        while( MYes<n.idx[k] ){
#	                                                #
#	                                                sMup = matrix(NA, nrow=MHat, ncol=S)
#	                                                sppT = matrix(NA, nrow=MHat, ncol=S)
#	                                                for(s in 1:S){
#	                                                        #
#	                                                        sppWhereStr = grep(sppGold[s], D$spp[not])
#	                                                        sppWhereAll = grep(1, D$spp[not])
#	                                                        #
#	                                                        whichNotSpp = c(sppWhereStr, sppWhereAll)
#	                                                        whichNot = intersect(int, whichNotSpp)[1]
#	                                                        #                  #NOTE: sort out correct index for xx
#	                                                        sMup[,s] = inv.logit(xx[not[whichNot],])
#	                                                        alp = sMup[,s]*(1-rho)/rho
#	                                                        bet = (1-sMup[,s])/rho	
#								print(whichNot)
#								print(whichNotSpp)
#								print(int)
#								print(sppWhereStr)
#								print(sppWhereAll)
#	                                                        #                               #NOTE:correct D$off index
#	                                                        sppT[,s] = rbinom(MHat, size=D$off[not[whichNot]], prob=rbeta(MHat, alp, bet))
#	                                                        #                          #NOTE: sort out correct index
#	                                                        mem = matrix(c(p, g, q, y, s, not[whichNot]), ncol=6)
#	                                                        #
#	                                                        if( wFlag ){
#	                                                                #
#	                                                                if( s==S ){ wFlag=0 }
#	                                                                #
#	                                                                colnames(mem) = c('port', 'gear', 'qtr', 'year', 'spp', 'xxIndex')
#	                                                                memOut = rbind(memOut, mem)
#	                                                        }
#	                                                }
#	                                                #transform sppComps
#	                                                sums = rowSums(sppT)
#	                                                ss   = sums==0
#	                                                sppP = sppT[!ss,]/sums[!ss]
#	                                                #
#	                                                tThing = head(sppT[!ss,], MHat-MYes)
#	                                                mThing = head(sMup[!ss,], MHat-MYes)
#	                                                pThing = head(sppP      , MHat-MYes)
#	                                                write.table(format(tThing, scientific=T, digits=22), file=sprintf("%slpPred.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
#	                                                write.table(format(mThing, scientific=T, digits=22), file=sprintf("%slpPost.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
#	                                                write.table(format(pThing, scientific=T, digits=22), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=F, quote=F, append=T, sep=',')
#	                                                #
#	                                                MYes = MYes + sum(!ss)
#	                                                MTot = MTot + MHat
#	                                                MZer = MZer + sum(ss)
#	                                                pZer = MZer / MTot
#	                                                #MHat = M-MYes
#	                                                #MHat = max(MHat + ceiling(MHat*pZer), 100)
#	                                                ##
#	                                                #print(c(M, MYes, pZer, MHat))
#	                                        }
#	                                        #
#	                                        met = t(c(MTot, pZer))
#	                                        colnames(met) = c('Total', 'Prop Zero')
#	                                        write.csv(met, file=sprintf("%szeroSum.test", path), row.names=F, quote=F)
#	                                }}}
#	                                #
#	                                #return( memOut )
#					memmo = c(memmo, list(memOut))
#	                        }
#	                        #reduce memmo
#	                        memmo = do.call(rbind, memmo)
#	                }else{
#	                        #
#	                        for(p in portGold){ path = sprintf('./%s/', p);
#	                        #foreach( p=portGold )%dopar%{
#	                                for(g in gearGold){ path = sprintf('./%s/%s/', p, g);
#	                                for(q in qtrGold ){ path = sprintf('./%s/%s/%s/', p, g, q);
#	                                for(y in yearGold){ path = sprintf('./%s/%s/%s/%s/', p, g, q, y);
#	                                        #
#	                                        MYes = 0 #the number of accepted draws
#	                                        MTot = 0 #total number of draws so far
#	                                        MZer = 0 #total number of draws with sum zero so far
#	                                        MHat = n.idx[k] #the number of draws to go get now
#	                                        #
#	                                        while( MYes<n.idx[k] ){
#	                                                #
#	                                                sppT = matrix(NA, nrow=MHat, ncol=S)
#	                                                for(s in 1:S){
#	                                                        #sort out correct index for xx and D$off                  
#	                                                        memory = as.integer(memmo[ memmo[,'port']==p & memmo[,'gear']==g & memmo[,'year']==y & memmo[,'qtr']==q & memmo[,'spp']==s, 6 ])
#	                                                        #                  
#	                                                        mup = inv.logit(xx[memory,])
#	                                                        alp = mup*(1-rho)/rho
#	                                                        bet = (1-mup)/rho
#	                                                        #
#	                                                        sppT[,s] = rbinom(MHat, size=D$off[memory], prob=rbeta(MHat, alp, bet))
#	                                                }
#	                                                #transform sppComps
#	                                                sums = rowSums(sppT)
#	                                                ss   = sums==0
#	                                                sppP = sppT[!ss,]/sums[!ss]
#	                                                #
#	                                                #print()
#	                                                #print( dim(sppT[!ss,]) )
#	                                                #print( dim(mup[!ss,]) )
#	                                                #print( dim(sppP) )
#	                                                ##
#	                                                tThing = head(sppT[!ss,], MHat-MYes)
#	                                                #print(dim(tThing))
#	                                                #mThing = head( mup[!ss,], MHat-MYes)
#	                                                #print(dim(mThing))
#	                                                pThing = head(sppP      , MHat-MYes)
#	                                                #print(dim(pThing))
#	                                                #
#	                                                write.table(format(tThing, scientific=T, digits=22), file=sprintf("%slpPred.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
#	                                                #write.table(format(mThing, scientific=T, digits=22), file=sprintf("%slpPost.csv",  path), row.names=F, col.names=F, quote=F, append=T, sep=',')
#	                                                write.table(format(pThing, scientific=T, digits=22), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=F, quote=F, append=T, sep=',')
#	                                                ##
#	                                                MYes = MYes + sum(!ss)
#	                                                MTot = MTot + MHat
#	                                                MZer = MZer + sum(ss)
#	                                                pZer = MZer / MTot
#	                                                #MHat = M-MYes
#	                                                #MHat = max(MHat + ceiling(MHat*pZer), 100)
#	                                                ##
#	                                                #print(c(M, MYes, pZer, MHat))
#	                                        }
#	                                        ##
#	                                        #met = t(c(MTot, pZer))
#	                                        #colnames(met) = c('Total', 'Prop Zero')
#	                                        #write.csv(met, file=sprintf("%szeroSum.test", path), row.names=F, quote=F)
#	                                }}}
#	                        }
#	                }
#	        }
#	}
#
#	#return to where we started
#	setwd(wd)
#}

