rm(list=ls())

#
library(INLA)
library(boot)
library(vioplot)
library(parallel)
library(HDInterval)
library(KernSmooth)
#
INLA:::inla.dynload.workaround()



#
#FUNCTIONS
#

#
goodRatios = function( loggedStuff ){
        #loggedStuff: the log(x) of the values (x) to compute the following ratio
        #       r_i = x_i       /sum(x)

        #
        c = max(loggedStuff) #mean(loggedStuff)
        stand = exp(loggedStuff-c)
        sStand = sum(stand)
        out = stand/sStand
        #
        return( out )
}

getMSE = function(data, means){
	#data: list of length length(means)
	#means: mean for each element of data
	
	#
	N = sum(sapply(data, length))
	M = length(means)
	out = c() 
	for(i in 1:M){ out=c(out, (data[[i]]-means[i])^2) } #mean(XX)*length(data[[i]])/N
	#
	return( mean(out) )
}

#
#CLEAN DATA
#

#dat = read.table('1985_trwl_mcat250.csv', header=T, sep=',')
#Dat = read.table('data85to90.csv', header=F, sep=',', stringsAsFactors=F)
Dat = read.table('data1978To1982.csv', header=T, sep=',', stringsAsFactors=F)
#colnames(Dat) = c('id', 'clust', 'species', 'weight', 'year', 'qtr', 'port', 'portComplex', 'gear', 'mcat', 'total', 'isLive')
#aggregate categories
dat = aggregate( data.frame(Dat$year, Dat$qtr, Dat$portComplex, Dat$gearGroup, Dat$marketCategory, Dat$live), by=list(Dat$sampleNumber, Dat$species), FUN=unique )
colnames(dat) = c('id', 'species', 'year', 'qtr', 'portComplex', 'gear', 'mcat', 'isLive')
#aggregate add weights such that there is one weight per species per sample (aggregate cluster samples)
dat = cbind(dat, aggregate(data.frame(Dat$weight), by=list(Dat$sampleNumber, Dat$species), FUN=sum)[,3])
colnames(dat)[9] = 'weight'
#preallocate for cluster size
dat = cbind( dat, rep(NA, dim(dat)[1]) )
#get cluster weight by sample
clustWeight = aggregate( data.frame(dat$weight), by=list(dat$id), FUN=sum )
#match up total sampled weight with species weights 
for(cw in 1:dim(clustWeight)[1]){ dat[dat[,1]==clustWeight[cw, 1], dim(dat)[2]]=clustWeight[cw, 2] }
colnames(dat)[10] = 'clustSize'
dat$portComplex = as.character(dat$portComplex)


##aggregate categories
#dat = aggregate( data.frame(Dat$year, Dat$qtr, Dat$portComplex, Dat$gear, Dat$mcat, Dat$isLive), by=list(Dat$id, Dat$species), FUN=unique )
#colnames(dat) = c('id', 'species', 'year', 'qtr', 'portComplex', 'gear', 'mcat', 'isLive')
##aggregate add weights such that there is one weight per species per sample (aggregate cluster samples)
#dat = cbind(dat, aggregate(data.frame(Dat$weight), by=list(Dat$id, Dat$species), FUN=sum)[,3])
#colnames(dat)[9] = 'weight'
##preallocate for cluster size
#dat = cbind( dat, rep(NA, dim(dat)[1]) )
##get cluster weight by sample
#clustWeight = aggregate( data.frame(dat$weight), by=list(dat$id), FUN=sum )
##match up total sampled weight with species weights 
#for(cw in 1:dim(clustWeight)[1]){ dat[dat[,1]==clustWeight[cw, 1], dim(dat)[2]]=clustWeight[cw, 2] }
#colnames(dat)[10] = 'clustSize'


#add zeros/account for clusters
yearEff = unique(dat$year)
gearEff = unique(dat$gear)
portEff = unique(dat$portComplex)
qtrEff  = unique(dat$gear)
sppEff  = unique(dat$species)
#subset data
mct = '250'
place = c('CRS', 'MNT', 'MRO') 
gear = c('TWL', 'HKL') #'TWL' #
yer = '1982'#'1990'
#qtr = '2' #'4'

pdf(sprintf('compPlot%s.pdf', paste(c(paste(place,collapse='_'), paste(gear,collapse='_')), collapse='__')), 
	height=12
)
par(mar=c(5, 4, 9, 2) + 0.1)
spTune = 1
tune = 10
layout(matrix(c(rep(7,spTune),rep(1,tune),rep(2,tune),rep(3,tune),rep(8,spTune),rep(4,tune),rep(5,tune),rep(6,tune)), tune*3+spTune, 2)) #layout(matrix(c(1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,6), 3, 2))
par(mar=c(5, 4, 4, 2) + 0.1)
for(ger in gear){
	for(plc in place){
		#zero datai
		D = dat[dat$mcat==mct & dat$year==yer & dat$portComplex==plc & dat$gear==ger,] # & dat$qtr==qtr,]
		end = length(D$id)
		for(id in unique(D$id)){
			#
		        wid = which(D$id==id)
		        #
		        clst = D$clustSize[wid[1]]
		        port = D$portComplex[wid[1]]
		        gear = D$gear[wid[1]]
		        year = D$year[wid[1]]
		        qtr  = D$qtr[wid[1]]
			#       
		        for(sn in sppEff[!sppEff%in%D$spp[wid]]){
		                #
		                end = end + 1
		                #
				add = matrix(NA, ncol=10, nrow=1)
				colnames(add) = c('id', 'species', 'year', 'qtr', 'portComplex', 'gear', 'mcat', 'isLive', 'weight', 'clustSize')
				add[,'id'] = id
				add[,'species'] = sn
				add[,'year'] = year
		                add[,'qtr']  = qtr
		                add[,'portComplex'] = port
		                add[,'gear'] = gear
		                add[,'mcat'] = mct
				add[,'isLive'] = 'N'
		                add[,'weight'] = 0
				add[,'clustSize'] = clst
				#
				D = rbind(D, add)
			}
		}
		#UNOBSERVED DATA
		fill = 100 #prediction size for unobserves strata
		#
		mcOut = mclapply( qtrEff, FUN = function(q){
		#for(p in portEff){
			add = numeric(0)
			#for(g in gearEff){
			#for(q in qtrEff ){
			#for(y in yearEff){
			for(s in sppEff ){
			        #D$portComplex==p       	&
			        #D$gear==g       	&
			        #D$year==y       	&
			        wJoint = which(
			                D$qtr==q     	&
			                D$species==s
			        )
			        #
			        if( length(wJoint)==0 ){
			                #data grows by a single row with 0 weight
			                #end = end + 1
			                ##
			                #D$id[end]     = NA
			                #D$weight[end] = NA 
			                #D$gear[end]   = g
			                #D$year[end]   = y
			                #D$qtr[end]    = q
			                #D$species[end] = s
					#D$portComplex[end] = p
					#D$clustSize[end] = fill 
					##
					addd = matrix(NA, ncol=10, nrow=1)
			                colnames(addd) = c('id', 'species', 'year', 'qtr', 'portComplex', 'gear', 'mcat', 'isLive', 'weight', 'clustSize')
					#
			                addd[,'id'] = NA
			                addd[,'species'] = s
			                addd[,'year'] = yer
			                addd[,'qtr']  = q
			                addd[,'portComplex'] = plc
			                addd[,'gear'] = ger
			                addd[,'mcat'] = mct
			                addd[,'isLive'] = 'N'
			                addd[,'weight'] = NA
			                addd[,'clustSize'] = fill
					#
			                add = rbind(add, addd)
			        }
			}#}}}
			return( add )
		}, #D=D, gearEff=gearEff, yearEff=yearEff, qtrEff=qtrEff, sppEff=sppEff,
		mc.cores=4)
		mcOut = do.call(rbind, mcOut)
		D = rbind(D, mcOut)
		
		#
		bp = boxplot(as.numeric(D$weight)~D$species, plot=F)
		o = order(bp$stats[5,], decreasing=T)
		#
		off = 0.5
		howMany = 5#6 #8 #7 #6
		all = bp$names[o]
		who = head(bp$names[o], howMany)
		#who = who[c(1,2,4,5,3,6)]
		DAT = D[D$species%in%who, c('weight', 'species', 'qtr', 'clustSize')]
		DAT$weight = as.numeric(DAT$weight)
		DAT$clustSize = as.numeric(DAT$clustSize)
		#fit poisson, binomial, negative binomial, beta-binomial
		cores = 4#8
		#DIC, WAIC, MLIK
		#5675.25, 5840.56, -2864.01
		pOut = inla(weight~species, data=DAT, family='poisson', num.threads=cores, 
			control.compute=list(
				config=T,
				waic=T,
				dic=T
			)
		)
		#1301.51, 1302.19, -688.19
		nbOut = inla(weight~species, data=DAT, family='nbinomial', num.threads=cores, 
			control.compute=list(
				config=T,
				waic=T,
				dic=T
			)
		)
		#6759.86, 6939.74, -3406.01
		bOut  = inla(weight~species, data=DAT, family='binomial', Ntrials=DAT$clustSize, num.threads=cores, 
			control.compute=list(
				config=T,
				waic=T,
				dic=T
			)
		)
		#1261.00, 1261.30, -650.49
		bbOut = inla(weight~species, data=DAT, family='betabinomial', Ntrials=DAT$clustSize, num.threads=cores,
			control.compute=list(
				config=T,
				waic=T,
				dic=T
			)
		)
		#
		pOut  = inla.hyperpar(pOut) 
		bOut  = inla.hyperpar(bOut)
		nbOut = inla.hyperpar(nbOut)
		bbOut = inla.hyperpar(bbOut)
		
		#
		#SAMPLE
		#
		
		#
		M = 10^4
		
		#poisson
		#pHype = inla.hyperpar.sample(M, pOut)
		pPost = inla.posterior.sample(M, pOut)
		fString = 'Predictor:%d' #sprintf('Predictor:%%0%dd', nchar(max(grep('Predictor', rownames(pPost[[1]]$latent)))) ) #'Predictor:%03d'
		#
		pDist = matrix(NA, nrow=M, ncol=howMany)
		colnames(pDist) = who
		pHDI = list()
		#
		pBox = matrix(NA, nrow=howMany, ncol=10)#12)
		colnames(pBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')#, 'pLower', 'pUpper')
		rownames(pBox) = who
		#
		for(w in rev(who)){
			#
			where = which(DAT$species==w)[1]
			wSam = sapply(pPost, function(logSam){
				#
				idxStr = sprintf(fString, where)
				sam = exp( logSam[['latent']][idxStr,] )	
				#
				return( sam )
			})
			#
			pred = rpois(M, wSam)
			pDist[,w] = pred
			#spIntHDI = HDInterval:::hdi.density(bkde(sp[,s], range.x=c(0,1), canonical=T), credMass=prob, allowSplit=T)
			pred = pred[!is.na(pred)]
			pBox[w,1:8] = c(   
					mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975), 
					mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(wSam, 0.975)
			)
		}
		pDist = pDist/rowSums(pDist)
		for(w in who){ 
			#
			naDist = pDist[,w]
			naDist = naDist[!is.na(naDist)]
			pBox[w,9:10] = c(mean(naDist), median(naDist))#, quantile(pDist[,w], 0.025), quantile(pDist[,w], 0.975)) 
			#spIntHDI = HDInterval:::hdi.density(bkde(bDist[,w], range.x=c(0,1), canonical=T), credMass=0.95, allowSplit=T)
		        spIntHDI = HDInterval:::hdi.density(density(naDist, from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
		        pHDI[[w]] = matrix(spIntHDI[,], ncol=2)
		        colnames(pHDI[[w]]) = c('begin', 'end')
		}
		
		
		#binomial
		#bHype = inla.hyperpar.sample(M, bOut)
		bPost = inla.posterior.sample(M, bOut)
		#
		bDist = matrix(NA, nrow=M, ncol=howMany)
		colnames(bDist) = who
		bHDI = list()
		#
		bBox = matrix(NA, nrow=howMany, ncol=10)#12)
		colnames(bBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')#, 'pLower', 'pUpper')
		rownames(bBox) = who
		#
		for(w in rev(who)){
			#
			where = which(DAT$species==w)[1]
			wSam = sapply(bPost, function(logSam){
				#
				idxStr = sprintf(fString, where)
				#sam = DAT$clustSize[where]*inv.logit( logSam[['latent']][idxStr,] )
				sam = inv.logit( logSam[['latent']][idxStr,] )	
				#
				return( sam )
			})
			#
			pred = rbinom(M, DAT$clustSize[where], wSam)
			bDist[,w] = pred 
			bBox[w,1:8] = c(
					mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975), 
		                        mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(wSam, 0.975)
			)	
		}
		bDist = bDist/rowSums(bDist)
		for(w in who){
		        #
			naDist = bDist[,w]
		        naDist = naDist[!is.na(naDist)]
			bBox[w,9:10] = c(mean(naDist), median(naDist))#, quantile(bDist[,w], 0.025), quantile(bDist[,w], 0.975))
			#spIntHDI = HDInterval:::hdi.density(bkde(bDist[,w], range.x=c(0,1), canonical=T), credMass=0.95, allowSplit=T)
			spIntHDI = HDInterval:::hdi.density(density(naDist, from=0, to=1, bw=0.11), credMass=0.95, allowSplit=T)
			bHDI[[w]] = matrix(spIntHDI[,], ncol=2)
			colnames(bHDI[[w]]) = c('begin', 'end')
		}
		
		#negative binomial
		#nbHype = inla.hyperpar.sample(M, nbOut)
		nbPost = inla.posterior.sample(M, nbOut)
		nbHype = sapply(nbPost, function(x){x[[1]]})
		#
		nbDist = matrix(NA, nrow=M, ncol=howMany)
		colnames(nbDist) = who
		nbHDI = list()
		#
		nbBox = matrix(NA, nrow=howMany, ncol=10)#12)
		colnames(nbBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')#, 'pLower', 'pUpper')
		rownames(nbBox) = who
		#
		for(w in rev(who)){
			#
			where = which(DAT$species==w)[1]
			nbSam = sapply(nbPost, function(logSam){
				#
				idxStr = sprintf(fString, where)
				sam = exp( logSam[['latent']][idxStr,] )
				#
				return( sam )
			})
			#
			n = exp(nbHype)
			p = 1/(nbSam/n+1)
			nbpred = rnbinom(M, n, p)
			nbDist[,w] = nbpred
			#
			nbpred = nbpred[!is.na(nbpred)]
			nbBox[w,1:8] = c(
					mean(nbpred), median(nbpred), quantile(nbpred, 0.025), quantile(nbpred, 0.975), 
		                        mean(nbSam) , median(nbSam) , quantile(nbSam, 0.025) , quantile(nbSam, 0.975)
			)	
		}
		nbDist = nbDist/rowSums(nbDist)
		nbDist = nbDist[!is.na(nbDist[,1]),]
		for(w in who){
		        nbBox[w,9:10] = c(mean(nbDist[,w]), median(nbDist[,w])) #, quantile(nbDist[,w], 0.025), quantile(nbDist[,w], 0.975))
			#spIntHDI = HDInterval:::hdi.density(bkde(nbDist[,w], range.x=c(0,1), canonical=T), credMass=0.95, allowSplit=T)
			spIntHDI = HDInterval:::hdi.density(density(nbDist[,w], from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
			nbHDI[[w]] = matrix(spIntHDI[,], ncol=2)
			colnames(nbHDI[[w]]) = c('begin', 'end')	
		}
		
		#beta binomial
		#bbHype = inla.hyperpar.sample(M, bbOut)
		bbPost = inla.posterior.sample(M, bbOut)
		bbHype = sapply(bbPost, function(x){x[[1]]})
		#
		bbDist = matrix(NA, nrow=M, ncol=howMany)
		colnames(bbDist) = who
		bbHDI = list()
		#
		bbBox = matrix(NA, nrow=howMany, ncol=10)#12)
		colnames(bbBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')#, 'pLower', 'pUpper')
		rownames(bbBox) = who
		#
		for(w in rev(who)){
			#
			where = which(DAT$species==w)[1]
			wSam = sapply(bbPost, function(logSam){
				#
				idxStr = sprintf(fString, where)
				sam = inv.logit( logSam[['latent']][idxStr,] )
				#
				return( sam )
			})
			#
			mup   = wSam #inv.logit(bbSam[,1:P+1])
			rho   = inv.logit(bbHype) #inv.logit(bbSam[,1])
			alpha = mup*(1-rho)/rho
			beta  = (1-mup)/rho
			#
			p = rbeta(M, alpha, beta)
		        pred = rbinom(M, size=DAT$clustSize[where], prob=p)
			bbDist[,w] = pred
			#
			bbBox[w,1:8] = c(
					mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975), 
		                        mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(wSam, 0.975)
			)	
		}
		bbDist = bbDist/rowSums(bbDist)
		bbDist = bbDist[!is.na(bbDist[,1]),]
		for(w in who){
		        bbBox[w,9:10] = c(mean(bbDist[,w]), median(bbDist[,w])) #, quantile(nbDist[,w], 0.025), quantile(nbDist[,w], 0.975))
			#plot(bkde(bbDist[,w], range.x=c(0,1), canonical=T))
			#spIntHDI = HDInterval:::hdi.density(bkde(bbDist[,w], range.x=c(0,1), canonical=T, bandwidth=0.1), credMass=0.95, allowSplit=T)
			spIntHDI = HDInterval:::hdi.density(density(bbDist[,w], from=0, to=1, bw=0.13), credMass=0.95, allowSplit=T)
			bbHDI[[w]] = matrix(spIntHDI[,], ncol=2)
			colnames(bbHDI[[w]]) = c('begin', 'end')
			#	nbBox[w,9:12] = c(mean(nbDist[,w]), median(nbDist[,w]), spIntHDI[1], spIntHDI[2])
			#	print(spIntHDI)
		}
		
		#MSE
		comps = list()
		pM = c()
		bM = c()
		nbM = c()
		bbM = c()
		for(i in 1:howMany){ 
			#
			comps[[i]] = DAT$weight[DAT$species==who[i]]/DAT$clustSize[DAT$species==who[i]]
			comps[[i]] = comps[[i]][!is.na(comps[[i]])]
			#
			pM  = rbind(pM, pBox[i, 'pMean'])
			bM  = rbind(bM, bBox[i, 'pMean'])
			nbM = rbind(nbM, nbBox[i, 'pMean'])
			bbM = rbind(bbM, bbBox[i, 'pMean'])
		}
		#
		pMSE = getMSE(comps, pM)
		bMSE = getMSE(comps, bM)
		nbMSE = getMSE(comps, nbM)
		bbMSE = getMSE(comps, bbM)
		
		##
		##PLOT COUNTS
		##
		#
		###dev.new()
		##pdf('weightPlot.pdf')
		##plot(0, 0, ylim=c(0, 60), xlim=c(1-off, howMany+off), xlab='', ylab='Weight', xaxt='n', main='95% Predictive HDI Model Comparison')
		##axis(1, at=1:howMany, labels=who)
		##for(i in 1:howMany){
		##	#data
		##	weights = D$weight[D$species==who[i]]
		##	points(rep(i, length(weights)), weights, pch='_', cex=4)
		##	#poisson
		##	px = i-0.25
		##	segments(px, pBox[i,'lower'], px, pBox[i,'upper'], lwd=4, col='blue')
		##	points(px, pBox[i, 'mean'], pch=19, col='blue')
		##	#binomial
		##	bx = i-0.25+0.5/3*1
		##	segments(bx, bBox[i,'lower'], bx, bBox[i,'upper'], lwd=4, col='red')
		##	points(bx, bBox[i, 'mean'], pch=19, col='red')
		##	#negative binomial
		##	nbx = i-0.25+0.5/3*2
		##	segments(nbx, nbBox[i,'lower'], nbx, nbBox[i,'upper'], lwd=4, col='forestgreen')
		##	points(nbx, nbBox[i, 'mean'], pch=19, col='forestgreen')
		##	#beta binomial
		##	bbx = i-0.25+0.5/3*3
		##	segments(bbx, bbBox[i,'lower'], bbx, bbBox[i,'upper'], lwd=4, col='darkorange')
		##	points(bbx, bbBox[i, 'mean'], pch=19, col='darkorange')	
		##}
		###legend
		##legend('topright', legend=c('Poisson', 'Binomial', 'Negative Binomial', 'Beta-Binomial'), lwd=4, col=c('blue', 'red', 'forestgreen', 'darkorange'))
		##dev.off()
		
		#
		#PLOT PROPS
		#
		
		#
		#dev.new()
		#pdf(sprintf('compPlotQtr%s.pdf', qtr))
		plot(0, 0, ylim=c(0, 1.15), xlim=c(1-off, howMany+off), xlab='', ylab='Proportion', xaxt='n', main=sprintf('Port:%s Gear:%s', plc, ger)) #95% Predictive HDI Model Comparison')
		axis(1, at=1:howMany, labels=who)
		for(i in 1:howMany){
		       	#make spp comp
		       	comps = DAT$weight[DAT$species==who[i]]/DAT$clustSize[DAT$species==who[i]]
		       	points(rep(i, length(comps)), comps, pch='_', cex=4)
			#poisson
		        px = i-0.25
		        #segments(px, pBox[i,'pLower'], px, pBox[i,'pUpper'], lwd=4, col='blue')
		        for(j in 1:dim(pHDI[[who[i]]])[1]){
				segments(px, pHDI[[who[i]]][j,'begin'], px, pHDI[[who[i]]][j,'end'], lwd=4, col='blue')
			}
			points(px, pBox[i, 'pMean'], pch=19, col='blue')
			#binomial
			bx = i-0.25+0.5/3*1
			#segments(bx, bBox[i,'pLower'], bx, bBox[i,'pUpper'], lwd=4, col='red')
			for(j in 1:dim(bHDI[[who[i]]])[1]){
				segments(bx, bHDI[[who[i]]][j,'begin'], bx, bHDI[[who[i]]][j,'end'], lwd=4, col='red')
			}
			points(bx, bBox[i, 'pMean'], pch=19, col='red')
			#negative binomial
			nbx = i-0.25+0.5/3*2
			for(j in 1:dim(nbHDI[[who[i]]])[1]){
				segments(nbx, nbHDI[[who[i]]][j,'begin'], nbx, nbHDI[[who[i]]][j,'end'], lwd=4, col='forestgreen')
			}
			points(nbx, nbBox[i, 'pMean'], pch=19, col='forestgreen')
			#beta binomial
			bbx = i-0.25+0.5/3*3
			for(j in 1:dim(bbHDI[[who[i]]])[1]){
				segments(bbx, bbHDI[[who[i]]][j,'begin'], bbx, bbHDI[[who[i]]][j,'end'], lwd=4, col='darkorange')
			}
			points(bbx, bbBox[i, 'pMean'], pch=19, col='darkorange')
		}
		#legend
		#legend(4.5, 0.75, legend=c('Poisson', 'Binomial', 'Negative Binomial', 'Beta-Binomial'), lwd=4, col=c('blue', 'red', 'forestgreen', 'darkorange'))
		legend('top', legend=c('Poisson', 'Binomial', 'Negative Binomial', 'Beta-Binomial'), pch=19, col=c('blue', 'red', 'forestgreen', 'darkorange'), ncol=2, lwd=4)#horiz=T, x.intersp=0.1,
		#dev.off()
	}
}
mtext("95% Predictive HDI Model Comparison", side=3, line=-2, outer=TRUE, cex=1.5)
dev.off()

##
##pdf(sprintf('compVioplotQtr%s.pdf', qtr))
#vioplot(bbDist[,1], bbDist[,2], bbDist[,3], bbDist[,4], bbDist[,5], bbDist[,6],
#	names=colnames(bbDist),
#	ylim=c(0, 1.15),
#	col='grey',
#	drawRect=F
#)
#for(i in 1:howMany){
#	#make spp comp
#	comps = DAT$weight[DAT$species==who[i]]/DAT$clustSize[DAT$species==who[i]]
#	points(rep(i, length(comps)), comps, pch='_', cex=4)
#}
#title(
#	main='Beta-Binomial Posterior Predictive Species Compositions',
#        ylab='Proportion'
#)
##dev.off()





	#dz=1,           #default=0.75
        #diff.logdens=1  #default=10
        #h=1e-10#,
        ##keep=T,
        #restart=T      
#)
#out = inla(model, family="betabinomial", data=D, Ntrials=D$off, num.threads=cores,
#                control.inla=list(
#                        int.strategy='ccd',
#                        tolerance=1e-10,
#                        h=1e-10,
#                        lincomb.derived.only=F
#                ),
#                control.compute=list(
#                        config=T,
#                        mlik=T,
#                        dic=T,
#                        waic=T
#                ),
#                control.fixed = list(
#                        expand.factor.strategy='inla'
#                ),
#                control.mode = list(
#                        result=starter,
#                        restart=T
#                )
#        )
#

###
###where = D$species%in%head(bp$names[o], 10)
###dev.new();
###boxplot(D$weight[where] ~ D$species[where])#D$species%in%head(bp$names[o], 10)])
#
###
###MAKE FULL DATA
###
##
###zero data
##for(id in unique(D$id)){
##        #
##        wid = which(D$id==id)
##        #
##        off  = D$off[wid[1]]
##        port = D$port[wid[1]]
##        gear = D$gear[wid[1]]
##        year = D$year[wid[1]]
##        qtr  = D$qtr[wid[1]]
##        #       
##        for(sn in sppEff[!sppEff%in%D$spp[wid]]){
##                #
##                end = end + 1
##                #
##                D$id[end]   = id
##                D$off[end]  = off
##                D$port[end] = port
##                D$gear[end] = gear
##                D$year[end] = year
##                D$qtr[end]  = qtr
##                D$spp[end]  = sn
##                #
##                D$weight[end] = 0
##        }
##}
##
###
###UNOBSERVED DATA
###
##
###prediction sum cluster size 
##fill = 100
###
##if( length(portEff)>1 ){ pIt=portEff }else{ pIt=c(1) }
##if( length(gearEff)>1 ){ gIt=gearEff }else{ gIt=c(1) }
##if( length(yearEff)>1 ){ yIt=yearEff }else{ yIt=c(1) }
##if( length( qtrEff)>1 ){ qIt=qtrEff  }else{ qIt=c(1) }
##if( length( sppEff)>1 ){ sIt=sppEff  }else{ sIt=c(1) }
###
##for(p in pIt){
##for(g in gIt){
##for(q in qIt){
##for(y in yIt){
##for(s in sIt){
##        #
##        wJoint = which(
##                D$port==p       &
##                D$gear==g       &
##                D$year==y       &
##                D$qtr==q        &
##                D$spp==s
##        )
##        #
##        if( length(wJoint)==0 ){
##                #data grows by a single row with 0 weight
##                end = end + 1
##                #
##                D$id[end]     = NA
##                D$weight[end] = NA
##                D$off[end]    = fill
##                D$port[end]   = p
##                D$gear[end]   = g
##                D$year[end]   = y
##                D$qtr[end]    = q
##                D$spp[end]    = s
##
##        }
##
##}}}}
##}


