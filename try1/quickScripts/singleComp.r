rm(list=ls())

#
library(INLA)
library(boot)

#
#CLEAN DATA
#

#dat = read.table('1985_trwl_mcat250.csv', header=T, sep=',')
Dat = read.table('data85to90.csv', header=F, sep=',', stringsAsFactors=F)
colnames(Dat) = c('id', 'clust', 'species', 'weight', 'year', 'qtr', 'port', 'portComplex', 'gear', 'mcat', 'total', 'isLive')
#aggregate categories
dat = aggregate( data.frame(Dat$year, Dat$qtr, Dat$portComplex, Dat$gear, Dat$mcat, Dat$isLive), by=list(Dat$id, Dat$species), FUN=unique )
colnames(dat) = c('id', 'species', 'year', 'qtr', 'portComplex', 'gear', 'mcat', 'isLive')
#aggregate add weights such that there is one weight per species per sample (aggregate cluster samples)
dat = cbind(dat, aggregate(data.frame(Dat$weight), by=list(Dat$id, Dat$species), FUN=sum)[,3])
colnames(dat)[9] = 'weight'
#preallocate for cluster size
dat = cbind( dat, rep(NA, dim(dat)[1]) )
#get cluster weight by sample
clustWeight = aggregate( data.frame(dat$weight), by=list(dat$id), FUN=sum )
#match up total sampled weight with species weights 
for(cw in 1:dim(clustWeight)[1]){ dat[dat[,1]==clustWeight[cw, 1], dim(dat)[2]]=clustWeight[cw, 2] }
colnames(dat)[10] = 'clustSize'

#add zeros/account for clusters
yearEff = unique(dat$year)
gearEff = unique(dat$gear)
portEff = unique(dat$portComplex)
qtrEff  = unique(dat$gear)
sppEff  = unique(dat$species)
#subset data
mct = '250'
plc = 'MNT'
ger = 'TWL'
yer = '1990'
#zero data
D = dat[dat$mcat==mct & dat$year==yer & dat$portComplex==plc & dat$gear==ger,]
end = length(D$id)
for(id in unique(D$id)){
	#
        wid = which(D$id==id)
        #
        clst = D$clustSize[wid[1]]
        port = D$port[wid[1]]
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
#unobserved data


#
bp = boxplot(as.numeric(D$weight)~D$species, plot=F)
o = order(bp$stats[5,], decreasing=T)
#
off = 0.5
howMany = 6#7
who = head(bp$names[o], howMany)
DAT = D[D$species%in%who, c('weight', 'species', 'clustSize')]
DAT$weight = as.numeric(DAT$weight)
DAT$clustSize = as.numeric(DAT$clustSize)
#
cores = 48
#fit poisson, binomial, negative binomial, beta-binomial
pOut  = inla(weight~species, data=DAT, family='poisson', num.threads=cores, control.compute=list(config=T))
bOut  = inla(weight~species, data=DAT, family='binomial', num.thread=cores, Ntrials=DAT$clustSize, control.compute=list(config=T))
nbOut = inla(weight~species, data=DAT, family='nbinomial', num.thread=cores, control.compute=list(config=T))
bbOut = inla(weight~species, data=DAT, family='betabinomial', num.thread=cores, Ntrials=DAT$clustSize, control.compute=list(config=T))

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

#
#SAMPLE
#

#
M = 10^4
#poisson
pHype = inla.hyperpar.sample(M, pOut)
pPost = inla.posterior.sample(M, pOut)
#
pBox = matrix(NA, nrow=howMany, ncol=8)
colnames(pBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper')
rownames(pBox) = who
for(w in rev(who)){
	#
	where = which(DAT$species==w)[1]
	wSam = sapply(pPost, function(logSam){
		#
		idxStr = sprintf('Predictor:%03d', where)
		sam = exp( logSam[['latent']][idxStr,] )	
		#
		return( sam )
	})
	#
	pred = rpois(M, wSam)
	pBox[w,] = c(   
			mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975), 
			mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(wSam, 0.975)
	)	
}
#binomial
bHype = inla.hyperpar.sample(M, bOut)
bPost = inla.posterior.sample(M, bOut)
#
bBox = matrix(NA, nrow=howMany, ncol=8)
colnames(bBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper')
rownames(bBox) = who
for(w in rev(who)){
	#
	where = which(DAT$species==w)[1]
	wSam = sapply(bPost, function(logSam){
		#
		idxStr = sprintf('Predictor:%03d', where)
		#sam = DAT$clustSize[where]*inv.logit( logSam[['latent']][idxStr,] )
		sam = inv.logit( logSam[['latent']][idxStr,] )	
		#
		return( sam )
	})
	#
	pred = rbinom(M, DAT$clustSize[where], wSam)
	bBox[w,] = c(
			mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975), 
                        mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(wSam, 0.975)
	)	
}
##negative binomial
#nbHype = inla.hyperpar.sample(M, nbOut)
#nbPost = inla.posterior.sample(M, nbOut)
##
#nbBox = matrix(NA, nrow=howMany, ncol=4)
#colnames(nbBox) = c('mean', 'median', 'lower', 'upper')
#rownames(nbBox) = who
#for(w in rev(who)){
#	#
#	where = which(DAT$species==w)[1]
#	wSam = sapply(nbPost, function(logSam){
#		#
#		idxStr = sprintf('Predictor:%03d', where)
#		sam = log( logSam[['latent']][idxStr,] )
#		#
#		return( sam )
#	})
#	#NOTE: FIX THIS HACK
#	nbBox[w,] = c(mean(wSam[!is.na(wSam)]), median(wSam[!is.na(wSam)]), quantile(wSam[!is.na(wSam)], 0.025), quantile(wSam[!is.na(wSam)], 0.975))	
#}
##beta binomial
#bbHype = inla.hyperpar.sample(M, bbOut)
#bbPost = inla.posterior.sample(M, bbOut)
##
#bbBox = matrix(NA, nrow=howMany, ncol=4)
#colnames(bbBox) = c('mean', 'median', 'lower', 'upper')
#rownames(bbBox) = who
#for(w in rev(who)){
#	#
#	where = which(DAT$species==w)[1]
#	wSam = sapply(bbPost, function(logSam){
#		#
#		idxStr = sprintf('Predictor:%03d', where)
#		sam = log( logSam[['latent']][idxStr,] )
#		#
#		return( sam )
#	})
#	#NOTE: FIX THIS HACK
#	bbBox[w,] = c(mean(wSam[!is.na(wSam)]), median(wSam[!is.na(wSam)]), quantile(wSam[!is.na(wSam)], 0.025), quantile(wSam[!is.na(wSam)], 0.975))	
#}

#
#PLOT
#

dev.new()
plot(0, 0, ylim=c(0, 60), xlim=c(1-off, howMany+off), xlab='', ylab='', xaxt='n')
axis(1, at=1:howMany, labels=who)
for(i in 1:howMany){
	#data
	weights = D$weight[D$species==who[i]]
	points(rep(i, length(weights)), weights, pch='_', cex=4)
	#poisson
	px = i-0.25
	segments(px, pBox[i,'lower'], px, pBox[i,'upper'], lwd=3, col='blue')
	points(px, pBox[i, 'mean'], pch=20, col='blue')
	#binomial
	bx = i-0.25+0.5/3*1
	segments(bx, bBox[i,'lower'], bx, bBox[i,'upper'], lwd=3, col='red')
	points(bx, bBox[i, 'mean'], pch=20, col='red')
	##negative binomial
	#nbx = i-0.25+0.5/3*2
	#segments(nbx, nbBox[i,'lower'], nbx, nbBox[i,'upper'], lwd=3, col='darkorange')
	#points(nbx, nbBox[i, 'mean'], pch=20, col='darkorange')
}
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


