rm(list=ls())

#
library(INLA)
INLA:::inla.dynload.workaround()
library(boot)
library(vioplot)
library(parallel)
library(HDInterval)
library(KernSmooth)

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
#GLOBAL
#

#
cores = 40 #30 #48 #25 #15 #8 #4

#
#CLEAN DATA
#

#dat = read.table('1985_trwl_mcat250.csv', header=T, sep=',')
#Dat = read.table('data85to90.csv', header=F, sep=',', stringsAsFactors=F)
#Dat = read.table('data1983To1990.csv', header=T, sep=',', stringsAsFactors=F)
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

#add zeros/account for clusters
yearEff = unique(dat$year)
gearEff = unique(dat$gear)
portEff = unique(dat$portComplex)
qtrEff  = unique(dat$qtr)
sppEff  = unique(dat$species)
##subset data
mct = '250'
#plc = 'MNT'
#ger = 'HKL' #'TWL'
#yer = '1990'
##zero data
#D = dat[dat$mcat==mct & dat$year==yer & dat$portComplex==plc & dat$gear==ger,]
#D = dat[dat$mcat==mct & dat$portComplex==plc & dat$gear==ger,]
#D = dat[dat$mcat==mct & dat$gear==ger,]
D = dat[dat$mcat==mct,]
end = length(D$id)
#for(id in unique(D$id)){
#mcOut = mclapply( unique(D$id), FUN = function(id){
uid = unique(D$id)
U = length(uid)
bounds = seq(1, U, ceiling(length(uid)/cores))
luid = split(uid, bounds) #split(sample(uid, U, replace=F), bounds)
mcOut = mclapply( luid, FUN = function(vid){
	#
	addd = numeric(0)
	for(id in vid){
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
			#D = rbind(D, add)
			addd = rbind(addd, add)
		}
	}
	return(addd)
}, mc.cores=cores)
mcOut = do.call(rbind, mcOut)
D = rbind(D, mcOut)
##UNOBSERVED DATA
#fill = 100 #prediction size for unobserves strata
##mcOut = mclapply( qtrEff, FUN = function(q){	
#mcOut = mclapply( portEff, FUN = function(p){
#	add = numeric(0)
#	#for(p in portEff){
#	for(g in gearEff){
#	for(q in qtrEff ){
#	for(y in yearEff){
#	for(s in sppEff ){
#		wJoint = which(
#	        	D$portComplex==p       	&
#	        	D$gear==g       	&
#	        	D$year==y		&
#	        #wJoint = which(
#	                D$qtr==q		&
#	                D$species==s
#	        )
#	        #
#	        if( length(wJoint)==0 ){
#			#
#			addd = matrix(NA, ncol=10, nrow=1)
#	                colnames(addd) = c('id', 'species', 'year', 'qtr', 'portComplex', 'gear', 'mcat', 'isLive', 'weight', 'clustSize')
#			#
#	                addd[,'id'] = NA
#	                addd[,'species'] = s
#	                addd[,'year'] = y #yer
#	                addd[,'qtr']  = q
#	                addd[,'portComplex'] = p #plc
#	                addd[,'gear'] = g #ger
#	                addd[,'mcat'] = mct
#	                addd[,'isLive'] = 'N'
#	                addd[,'weight'] = NA
#	                addd[,'clustSize'] = fill
#			#
#	                add = rbind(add, addd)
#	        }
#	}}}}#}
#	return( add )
#}, mc.cores=cores)
#mcOut = do.call(rbind, mcOut)
#D = rbind(D, mcOut)

#
bp = boxplot(as.numeric(D$weight)~D$species, plot=F)
o = order(bp$stats[5,], decreasing=T)
#
off = 0.5
howMany = length(sppEff) #6 # 7
all = bp$names[o]
who = head(bp$names[o], howMany)
DAT = D[D$species%in%who, c('id', 'weight', 'species', 'portComplex', 'gear', 'year', 'qtr', 'clustSize')]
DAT$weight = as.numeric(DAT$weight)
DAT$clustSize = as.numeric(DAT$clustSize)
#build qtr|year & year|qtr
end = dim(DAT)[1]
eval(parse( text=sprintf('DAT$qGiven%s=matrix(NA, nrow=end, ncol=1)', yearEff) ))
eval(parse( text=sprintf('DAT$yGiven%s=matrix(NA, nrow=end, ncol=1)', qtrEff) ))
for( w in 1:end ){
	#
        qGy = sprintf('%s|%s', DAT$qtr[w], DAT$year[w])
        yGq = sprintf('%s|%s', DAT$year[w], DAT$qtr[w])
        #
	eval(parse( text=sprintf('DAT$qGiven%s[w]=qGy', DAT$year[w]) ))
        eval(parse( text=sprintf('DAT$yGiven%s[w]=yGq', DAT$qtr[w]) )) 
}
#build qtr:year
DAT$qGivenY = rep(NA, end)
for(y in yearEff){
        #
        eval(parse( text=sprintf('qwho = !is.na(DAT$qGiven%s)', y) ))
        eval(parse( text=sprintf('DAT$QGivenY[qwho] = DAT$qGiven%s[qwho]', y) ))
}

#
#MODEL
#

#dic, waic, mlik

##try year + qtr
#writeLines('FIXED MODEL')
#out = inla(weight~species + portComplex + gear + year + qtr, 
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)
##out = inla.hyperpar(out)
#print(summary(out))

##try year + f(qtr)
#writeLines('RANDOM QTR MODEL')
#out = inla(weight~species + portComplex + gear + year + f(qtr), 
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)
#out = inla.hyperpar(out)
#print(summary(out))

##try f(year) + qtr
#writeLines('RANDOM YEAR MODEL')
#out = inla(weight~species + portComplex + gear + f(year) + qtr, 
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)
#out = inla.hyperpar(out)
#print(summary(out))

##
#writeLines('BOTH RANDOM MODEL')
#out = inla(weight~species + portComplex + gear + f(year) + f(qtr), 
#       	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#	#control.inla=list(
#	#	strategy='gaussian',
#        #        int.strategy='eb',
#        #        tolerance=1e-10,
#        #        h=1e-10,
#        #        lincomb.derived.only=F
#	#),
#	control.compute=list(
#        	config=T,
#                waic=T,
#                dic=T
#       	),
#       	control.mode = list(
#               restart=T
#       	)
#)
##out = inla.hyperpar(out)
#print(summary(out))
#
##
#writeLines('BOTH RANDOM PLUS V MODEL')
#out = inla(weight~species + portComplex + gear + f(year) + f(qtr) + f(QGivenY), 
#       	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#	#control.inla=list(
#	#	 strategy='gaussian',
#        #        int.strategy='eb',
#        #        tolerance=1e-10,
#        #        h=1e-10,
#        #        lincomb.derived.only=F
#	#),
#	control.compute=list(
#        	config=T,
#                waic=T,
#                dic=T
#       	),
#       	control.mode = list(
#               restart=T
#       	)
#)
##out = inla.hyperpar(out)
#print(summary(out))

#
sdPrior = abs(rcauchy(10^6, 0, 10^3))#-2))
pPrior = log( (1/(sdPrior^2)) ) #[(1/(sdPrior^2))<10^6] )
pPrior = density(pPrior, n=10^4) #, from=0, to=10^6)
y = pPrior$y #[1:which(pPrior$y==max(pPrior$y))]=max(pPrior$y)
y[1:which(y==max(y))]=max(y)
pPriorTable = INLA:::inla.paste(c("table:", cbind(pPrior$x, y)))
#
writeLines('V MODEL')
out = inla(weight~species + portComplex + gear + 
	f(QGivenY, model='iid', hyper=list(prec=list(prior=pPriorTable))), 
       	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores, #verbose=T,
	control.inla=list(
		#strategy='gaussian',	
        	diagonal=0.01,
		int.strategy='eb', #'ccd', #'eb',
		tolerance=1e-10,
                h=1e-10,
                lincomb.derived.only=F
	),
	control.compute=list(
        	config=T,
                waic=T,
                dic=T
       	),
       	control.mode = list(
               restart=T
       	)
)
writeLines('Hype Model')
out = inla.hyperpar(out)
print(summary(out))



##
#writeLines('BOTH RANDOM PLUS Vm MODEL')
#out = inla(weight~species + portComplex + gear + f(year) + f(qtr) + f(qGiven1985) + f(qGiven1986) + f(qGiven1987) + f(qGiven1988) + f(qGiven1989) + f(qGiven1990),
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	#control.inla=list(
#        #        strategy='gaussian',
#        #        int.strategy='eb',
#        #        tolerance=1e-10,
#        #        h=1e-10,
#        #        lincomb.derived.only=F
#        #),
#	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)
#out = inla.hyperpar(out)
#print(summary(out))
#
##
#writeLines('BOTH RANDOM PLUS Veta MODEL')
#out = inla(weight~species + portComplex + gear + f(year) + f(qtr) + f(yGiven1) + f(yGiven2) + f(yGiven3) + f(yGiven4),
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#	#control.inla=list(
#        #        strategy='gaussian',
#        #        int.strategy='eb',
#        #        tolerance=1e-10,
#        #        h=1e-10,
#        #        lincomb.derived.only=F
#        #),
#	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)
#out = inla.hyperpar(out)
#print(summary(out))

#
#SAMPLE
#

#
M = 10^3

#
writeLines('SAMPLE\n')
postQY = inla.posterior.sample(M, out)
hypeQY = sapply(postQY, function(x){x[[1]]})
rho    = inv.logit(hypeQY)
#
distQY = matrix(NA, nrow=M, ncol=howMany)
colnames(distQY) = who
#hdiQY = list()
#
#boxQY = matrix(NA, nrow=howMany, ncol=10)
#colnames(boxQY) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')
#rownames(boxQY) = who
##
#

writeLines('MSE')

#registerDoParallel(cores=length(portEff))
#preds = foreach( p=portEff )%dopar%{
#preds = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), mse=numeric(), n=numeric(), stringsAsFactors=F)
pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), mse=numeric(), n=numeric(), stringsAsFactors=F)
end = 1
for(p in portEff){
        #
        #end  = 1 
        #pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), mse=numeric(), n=numeric(), stringsAsFactors=F) #weightPredHDI=numeric(), weightPredCI=numeric(), propPredHDI=numeric(), propPredCI
        #
        for(g in gearEff){#[1]){
        for(q in qtrEff ){#[1] ){
        for(y in yearEff){#[1]){
                #observations
                W = DAT[DAT$portComplex==p & DAT$gear==g & DAT$qtr==q & DAT$year==y,]
                #avoid the case where no data exists
                if( dim(W)[1]>0 ){
                        #cluster info
                        #aggregate(W$weight, by=list(W$sampleNumber, W$clusterNumber)
                        clustSize = aggregate(W$weight, by=list(W$id), FUN=sum)
                        #colnames(clustSize) = c('sampleNumber', 'clusterNumber', 'size')
                        colnames(clustSize) = c('sampleNumber', 'size')
                        ##predictive distribution samples by species in the p, g, q, y stratum
                        #distQY = matrix(NA, nrow=M, ncol=howMany)
                        #colnames(distQY) = who
                        #predictive distribution samples by species in the p, g, q, y stratum
                        distQY = c() #matrix(NA, nrow=M, ncol=howMany)
                        sNome  = c()
                        sCount = 1
                        #make predictive distributions
                        for(w in rev(who)){
                                #
                                where = which(
                                        DAT$portComplex==p      &
                                        DAT$gear==g             &
                                        DAT$qtr==q              &
                                        DAT$year==y             &
                                        DAT$species==w
                                )[1]
                                if( !is.na(where) ){
                                        mup   = sapply(postQY, function(logSam){
                                                #
                                                idxStr = sprintf('Predictor:%05d', where)
                                                sam = inv.logit( logSam[['latent']][idxStr,] )
                                                #
                                                return( sam )
                                        })
                                        #mup   = wSam 
                                        alpha = mup*(1-rho)/rho
                                        beta  = (1-mup)/rho
                                        #
                                        pp = rbeta(M, alpha, beta)
                                        #distQY[,w] = rbinom(M, size=DAT$clustSize[where], prob=pp)
                                        distQY = cbind(distQY, rbinom(M, size=DAT$clustSize[where], prob=pp))
                                        colnames(distQY)[sCount] = w
                                        sNome = c(sNome, w)
                                        sCount = sCount + 1
                                }
                        }
                        distQY = distQY/rowSums(distQY)
                        distQY = t(t(distQY[!is.na(distQY[,1]),]))
                        colnames(distQY) = sNome
                        predM  = colMeans(distQY)
                        #
                        #
                        for(s in rev(who)){
                                #print(s)
                                #spIntHDI = hdi(sp[,s], credMass=prob)  
                                #spIntHDI = hdi(density(sp[,s], from=0, to=1), credMass=prob, allowSplit=T)
                                #spIntHDI = HDInterval:::hdi.density(bkde(sp[,s], range.x=c(0,1), canonical=T), credMass=prob, allowSplit=T)
                                #spIntCI = t(quantile(sp[,s], c((1-prob)/2, prob+(1-prob)/2)))
                                #
                                #lpIntHDI = HDInterval:::hdi.density(bkde(lp[,s], range.x=c(0,max(lp[,s])), canonical=T), credMass=prob, allowSplit=T)
                                #lpIntHDI = hdi(density(lp[,s], from=0, to=max(lp[,s])), credMass=prob, allowSplit=T)
                                #lpIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
                                #
                                WS = W[W$species==s,]
                                #print( gridSize )
                                #if( dim(WS)[1]>0 ){
                                if( !all(is.na(WS$id)) ){
                                        #make comps
                                        cp = c()
                                        #wp = c()
                                        for(sam in WS$id[!is.na(WS$id)]){
                                                cp = c(cp, WS[WS$id==sam, 'weight']/clustSize[clustSize$sampleNumber==sam,'size'])
                                                #for(clust in WS[WS$sampleNumber==sam, 'clusterNumber']){
                                                #               cp = c(cp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight']/clustSize[clustSize$sampleNumber==sam & clustSize$clusterNumber==clust,'size'])
                                                #               wp = c(wp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight'])
                                                #       }
                                        }
                                        #
                                        mSqErr = mean( (cp-predM[s])^2 )
                                        ##spHDI
                                        #inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other intervals
                                        #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
                                        #cpHdiMean = mean(inOut)
                                        ##
                                        ##spCI
                                        #inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other intervals
                                        #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
                                        #cpCiMean = mean(inOut)
                                        ##
                                        ##lpHDI
                                        #inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
                                        #for(i in 1:dim(lpIntHDI)[1]){ inOut=findInterval(wp, lpIntHDI[i,])==1 | inOut }
                                        #wpHdiMean = mean(inOut)
                                        ##wpHdiMean = 0
                                        ###
                                        ##lpCI
                                        #inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
                                        #for(i in 1:dim(lpIntCI)[1]){ inOut=findInterval(wp, lpIntCI[i,])==1 | inOut }
                                        #wpCiMean = mean(inOut)
                                        ##
                                        hand = c(as.character(p), as.character(g), q, y, as.character(s), mSqErr, length(cp)) #wpHdiMean, wpCiMean, cpHdiMean, cpCiMean, length(cp))
                                        pred[end,] = hand
                                        end = end+1
                                }
                        }
                }
        }}}
        #return(pred)
}
##
#preds = do.call(rbind, preds)
#preds$mse = as.numeric(preds$mse)
#preds$n   = as.numeric(preds$n)
##
#mse = sum(preds$mse[!is.na(preds$mse)]*preds$n[!is.na(preds$mse)])/sum(preds$n[!is.na(preds$mse)])
#mse = sum(preds$mse*preds$n)/sum(preds$n)
pred$mse = as.numeric(pred$mse)
pred$n = as.numeric(pred$n)
#mse = sum(pred$mse*pred$n)/sum(pred$n)
mse = sum(pred$mse[!is.na(pred$mse)]*pred$n[!is.na(pred$mse)])/sum(pred$n[!is.na(pred$mse)])
#
writeLines(sprintf('MSE: %1.7f\n', mse)) #mseQY))
metrics = t(c(out$mlik[1], out$waic$waic, out$dic$dic, mse))
colnames(metrics) = c('mlik', 'waic', 'dic', 'mse')
write.csv(format(metrics, scientific=T, digits=10), file="./aggMetricsCauchy.csv", row.names=F, quote=F)




















#
##
#M = 10^4
#
##
#writeLines('SAMPLE\n')
#postQY = inla.posterior.sample(M, out)
#hypeQY = sapply(postQY, function(x){x[[1]]})
#rho    = inv.logit(hypeQY)
##
#distQY = matrix(NA, nrow=M, ncol=howMany)
#colnames(distQY) = who
##hdiQY = list()
##
##boxQY = matrix(NA, nrow=howMany, ncol=10)
##colnames(boxQY) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')
##rownames(boxQY) = who
###
##
#
#writeLines('MSE')
#
##registerDoParallel(cores=length(portEff))
##preds = foreach( p=portEff )%dopar%{
#preds = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), mse=numeric(), n=numeric(), stringsAsFactors=F)
#pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), mse=numeric(), n=numeric(), stringsAsFactors=F)
#for(p in portEff){
#        ##
#        #end  = 1 
#        #pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), mse=numeric(), n=numeric(), stringsAsFactors=F) #weightPredHDI=numeric(), weightPredCI=numeric(), propPredHDI=numeric(), propPredCI=numeric(), n=numeric(), stringsAsFactors=F)
#        ##
#        for(g in gearEff){#[1]){
#        for(q in qtrEff ){#[1] ){
#        for(y in yearEff){#[1]){
#                #observations
#		W = DAT[DAT$portComplex==p & DAT$gear==g & DAT$qtr==q & DAT$year==y,]
#                #avoid the case where no data exists
#                if( dim(W)[1]>0 ){
#			#cluster info
#			#aggregate(W$weight, by=list(W$sampleNumber, W$clusterNumber)
#                        clustSize = aggregate(W$weight, by=list(W$id), FUN=sum)
#                        #colnames(clustSize) = c('sampleNumber', 'clusterNumber', 'size')
#                        colnames(clustSize) = c('sampleNumber', 'size')
#			#predictive distribution samples by species in the p, g, q, y stratum
#			distQY = matrix(NA, nrow=M, ncol=howMany)
#			colnames(distQY) = who
#			#make predictive distributions
#			for(w in rev(who)){
#			        #
#			        where = which( 
#					DAT$portComplex==p 	& 
#					DAT$gear==g 		& 
#					DAT$qtr==q 		& 
#					DAT$year==y 		& 
#					DAT$species==w
#				)[1]
#			        mup   = sapply(postQY, function(logSam){
#			                #
#			                idxStr = sprintf('Predictor:%06d', where)
#			                sam = inv.logit( logSam[['latent']][idxStr,] )
#			                #
#			                return( sam )
#			        })
#			        #mup   = wSam 
#			        alpha = mup*(1-rho)/rho
#			        beta  = (1-mup)/rho
#			        #
#			        pp = rbeta(M, alpha, beta)
#			        distQY[,w] = rbinom(M, size=DAT$clustSize[where], prob=pp)
#			}
#			distQY = distQY/rowSums(distQY)
#			distQY = distQY[!is.na(distQY[,1]),]
#			predM  = colMeans(distQY) 
#                        #
#                        for(s in rev(who)){
#                                #print(s)
#                                #spIntHDI = hdi(sp[,s], credMass=prob)  
#                                #spIntHDI = hdi(density(sp[,s], from=0, to=1), credMass=prob, allowSplit=T)
#                                #spIntHDI = HDInterval:::hdi.density(bkde(sp[,s], range.x=c(0,1), canonical=T), credMass=prob, allowSplit=T)
#                                #spIntCI = t(quantile(sp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#                                #
#                                #lpIntHDI = HDInterval:::hdi.density(bkde(lp[,s], range.x=c(0,max(lp[,s])), canonical=T), credMass=prob, allowSplit=T)
#                                #lpIntHDI = hdi(density(lp[,s], from=0, to=max(lp[,s])), credMass=prob, allowSplit=T)
#                                #lpIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#                                #
#                                WS = W[W$species==s,]
#                                #print( gridSize )
#                                #if( dim(WS)[1]>0 ){
#				if( !all(is.na(WS$id)) ){
#                                        #make comps
#                                        cp = c()
#                                        #wp = c()
#                                        for(sam in WS$id[!is.na(WS$id)]){
#						cp = c(cp, WS[WS$id==sam, 'weight']/clustSize[clustSize$sampleNumber==sam,'size'])
#                                                #for(clust in WS[WS$sampleNumber==sam, 'clusterNumber']){
#                                                #               cp = c(cp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight']/clustSize[clustSize$sampleNumber==sam & clustSize$clusterNumber==clust,'size'])
#                                                #               wp = c(wp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight'])
#                                                #       }
#                                        }
#                                       	#
#					mSqErr = mean( (cp-predM[s])^2 )
#					##spHDI
#                                        #inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other intervals
#                                        #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
#                                        #cpHdiMean = mean(inOut)
#                                        ##
#                                        ##spCI
#                                        #inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other intervals
#                                        #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
#                                        #cpCiMean = mean(inOut)
#                                        ##
#                                        ##lpHDI
#                                        #inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
#                                        #for(i in 1:dim(lpIntHDI)[1]){ inOut=findInterval(wp, lpIntHDI[i,])==1 | inOut }
#                                        #wpHdiMean = mean(inOut)
#                                        ##wpHdiMean = 0
#                                        ###
#                                        ##lpCI
#                                        #inOut = rep(0, length(wp))    #check if in interval, convert to proper bool, and unite with other intervals
#                                        #for(i in 1:dim(lpIntCI)[1]){ inOut=findInterval(wp, lpIntCI[i,])==1 | inOut }
#                                        #wpCiMean = mean(inOut)
#                                        ##
#                                        hand = c(as.character(p), as.character(g), q, y, as.character(s), mSqErr, length(cp)) #wpHdiMean, wpCiMean, cpHdiMean, cpCiMean, length(cp))
#	                                pred[end,] = hand        
#					end = end+1
#                                }
#                        }
#                }
#        }}}
#        #return(pred)
#}
###
##preds = do.call(rbind, preds)
##preds$mse = as.numeric(preds$mse)
##preds$n   = as.numeric(preds$n)
###
##mse = sum(preds$mse*preds$n)/sum(preds$n)
#pred$mse = as.numeric(pred$mse)
#pred$n = as.numeric(pred$n)
##mse = sum(pred$mse*pred$n)/sum(pred$n)
#mse = sum(pred$mse[!is.na(pred$mse)]*pred$n[!is.na(pred$mse)])/sum(pred$n[!is.na(pred$mse)])

##writeLines(sprintf('MSE: %1.7f\n', mseQY))
#metrics = t(c(out$mlik[1], out$waic$waic, out$dic$dic)) #, mse))
#colnames(metrics) = c('mlik', 'waic', 'dic') #, 'mse')
#write.csv(format(metrics, scientific=T, digits=10), file="./aggMetrics.csv-2try", row.names=F, quote=F)




##for(p in portEff){
##for(g in gearEff){
##for(q in qtrEff ){
##for(y in yearEff){
#for(w in rev(who)){
##distQY = mclapply( rev(who), FUN = function(w){
#       	#
#       	where = which(DAT$species==w)[1]
#       	wSam  i sapply(postQY, function(logSam){
#       	        #
#       	        idxStr = sprintf('Predictor:%06d', where)
#       	        sam = inv.logit( logSam[['latent']][idxStr,] )
#       	        #
#       	        return( sam )
#       	})
#       	#
#       	mup   = wSam
#       	rho   = inv.logit(hypeQY)
#       	alpha = mup*(1-rho)/rho
#       	beta  = (1-mup)/rho
#       	#
#	p = rbeta(M, alpha, beta)
#        pred = rbinom(M, size=DAT$clustSize[where], prob=p)
#	distQY[,w] = pred
#	##
#	#boxQY[w,1:8] = c(
#	#	mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975),
#	#	mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(pred, 0.975)
#	#)
#}
##	return( pred )
##}, mc.cores=cores)
##distQY = do.call(cbind, distQY)
##
#distQY = distQY/rowSums(distQY)
#distQY = distQY[!is.na(distQY[,1]),]
##for(w in who){
##	boxQY[w,9:10] = c(mean(distQY[,w]), median(distQY[,w]))	
##	spIntHDI = HDInterval:::hdi.density(density(distQY[,w], from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
##	hdiQY[[w]] = matrix(spIntHDI[,], ncol=2)
##	colnames(hdiQY[[w]]) = c('begin', 'end')
##}
#
##
##MSE
##
#
#comps = list()
#mQY = c()
##mqY = c()
##mQy = c()
#for(i in 1:howMany){ 
#       	#
#       	comps[[i]] = DAT$weight[DAT$species==who[i]]/DAT$clustSize[DAT$species==who[i]]
#	comps[[i]] = comps[[i]][!is.na(comps[[i]])]
#	#
#	mQY = rbind(mQY, mean(distQY[,i]))
#	#mqY = rbind(mqY, boxqY[i, 'pMean'])
#	#mQy = rbind(mQy, boxQy[i, 'pMean'])
#}
##
#mseQY = getMSE(comps, mQY)
#writeLines(sprintf('MSE: %f\n', mseQY))
##mseqY = getMSE(comps, mqY)
##mseQy = getMSE(comps, mQy)



##
#writeLines('eta SAMPLE\n')
#postqY = inla.posterior.sample(M, outqY)
#hypeqY = sapply(postqY, function(x){x[[1]]})
##
#distqY = matrix(NA, nrow=M, ncol=howMany)
#colnames(distqY) = who
#hdiqY = list()
##
#boxqY = matrix(NA, nrow=howMany, ncol=10)
#colnames(boxqY) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')
#rownames(boxqY) = who
##
#for(w in rev(who)){
#       	#
#       	where = which(DAT$species==w)[1]
#       	wSam = sapply(postsY, function(logSam){
#       	        #
#       	        idxStr = sprintf('Predictor:%03d', where)
#       	        sam = inv.logit( logSam[['latent']][idxStr,] )
#       	        #
#       	        return( sam )
#       	})
#       	#
#       	mup   = wSam
#       	rho   = inv.logit(hypeqY)
#       	alpha = mup*(1-rho)/rho
#       	beta  = (1-mup)/rho
#       	#
#	p = rbeta(M, alpha, beta)
#        pred = rbinom(M, size=DAT$clustSize[where], prob=p)
#	distqY[,w] = pred
#	#
#	boxqY[w,1:8] = c(
#		mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975),
#		mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(pred, 0.975)
#	)
#}
#distqY = distqY/rowSums(distqY)
#distqY = distqY[!is.na(distqY[,1]),]
#for(w in who){
#	boxqY[w,9:10] = c(mean(distqY[,w]), median(distqY[,w]))	
#	spIntHDI = HDInterval:::hdi.density(density(distqY[,w], from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
#	hdiqY[[w]] = matrix(spIntHDI[,], ncol=2)
#	colnames(hdiqY[[w]]) = c('begin', 'end')
#}
#
##
##MSE
##
#
#comps = list()
#mQY = c()
#mqY = c()
##mQy = c()
#for(i in 1:howMany){ 
#       	#
#       	comps[[i]] = DAT$weight[DAT$species==who[i]]/DAT$clustSize[DAT$species==who[i]]
#	comps[[i]] = comps[[i]][!is.na(comps[[i]])]
#	#
#	mQY = rbind(mQY, boxQY[i, 'pMean'])
#	mqY = rbind(mqY, boxqY[i, 'pMean'])
#	#mQy = rbind(mQy, boxQy[i, 'pMean'])
#}
##
#mseQY = getMSE(comps, mQY)
##mseqY = getMSE(comps, mqY)
##mseQy = getMSE(comps, mQy)







##try f(year:qtr)
##weight~species + portComplex + gear + f(QGivenY): 103261.82, 103200.98, -69360.58
#writeLines('FULL MODEL\n')
#outQY = inla(weight~species + f(QGivenY), 
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)

##try f(year:1) + f(year:2) + f(year:3) + f(year:4)
#writeLines('eta MODEL\n')
#outqY = inla(weight~species + portComplex + gear + f(yGiven1) + f(yGiven2) + f(yGiven3) + f(yGiven4), 
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)

##try f(1985:qtr) + f(1986:qtr) + f(1987:qtr) + f(1988:qtr) + f(1989:qtr) + f(1990:qtr)
#writeLines('m MODEL\n')
#outQy = inla(weight~species + portComplex + gear + f(qGiven1985) + f(qGiven1986) + f(qGiven1987) + f(qGiven1988) + f(qGiven1989) + f(qGiven1990), 
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)

###try year:qtr
##writeLines('FIXED INTERACTION MODEL\n')
##outFI = inla(weight~species + portComplex + gear + QGivenY, 
##	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
##       	control.compute=list(
##        	config=T,
##               	waic=T,
##               	dic=T
##       	),
##	control.mode = list(
##        	restart=T
##	)
##)

##try year + qtr
#writeLines('FIXED MODEL\n')
#outF = inla(weight~species + portComplex + gear + year + qtr, 
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)

##try f(year) + f(qtr)
##weight~species + portComplex + gear + f(year) + f(qtr): 102214.97, 102164.61, -68632.82
#writeLines('RANDOM MODEL\n')
#outR = inla(weight~species + f(year) + f(qtr), 
#	Ntrials=DAT$clustSize, family='betabinomial', data=DAT, num.threads=cores,
#       	control.compute=list(
#        	config=T,
#               	waic=T,
#               	dic=T
#       	),
#	control.mode = list(
#        	restart=T
#	)
#)

##1261.00, 1261.30, -650.49
#bbOut = inla(weight~species, data=DAT, family='betabinomial', Ntrials=DAT$clustSize, num.threads=cores,
#	control.compute=list(
#		config=T,
#		waic=T,
#		dic=T
#	)
#)
##
#pOut  = inla.hyperpar(pOut) 
#bOut  = inla.hyperpar(bOut)
#nbOut = inla.hyperpar(nbOut)
#bbOut = inla.hyperpar(bbOut)
#
##
##SAMPLE
##
#
##
#M = 10^4
#
##poisson
##pHype = inla.hyperpar.sample(M, pOut)
#pPost = inla.posterior.sample(M, pOut)
##
#pDist = matrix(NA, nrow=M, ncol=howMany)
#colnames(pDist) = who
#pHDI = list()
##
#pBox = matrix(NA, nrow=howMany, ncol=10)#12)
#colnames(pBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')#, 'pLower', 'pUpper')
#rownames(pBox) = who
##
#for(w in rev(who)){
#	#
#	where = which(DAT$species==w)[1]
#	wSam = sapply(pPost, function(logSam){
#		#
#		idxStr = sprintf('Predictor:%03d', where)
#		sam = exp( logSam[['latent']][idxStr,] )	
#		#
#		return( sam )
#	})
#	#
#	pred = rpois(M, wSam)
#	pDist[,w] = pred
#	#spIntHDI = HDInterval:::hdi.density(bkde(sp[,s], range.x=c(0,1), canonical=T), credMass=prob, allowSplit=T)
#	pBox[w,1:8] = c(   
#			mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975), 
#			mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(wSam, 0.975)
#	)
#}
#pDist = pDist/rowSums(pDist)
#for(w in who){ 
#	#
#	pBox[w,9:10] = c(mean(pDist[,w]), median(pDist[,w]))#, quantile(pDist[,w], 0.025), quantile(pDist[,w], 0.975)) 
#	#spIntHDI = HDInterval:::hdi.density(bkde(bDist[,w], range.x=c(0,1), canonical=T), credMass=0.95, allowSplit=T)
#        spIntHDI = HDInterval:::hdi.density(density(pDist[,w], from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
#        pHDI[[w]] = matrix(spIntHDI[,], ncol=2)
#        colnames(pHDI[[w]]) = c('begin', 'end')
#}
#
#
##binomial
##bHype = inla.hyperpar.sample(M, bOut)
#bPost = inla.posterior.sample(M, bOut)
##
#bDist = matrix(NA, nrow=M, ncol=howMany)
#colnames(bDist) = who
#bHDI = list()
##
#bBox = matrix(NA, nrow=howMany, ncol=10)#12)
#colnames(bBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')#, 'pLower', 'pUpper')
#rownames(bBox) = who
##
#for(w in rev(who)){
#	#
#	where = which(DAT$species==w)[1]
#	wSam = sapply(bPost, function(logSam){
#		#
#		idxStr = sprintf('Predictor:%03d', where)
#		#sam = DAT$clustSize[where]*inv.logit( logSam[['latent']][idxStr,] )
#		sam = inv.logit( logSam[['latent']][idxStr,] )	
#		#
#		return( sam )
#	})
#	#
#	pred = rbinom(M, DAT$clustSize[where], wSam)
#	bDist[,w] = pred 
#	bBox[w,1:8] = c(
#			mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975), 
#                        mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(wSam, 0.975)
#	)	
#}
#bDist = bDist/rowSums(bDist)
#for(w in who){
#        #
#	bBox[w,9:10] = c(mean(bDist[,w]), median(bDist[,w]))#, quantile(bDist[,w], 0.025), quantile(bDist[,w], 0.975))
#	#spIntHDI = HDInterval:::hdi.density(bkde(bDist[,w], range.x=c(0,1), canonical=T), credMass=0.95, allowSplit=T)
#	spIntHDI = HDInterval:::hdi.density(density(bDist[,w], from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
#	bHDI[[w]] = matrix(spIntHDI[,], ncol=2)
#	colnames(bHDI[[w]]) = c('begin', 'end')
#}
#
##negative binomial
##nbHype = inla.hyperpar.sample(M, nbOut)
#nbPost = inla.posterior.sample(M, nbOut)
#nbHype = sapply(nbPost, function(x){x[[1]]})
##
#nbDist = matrix(NA, nrow=M, ncol=howMany)
#colnames(nbDist) = who
#nbHDI = list()
##
#nbBox = matrix(NA, nrow=howMany, ncol=10)#12)
#colnames(nbBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')#, 'pLower', 'pUpper')
#rownames(nbBox) = who
##
#for(w in rev(who)){
#	#
#	where = which(DAT$species==w)[1]
#	nbSam = sapply(nbPost, function(logSam){
#		#
#		idxStr = sprintf('Predictor:%03d', where)
#		sam = exp( logSam[['latent']][idxStr,] )
#		#
#		return( sam )
#	})
#	#
#	n = exp(nbHype)
#	p = 1/(nbSam/n+1)
#	nbpred = rnbinom(M, n, p)
#	nbDist[,w] = nbpred
#	#
#	nbBox[w,1:8] = c(
#			mean(nbpred), median(nbpred), quantile(nbpred, 0.025), quantile(nbpred, 0.975), 
#                        mean(nbSam) , median(nbSam) , quantile(nbSam, 0.025) , quantile(nbSam, 0.975)
#	)	
#}
#nbDist = nbDist/rowSums(nbDist)
#nbDist = nbDist[!is.na(nbDist[,1]),]
#for(w in who){
#        nbBox[w,9:10] = c(mean(nbDist[,w]), median(nbDist[,w])) #, quantile(nbDist[,w], 0.025), quantile(nbDist[,w], 0.975))
#	#spIntHDI = HDInterval:::hdi.density(bkde(nbDist[,w], range.x=c(0,1), canonical=T), credMass=0.95, allowSplit=T)
#	spIntHDI = HDInterval:::hdi.density(density(nbDist[,w], from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
#	nbHDI[[w]] = matrix(spIntHDI[,], ncol=2)
#	colnames(nbHDI[[w]]) = c('begin', 'end')	
#}
#
##beta binomial
##bbHype = inla.hyperpar.sample(M, bbOut)
#bbPost = inla.posterior.sample(M, bbOut)
#bbHype = sapply(bbPost, function(x){x[[1]]})
##
#bbDist = matrix(NA, nrow=M, ncol=howMany)
#colnames(bbDist) = who
#bbHDI = list()
##
#bbBox = matrix(NA, nrow=howMany, ncol=10)#12)
#colnames(bbBox) = c('mean', 'median', 'lower', 'upper', 'mMean', 'mMedian', 'mLower', 'mUpper', 'pMean', 'pMedian')#, 'pLower', 'pUpper')
#rownames(bbBox) = who
##
#for(w in rev(who)){
#	#
#	where = which(DAT$species==w)[1]
#	wSam = sapply(bbPost, function(logSam){
#		#
#		idxStr = sprintf('Predictor:%03d', where)
#		sam = inv.logit( logSam[['latent']][idxStr,] )
#		#
#		return( sam )
#	})
#	#
#	mup   = wSam #inv.logit(bbSam[,1:P+1])
#	rho   = inv.logit(bbHype) #inv.logit(bbSam[,1])
#	alpha = mup*(1-rho)/rho
#	beta  = (1-mup)/rho
#	#
#	p = rbeta(M, alpha, beta)
#        pred = rbinom(M, size=DAT$clustSize[where], prob=p)
#	bbDist[,w] = pred
#	#
#	bbBox[w,1:8] = c(
#			mean(pred), median(pred), quantile(pred, 0.025), quantile(pred, 0.975), 
#                        mean(wSam), median(wSam), quantile(wSam, 0.025), quantile(wSam, 0.975)
#	)	
#}
#bbDist = bbDist/rowSums(bbDist)
#bbDist = bbDist[!is.na(bbDist[,1]),]
#for(w in who){
#        bbBox[w,9:10] = c(mean(bbDist[,w]), median(bbDist[,w])) #, quantile(nbDist[,w], 0.025), quantile(nbDist[,w], 0.975))
#	#plot(bkde(bbDist[,w], range.x=c(0,1), canonical=T))
#	#spIntHDI = HDInterval:::hdi.density(bkde(bbDist[,w], range.x=c(0,1), canonical=T, bandwidth=0.1), credMass=0.95, allowSplit=T)
#	spIntHDI = HDInterval:::hdi.density(density(bbDist[,w], from=0, to=1, bw=0.1), credMass=0.95, allowSplit=T)
#	bbHDI[[w]] = matrix(spIntHDI[,], ncol=2)
#	colnames(bbHDI[[w]]) = c('begin', 'end')
#	#	nbBox[w,9:12] = c(mean(nbDist[,w]), median(nbDist[,w]), spIntHDI[1], spIntHDI[2])
#	#	print(spIntHDI)
#}
#
##MSE
#comps = list()
#pM = c()
#bM = c()
#nbM = c()
#bbM = c()
#for(i in 1:howMany){ 
#	#
#	comps[[i]] = DAT$weight[DAT$species==who[i]]/DAT$clustSize[DAT$species==who[i]]
#	comps[[i]] = comps[[i]][!is.na(comps[[i]])]
#	#
#	pM  = rbind(pM, pBox[i, 'pMean'])
#	bM  = rbind(bM, bBox[i, 'pMean'])
#	nbM = rbind(nbM, nbBox[i, 'pMean'])
#	bbM = rbind(bbM, bbBox[i, 'pMean'])
#}
##
#pMSE = getMSE(comps, pM)
#bMSE = getMSE(comps, bM)
#nbMSE = getMSE(comps, nbM)
#bbMSE = getMSE(comps, bbM)

##
##PLOT COUNTS
##
#
##dev.new()
#pdf('weightPlot.pdf')
#plot(0, 0, ylim=c(0, 60), xlim=c(1-off, howMany+off), xlab='', ylab='Weight', xaxt='n', main='95% Predictive HDI Model Comparison')
#axis(1, at=1:howMany, labels=who)
#for(i in 1:howMany){
#	#data
#	weights = D$weight[D$species==who[i]]
#	points(rep(i, length(weights)), weights, pch='_', cex=4)
#	#poisson
#	px = i-0.25
#	segments(px, pBox[i,'lower'], px, pBox[i,'upper'], lwd=4, col='blue')
#	points(px, pBox[i, 'mean'], pch=19, col='blue')
#	#binomial
#	bx = i-0.25+0.5/3*1
#	segments(bx, bBox[i,'lower'], bx, bBox[i,'upper'], lwd=4, col='red')
#	points(bx, bBox[i, 'mean'], pch=19, col='red')
#	#negative binomial
#	nbx = i-0.25+0.5/3*2
#	segments(nbx, nbBox[i,'lower'], nbx, nbBox[i,'upper'], lwd=4, col='forestgreen')
#	points(nbx, nbBox[i, 'mean'], pch=19, col='forestgreen')
#	#beta binomial
#	bbx = i-0.25+0.5/3*3
#	segments(bbx, bbBox[i,'lower'], bbx, bbBox[i,'upper'], lwd=4, col='darkorange')
#	points(bbx, bbBox[i, 'mean'], pch=19, col='darkorange')	
#}
##legend
#legend('topright', legend=c('Poisson', 'Binomial', 'Negative Binomial', 'Beta-Binomial'), lwd=4, col=c('blue', 'red', 'forestgreen', 'darkorange'))
#dev.off()
#
##
##PLOT PROPS
##
#
##
##dev.new()
#pdf('compPlot.pdf')
#plot(0, 0, ylim=c(0, 1.15), xlim=c(1-off, howMany+off), xlab='', ylab='Proportion', xaxt='n', main='95% Predictive HDI Model Comparison')
#axis(1, at=1:howMany, labels=who)
#for(i in 1:howMany){
#       	#make spp comp
#       	comps = DAT$weight[DAT$species==who[i]]/DAT$clustSize[DAT$species==who[i]]
#       	points(rep(i, length(comps)), comps, pch='_', cex=4)
#	#poisson
#        px = i-0.25
#        #segments(px, pBox[i,'pLower'], px, pBox[i,'pUpper'], lwd=4, col='blue')
#        for(j in 1:dim(pHDI[[who[i]]])[1]){
#		segments(px, pHDI[[who[i]]][j,'begin'], px, pHDI[[who[i]]][j,'end'], lwd=4, col='blue')
#	}
#	points(px, pBox[i, 'pMean'], pch=19, col='blue')
#	#binomial
#	bx = i-0.25+0.5/3*1
#	#segments(bx, bBox[i,'pLower'], bx, bBox[i,'pUpper'], lwd=4, col='red')
#	for(j in 1:dim(bHDI[[who[i]]])[1]){
#		segments(bx, bHDI[[who[i]]][j,'begin'], bx, bHDI[[who[i]]][j,'end'], lwd=4, col='red')
#	}
#	points(bx, bBox[i, 'pMean'], pch=19, col='red')
#	#negative binomial
#	nbx = i-0.25+0.5/3*2
#	for(j in 1:dim(nbHDI[[who[i]]])[1]){
#		segments(nbx, nbHDI[[who[i]]][j,'begin'], nbx, nbHDI[[who[i]]][j,'end'], lwd=4, col='forestgreen')
#	}
#	points(nbx, nbBox[i, 'pMean'], pch=19, col='forestgreen')
#	#beta binomial
#	bbx = i-0.25+0.5/3*3
#	for(j in 1:dim(bbHDI[[who[i]]])[1]){
#		segments(bbx, bbHDI[[who[i]]][j,'begin'], bbx, bbHDI[[who[i]]][j,'end'], lwd=4, col='darkorange')
#	}
#	points(bbx, bbBox[i, 'pMean'], pch=19, col='darkorange')
#}
##legend
##legend(4.5, 0.75, legend=c('Poisson', 'Binomial', 'Negative Binomial', 'Beta-Binomial'), lwd=4, col=c('blue', 'red', 'forestgreen', 'darkorange'))
#legend('top', legend=c('Poisson', 'Binomial', 'Negative Binomial', 'Beta-Binomial'), pch=19, col=c('blue', 'red', 'forestgreen', 'darkorange'), ncol=2, lwd=4)#horiz=T, x.intersp=0.1,
#dev.off()
#
##
#pdf('compVioplot.pdf')
#vioplot(bbDist[,1], bbDist[,2], bbDist[,3], bbDist[,4], bbDist[,5], bbDist[,6],
#	names=colnames(bbDist),
#	ylim=c(0, 1.15),
#	col='grey',
#	drawRect=F
#)
#title(
#	main='Beta-Binomial Posterior Predictive Species Compositions',
#        ylab='Proportion'
#)
#dev.off()





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


