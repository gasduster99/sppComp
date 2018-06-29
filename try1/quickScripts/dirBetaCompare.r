rm(list=ls())

#
suppressMessages(library(HDInterval, quietly=FALSE))
library(MCMCpack)
library(rstan)
library(boot)
library(INLA)
library(loo)
#
INLA:::inla.dynload.workaround()

#
#FUNCTIONS
#

#
sppPredPerf = function(W, sp, prob, adj){
        #W      : a single stratum of D continaing the names species, nBB, and weight
        #sp     : a matrix of posterior predictive distribution for each spp
	#prob	: nominal level of prediction
	#adj	: density estimation tuning parameter

        #
        W$weight = as.numeric(W$weight)
        W$nBB = as.numeric(W$nBB)
	#
	end = 1
	pred = list()
        #avoid the case where no data exists
        if( dim(W)[1]>0 ){
                #
                for(s in colnames(sp)){
                        #
                        spIntHDI = hdi(density(sp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
                        #spIntHDI = hdi(density(sp[,s], from=0, to=1), credMass=prob, allowSplit=T)
                        #
                        WS = W[W$species==s,]
                        if( dim(WS)[1]>0 ){
                                #make comps
                                cp = WS$weight/WS$nBB
                                #cp = c()
                                #wp = c()
                                #for(sam in WS$sampleNumber){
                                #        for(clust in WS[WS$sampleNumber==sam, 'clusterNumber']){
                                #               #cp = c(cp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight']/clustSize[clustSize$sampleNumber==sam & clustSize$clusterNumber==clust,'size'])
                                #               #wp = c(wp, WS[WS$sampleNumber==sam & WS$clusterNumber==clust, 'weight'])
                                #               cp = c(cp, WS[WS$sampleNumber==sam, 'weight']/WS[WS$sampleNumber==sam, 'clustSize'])
                                #               print(WS[WS$sampleNumber==sam, 'weight']/WS[WS$sampleNumber==sam, 'clustSize'])
                                #       }
                                #}
                                #print(cp)
                                #print(wp)
                                #spHDI
                                inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other intervals
                                for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
                                cpHdiMean = mean(inOut)
                                #
                                #pred$port[end]    = as.character(p)
                                #pred$gear[end]    = as.character(g)
                                #pred$qtr[end]     = q
                                #pred$year[end]    = y
                                pred$species[end] = as.character(s)
                                pred$n[end]       = length(cp)
                                #pred$landing[end] = WS[1,'landing']
                                pred$coverage[end]= cpHdiMean
                                #
                                end = end + 1
                        }
                }
        }
	#
	return( as.data.frame(pred) )
}

#
plotPerfMod = function(..., level,
        col='black',
        pch=19,
        save=F,
        saveString=''
        ){
        #...    : as many predictive performance data structures that are desired to compare
        #level  : a reference level comparing predictions
        #col    : colors for the ... structures
        #pch    : point shape for the ... structures
        #
        #value: a series of page sized plots

        #
        l = list(...)
        preds = l[[1]]
        #
        scale = 20#18.5 
        #
        perPage = 22
        R = dim(preds)[1]
        c = dim(preds)[2]
        #
        cexs = preds$landing/mean(preds$landing)
        cols = rep('black', R)
        #
        formString = sprintf('%%s/%%s-%1.2f-Diagnostic-%%%dd%%s.pdf', round(level, 2), nchar(as.character(ceiling(R/perPage))) )
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
                } else{ dev.new(width=8.5/(8.5+11)*scale, height=11/(8.5+11)*scale) }
                #
                il = 1
                for(preds in l){
                        #
                        pp = preds[rows,]
                        #
                        if( il>1 ){ par(new=TRUE) }
                        #4.2
                        par(mar=c(5.1,4.2*(c-2)^1.06,4.1,2.1))
                        plot(pp$coverage, r:1,
                                pch  = pch[il],
                                cex  = cexs[rows],
                                col  = col[il],
                                xlim = c(0, 1),
                                yaxt = 'n',
                                ann  = F,
                                axes = F
                        )
			print(cexs[rows])
			print(rows)
                        #
                        axis(side=1,
                                at = round(c(
                                        0, #quantile(pp$coverage, 0.01), 
                                        0.5,
                                        max(0, min(1, level)),
                                        1 #quantile(pp$coverage, 0.99)
                                ), 2)
                        )
                        #
                        for(i in 1:r){  segments(level, i, rev(pp$coverage)[i], i, col=col[il]) } #rev(cols[rows])[i]) }
                        #
                        abline( v  = level,
                               col = "black",
                               lwd = 2
                        )
                        #
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
                        il = il+1
                }
                #
                if( save ){ dev.off() }
        }
        #
        if( save ){
                #
                system(sprintf('convert %s/%s-%1.2f-Diagnostic-*%s.pdf %s/%s-%1.2f-Diagnostic%s.pdf', path, path, round(level,2), saveString, path, path, round(level,2), saveString))
                #system(sprintf('pdftk %s/%s-%1.2f-Diagnostic-*%s.pdf output %s/%s-%1.2f-Diagnostic%s.pdf', path, path, round(level,2), saveString, path, path, round(level,2), saveString)) 
                #system(sprintf('convert %s/%s-Diagnostic.pdf %s/%s-Diagnostic.png', path, path, path, path))
        }
}

#
#DATA
#

#
set.seed(1)
#
alpha = c(100, 10, 10, 5, 1, 1, 0.1, 0.1, 0.001) #seq(10, 20, length.out=5) #c(200, 150, 150, 120, 100)  #
#
n  = 30
P  = length(alpha)
mn = 100
#
ps  = matrix(NA, nrow=n, ncol=P)
dat = matrix(NA, nrow=n, ncol=P)
as  = alpha/sum(alpha)
#alphas = matrix(NA, nrow=n, ncol=P)
#colnames(ps) = c('dat1', 'dat2', 'dat3', 'dat4')
for(i in 1:n){
        #alphas[i, ] = rdirichlet(1, alpha)
        #ps[i,] = rdirichlet(1, alphas[i,])
        ps[i,]  = rdirichlet(1, alpha)
        dat[i,] = rmultinom(1, mn, ps[i,])
}
#
spp = c()
aSam = c()
for(i in 1:P){
        spp  = c(spp, rep(i, n))
        aSam = cbind(aSam, rep(as[i], mn))
}

#
#MODEL
#

#
M    = 10^3
cpus = 8
D = data.frame(
	nSpp = as.vector(dat), 
	spp  = as.character(spp), 
	N    = n, 
	MN   = mn,
	P    = P
)
#poisson model
pOut = inla(nSpp~spp-1, 
	family		= 'poisson', 
	data		= D, 
	num.threads	= cpus, 
	control.compute	= list(dic=T, waic=T, config=T)
)
#negative binomial model
nbOut = inla(nSpp~spp-1, 
	family		= 'nbinomial', 
	data		= D, 
	#offset		= rep(mn, n),
	num.threads	= cpus, 
	control.compute = list(dic=T, waic=T, config=T)
)
#beta-binomial model
bbOut = inla( nSpp~spp-1, 
	family          = 'betabinomial', 
	data            = D, 
	Ntrials         = rep(mn, n),
        num.threads     = cpus,
        control.compute = list(dic=T, waic=T, config=T)
)
#dirichelet-multinomial model
D = list(
        nSpp = as.array(dat),
        spp  = spp,
        N    = n, 
        MN   = mn,
        P    = P,
	as   = alpha 
)
DMOut = stan( file = "dirMult.stan",
	data  = D,
	iter  = M,
        cores = cpus,
	chains= cpus
)

#
#PREDICTION
#

#
DMSam = extract(DMOut, permuted=T)
m = dim(DMSam$theta)[1]
pDM = matrix(NaN, nrow=m, ncol=P)
propDM = pDM
for(i in 1:m){
	propDM[i,] = rdirichlet(1, DMSam$alpha[i,])
        pDM[i,] = rmultinom(1, mn, propDM[i,])
}
ppDM = pDM/rowSums(pDM)
#
bbSam = inla.posterior.sample(m, bbOut)
bbSam = t(matrix(unlist(bbSam), ncol=m))[,c(1, (P*n+1):(P*n+P)+1)]
mup = inv.logit(bbSam[,1:P+1])
rho = inv.logit(bbSam[,1])
#
alp = mup*(1-rho)/rho
bet = (1-mup)/rho
#
pBB = matrix(NaN, nrow=m, ncol=P)
for(i in 1:P){
	ps = rbeta(m, alp[,i], bet[,i])
	pBB[,i] = rbinom(m, size=mn, prob=ps)	
}
ppBB = pBB/rowSums(pBB)
#
ppBBClean   = ppBB[!is.na(ppBB[,1]),]
propDMClean = propDM[!is.na(ppBB[,1]),]
ppBBClean   = ppBBClean[!is.na(propDMClean[,1]),]
propDMClean = propDMClean[!is.na(propDMClean[,1]),]
colnames(ppBBClean) = sprintf('spp%d', 1:length(alpha))
colnames(propDMClean) = sprintf('spp%d', 1:length(alpha))
#
W = data.frame(
	species = sprintf('spp%d', spp),
	nBB 	= rep(mn, length(as.vector(dat))),
	weight 	= as.vector(dat)
)

#
#PLOT
#

#
level = 0.68
betaAdj = optimize(
	function(x){
		pp = sppPredPerf(W, ppBBClean, level, x)
		return(abs(sum(pp$coverage*pp$n)/sum(pp$n) - 0.68))
	},
	c(0, 10)
)$minimum
diriAdj = optimize(
	function(x){
		pp = sppPredPerf(W, propDMClean, level, x)
		return(abs(sum(pp$coverage*pp$n)/sum(pp$n) - 0.68))
	},
	c(0, 10)
)$minimum
#
ppBeta = sppPredPerf(W, ppBBClean, level, betaAdj)
ppBeta$landing = rep(1, length(ppBeta$n))
ppDiri = sppPredPerf(W, propDMClean, level, diriAdj)
ppDiri$landing = rep(1, length(ppDiri$n))
#
plotPerfMod(ppBeta, ppDiri, level=level, col=c('black', 'red'))

##
#level = 0.95
##
#ppBeta = sppPredPerf(W, ppBBClean, level, betaAdj)
#ppBeta$landing = rep(1, length(ppBeta$n))
#ppDiri = sppPredPerf(W, propDMClean, level, diriAdj)
#ppDiri$landing = rep(1, length(ppDiri$n))
##
#plotPerfMod(ppBeta, ppDiri, level=level, col=c('black', 'red'))




#
#PLOT
#

#
#boxplot(W$weight/W$nBB~W$species, ylim=c(0,1), range=0)
#boxplot(propDMClean, col='blue', add=T, range=0)
#boxplot(ppBBClean, col='red', add=T, range=0)

##
#dev.new()
#boxplot(cbind(propDM, ppBB))
##
#dev.new()
#boxplot(cbind(pDM, pBB))
##
#dev.new()
#boxplot(dat)
##
#dev.new()
#boxplot(dat/rowSums(dat))
##abline(h=alpha[1]/sum(alpha), col='red')
##
##ppBB = ppBB[!is.na(ppBB[,1]),]
##dev.new()
##smoothScatterd((ppBB, ppDM)
#
