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
#DATA
#

#
set.seed(1)
#
alpha = c(200, 150, 100, 150, 120, 100, 90)  #c(100, 10, 10, 5, 1, 1, 0.1, 0.1, 0.001)
#
n  = 100
P  = length(alpha)
mn = 50
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
cpus = 4
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
        cores = cpus
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
betaAdj = optimize(
	function(x){
		pp = sppPredPerf(W, ppBBClean, 0.68, x)
		return(abs(sum(pp$coverage*pp$n)/sum(pp$n) - 0.68))
	},
	c(0, 10)
)$minimum
diriAdj = optimize(
	function(x){
		pp = sppPredPerf(W, propDMClean, 0.68, x)
		return(abs(sum(pp$coverage*pp$n)/sum(pp$n) - 0.68))
	},
	c(0, 10)
)$minimum
#
ppBeta = sppPredPerf(W, ppBBClean, 0.68, betaAdj)
ppDiri = sppPredPerf(W, propDMClean, 0.68, diriAdj)

#
#PLOT
#

boxplot(W$weight/W$nBB~W$species, ylim=c(0,1), range=0)
boxplot(propDMClean, col='blue', add=T, range=0)
boxplot(ppBBClean, col='red', add=T, range=0)

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
