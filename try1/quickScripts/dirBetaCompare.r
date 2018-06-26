rm(list=ls())

#
library(MCMCpack)
library(rstan)
library(boot)
library(INLA)
library(loo)
#
INLA:::inla.dynload.workaround()

#
#DATA
#

#
set.seed(1)
#
alpha = c(100, 10, 10, 5, 1, 1, 0.1, 0.1, 0.001, 0.001)
#
n  = 1000
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
M    = 10^4
cpus = 4
#beta-binomial model
D = data.frame(
	nSpp = as.vector(dat), 
	spp  = as.character(spp), 
	N    = n, 
	MN   = mn,
	P    = P
)
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
#PLOT
#

#
dev.new()
boxplot(cbind(propDM, ppBB))
#
dev.new()
boxplot(cbind(pDM, pBB))
#
dev.new()
boxplot(dat)
#
dev.new()
boxplot(dat/rowSums(dat))
#abline(h=alpha[1]/sum(alpha), col='red')
#
#ppBB = ppBB[!is.na(ppBB[,1]),]
#dev.new()
#smoothScatterd((ppBB, ppDM)

