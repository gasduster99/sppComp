rm(list=ls())

#
library(MCMCpack)
library(INLA)
set.seed(1)

#
#DATA
#

#
n = 1000
alpha = c(1, 2, 3, 400)
P = length(alpha)
mn = 1000
#
ps = matrix(NA, nrow=n, ncol=P)
dat = matrix(NA, nrow=n, ncol=P)
for(i in 1:n){
	ps[i,] = rdirichlet(1, alpha)
	dat[i,] = rmultinom(1, mn, ps[i,])
}
D = data.frame(nSpp = as.vector(dat), spp=as.character(c(rep(1, n), rep(2, n), rep(3, n), rep(4, n))))

#
#MODEL
#

#
cpus = 8

#poisson model
pOut = inla(nSpp~spp-1, family='poisson', data=D, num.threads=cpus, control.compute=list(dic=T, waic=T, config=T))
#negative binomial model
nbOut = inla(nSpp~spp-1, family='nbinomial', data=D, num.threads=cpus, control.compute=list(dic=T, waic=T, config=T))

#
#PROPS
#

#
m = 1000
#
pSam = inla.posterior.sample(m, pOut)
pSam = exp(t(matrix(unlist(pSam), ncol=m))[,401:404])
colnames(pSam) = as.character(1:4)
pP = pSam/rowSums(pSam)
#
nbSam = inla.posterior.sample(m, nbOut)
nbSam = t(matrix(unlist(nbSam), ncol=m))[,c(1, 402:405)]
nbMat = nbSam
colnames(nbMat) = c('size', as.character(1:4))
nbMat[,2:5] = exp(nbMat[,2:5])
nOdds = nbMat[,2:5]/nbMat[,1]
#
lam = matrix(NA, nrow=m, ncol=P)
pred = matrix(NA, nrow=m, ncol=P)
for(l in 1:4){ 
	lam[,l] = rgamma(m, shape=nbSam[,1], scale=nOdds[,l]) 
	pred[,l] = rnbinom(m, mu=nbSam[,l+1], size=nbSam[,1])#rpois(m, lam[,l])
}
nbP = lam/rowSums(lam)
predP = pred/rowSums(pred)
#
dev.new();
boxplot(cbind(ps, pP, nbP, predP))
