rm(list=ls())

#
library(INLA)

#
#
#

#dat = read.table('1985_trwl_mcat250.csv', header=T, sep=',')
dat = read.table('data85to90.csv', header=F, sep=',', stringsAsFactors=F)
colnames(dat) = c('number', 'V2', 'species', 'weight', 'year', 'qtr', 'port', 'portComplex', 'gear', 'mcat', 'total', 'isLive')

#add zeros/account for clusters

#subset data
pThing = dat[dat$mcat=='250' & dat$year=='1990' & dat$portComplex=='MNT' & dat$gear=='TWL' ,]
#plot data, with model outputs (weight and comp. scale)
bp = boxplot(pThing$weight~as.character(pThing$species), plot=F)
o = order(bp$stats[5,], decreasing=T)
#
off = 0.5
howMany = 10
who = head(bp$names[o], howMany)
D = pThing[pThing$species%in%who, c('weight', 'species')]

#fit poisson, binomial, negative binomial, beta-binomial
pOut  = inla(weight~species, data=D, family='poisson')
#bOut  = inla(weight~species, data=D, family='binomial')
#nbOut = inla(weight~species, data=D, family='nbinomial')
#bbOut = inla(weight~species, data=D, family='betabinomial')

#
plot(0, 0, ylim=c(0, 60), xlim=c(1-off, howMany+off), xlab='', ylab='')
for(i in 1:length(who)){
	weights = pThing$weight[pThing$species==who[i]]
	points(rep(i, length(weights)), weights, pch='_', cex=4)
}

#where = pThing$species%in%head(bp$names[o], 10)
#dev.new();
#boxplot(pThing$weight[where] ~ pThing$species[where])#pThing$species%in%head(bp$names[o], 10)])


