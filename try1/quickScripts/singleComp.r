rm(list=ls())

#
library(INLA)

#
#
#

#dat = read.table('1985_trwl_mcat250.csv', header=T, sep=',')
dat = read.table('data85to90.csv', header=F, sep=',', stringsAsFactors=F)
colnames(dat) = c('id', 'clust', 'species', 'weight', 'year', 'qtr', 'port', 'portComplex', 'gear', 'mcat', 'total', 'isLive')

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
#qtr = 
D = dat[dat$mcat==mct & dat$year==yer & dat$portComplex==plc & dat$gear==ger,]
end = length(D$id)
for(id in unique(D$id)){
	#
        wid = which(D$id==id)
        #
        #off  = D$off[wid[1]]
        port = D$port[wid[1]]
        gear = D$gear[wid[1]]
        year = D$year[wid[1]]
        qtr  = D$qtr[wid[1]]
	#       
        for(sn in sppEff[!sppEff%in%D$spp[wid]]){
                #
                end = end + 1
                #
		add = matrix(rep(0, 12), ncol=12, nrow=1)
		colnames(add) = c('id', 'clust', 'species', 'weight', 'year', 'qtr', 'port', 'portComplex', 'gear', 'mcat', 'total', 'isLive')
		add[,'id']   = id
                #add$off = off
                add[,'portComplex'] = port
                add[,'gear'] = gear
                add[,'year'] = year
                add[,'qtr']  = qtr
                add[,'species']  = sn
                add[,'weight'] = 0
		#
		D = rbind(D, add)
	}
}

#plot data, with model outputs (weight and comp. scale)
bp = boxplot(as.numeric(D$weight)~D$species, plot=F)
o = order(bp$stats[5,], decreasing=T)
#
off = 0.5
howMany = 7
who = head(bp$names[o], howMany)
DAT = D[D$species%in%who, c('weight', 'species')]
DAT$weight = as.numeric(DAT$weight)
#
#fit poisson, binomial, negative binomial, beta-binomial
pOut  = inla(weight~species, data=DAT, family='poisson', control.compute=list(config=T))
##bOut  = inla(weight~species, data=DAT, family='binomial')
##nbOut = inla(weight~species, data=DAT, family='nbinomial')
##bbOut = inla(weight~species, data=DAT, family='betabinomial')

#
plot(0, 0, ylim=c(0, 60), xlim=c(1-off, howMany+off), xlab='', ylab='', xaxt='n')
axis(1, at=1:howMany, labels=who)
for(i in 1:howMany){
	weights = D$weight[D$species==who[i]]
	points(rep(i, length(weights)), weights, pch='_', cex=4)
}
##
##where = D$species%in%head(bp$names[o], 10)
##dev.new();
##boxplot(D$weight[where] ~ D$species[where])#D$species%in%head(bp$names[o], 10)])

##
##MAKE FULL DATA
##
#
##zero data
#for(id in unique(D$id)){
#        #
#        wid = which(D$id==id)
#        #
#        off  = D$off[wid[1]]
#        port = D$port[wid[1]]
#        gear = D$gear[wid[1]]
#        year = D$year[wid[1]]
#        qtr  = D$qtr[wid[1]]
#        #       
#        for(sn in sppEff[!sppEff%in%D$spp[wid]]){
#                #
#                end = end + 1
#                #
#                D$id[end]   = id
#                D$off[end]  = off
#                D$port[end] = port
#                D$gear[end] = gear
#                D$year[end] = year
#                D$qtr[end]  = qtr
#                D$spp[end]  = sn
#                #
#                D$weight[end] = 0
#        }
#}
#
##
##UNOBSERVED DATA
##
#
##prediction sum cluster size 
#fill = 100
##
#if( length(portEff)>1 ){ pIt=portEff }else{ pIt=c(1) }
#if( length(gearEff)>1 ){ gIt=gearEff }else{ gIt=c(1) }
#if( length(yearEff)>1 ){ yIt=yearEff }else{ yIt=c(1) }
#if( length( qtrEff)>1 ){ qIt=qtrEff  }else{ qIt=c(1) }
#if( length( sppEff)>1 ){ sIt=sppEff  }else{ sIt=c(1) }
##
#for(p in pIt){
#for(g in gIt){
#for(q in qIt){
#for(y in yIt){
#for(s in sIt){
#        #
#        wJoint = which(
#                D$port==p       &
#                D$gear==g       &
#                D$year==y       &
#                D$qtr==q        &
#                D$spp==s
#        )
#        #
#        if( length(wJoint)==0 ){
#                #data grows by a single row with 0 weight
#                end = end + 1
#                #
#                D$id[end]     = NA
#                D$weight[end] = NA
#                D$off[end]    = fill
#                D$port[end]   = p
#                D$gear[end]   = g
#                D$year[end]   = y
#                D$qtr[end]    = q
#                D$spp[end]    = s
#
#        }
#
#}}}}
#}


