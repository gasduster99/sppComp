rm(list=ls())

#
source('../dataFunk.r')
source('../predictFunk.r')
source('../modelFunk.r')

#
#KNOBS
#

#
mcat = 269 
minYear = 1978        
maxYear = 1982  
#
dataFile = sprintf('../%sdata%sTo%s_2018-06-08.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4))
samplePath = sprintf('./%s%s%s/', mcat, minYear, maxYear)
#gold standards for defining strata
#portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET') #c('HKL', 'TWL', 'FPT', 'NET', 'MDT')
catGold  = 1:3

#
#DATA
#

#a work around for adding in landing recipt landing size (this eventually should go inside getRawData in the sql call)
#SAMPLE
sam = read.table(sprintf('../data%dTo%d.csv', minYear, maxYear), sep=',', header=T, stringsAsFactors=F)
ss = aggregate(sam$weight, by=list(species=sam$species, portComplex=sam$portComplex, year=sam$year, qtr=sam$qtr, gearGroup=sam$gearGroup, marketCategory=sam$marketCategory, live=sam$live, sampleNumber=sam$sampleNumber, totalWeight=sam$totalWeight), FUN=sum)
colnames(ss) = c('species', 'portComplex', 'year', 'qtr', 'gearGroup', 'marketCategory', 'live', 'sampleNumber', 'totalWeight', 'weight')
tt = aggregate(ss$weight, by=list(portComplex=ss$portComplex, year=ss$year, qtr=ss$qtr, gearGroup=ss$gearGroup, marketCategory=ss$marketCategory, live=ss$live, sampleNumber=ss$sampleNumber, totalWeight=ss$totalWeight), FUN=sum)
colnames(tt) = c('portComplex', 'year', 'qtr', 'gearGroup', 'marketCategory', 'live', 'sampleNumber', 'totalWeight', 'weight')
#merge in the landings and focus on WDOW
dd = merge(ss, tt, by=c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live', 'sampleNumber', 'totalWeight'), all.x=T)
colnames(dd) = c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live', 'sampleNumber', 'lands', 'species', 'sampleWeight', 'sampleTotal')
dd$lands = dd$lands/2204.62
#
#thresh = quantile(dd$lands, probs=c(2/5, 4/5))
#thresh = c(0.1, thresh, max(dd$lands))
thresh = c(min(dd$lands), 2.821348, 16.496721, max(dd$lands))
dd$landCat = rep(NA, length(dd$lands))
for(i in 1:(length(thresh)-1)){ 
	dd$landCat[dd$lands<thresh[i+1] & dd$lands>=thresh[i]] = i
}
#
#Draw = getRawData(mcat, minYear, maxYear, save=T)
Draw = read.csv(dataFile)
#workaround for adding rptLands
Draw = merge(Draw, dd, by=c('species', 'year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live', 'sampleNumber'), all.x=T)
Draw = Draw[,-which(colnames(Draw)%in%c('sampleWeight', 'sampleTotal', 'lands'))]
#colnames(Draw)[which(colnames(Draw)%in%c('lands'))] = 'rptLands' 
#now I define sppGold from the data
sppGold = as.character(unique(Draw$species))
#add implied multinomial species structure
D = makeD(sppGold, Draw)
#add predictive structure 
DPred = addPredStrat(sppGold, portGold, gearGold, yearGold, qtrGold, catGold, D)

#
#MODEL
#

#interactions
DPred$YQ = as.character(interaction(DPred$year, DPred$qtr))
DPred$SP = as.character(interaction(DPred$species, DPred$port))
DPred$SG = as.character(interaction(DPred$species, DPred$gear))
DPred$SL = as.character(interaction(DPred$species, DPred$landCat))
#prior
sdPrior = abs(rcauchy(10^6, 0, 10^3))
pPrior = log( (1/(sdPrior^2)) ) #[(1/(sdPrior^2))<10^6] )
pPrior = density(pPrior, n=10^4) #, from=0, to=10^6)
y = pPrior$y 
y[1:which(y==max(y))]=max(y)
pPriorTable = INLA:::inla.paste(c("table:", cbind(pPrior$x, y)))
#model
modelDef = weight~species+gear+port+f(YQ)+f(SP)+SL
#, model='iid', hyper=list(prec=list(prior=pPriorTable)))+f(SP, model='iid', hyper=list(prec=list(prior=pPriorTable)))
fit = runModel(modelDef, DPred, 24)
#sample
sampleTime = system.time(sampler(fit, portGold, gearGold, qtrGold, yearGold, catGold, DPred, M=10^4, samplePath=samplePath, cores=7))
metrics = t(c(fit$mlik[1], fit$waic$waic, fit$dic$dic, fit$cpu.used['Total']))
colnames(metrics) = c('mlik', 'waic', 'dic', 'time')
write.csv(format(metrics, scientific=T, digits=22), file="./metrics.csv", row.names=F, quote=F)

##
##DIAGNOSTICS
##
#
##
#nominal = 0.68
#pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
#plotPerfMod(pp, col=c('black'), pch=c(19), level=nominal, save=T)
#write.csv(pp, file='disaggregated68.csv', row.names=F)
#plotPerfMod(aggPerf(pp, c('year', 'gear', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
#write.csv(aggPerf(pp, c('year', 'gear', 'species')), file='gearYearSpp68.csv', row.names=F)
#plotPerfMod(aggPerf(pp, c('year', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
#write.csv(aggPerf(pp, c('year', 'species')), file='yearSpp68.csv', row.names=F)
##
#mads = aggMad(pp, by=list(species=pp$species), nominal)
#mads = mads[order(mads$mad),]
#write.csv(mads, file='sppMad68.csv', row.names=F)
#pdf(sprintf('sppMad68.pdf'), width=15)
#barplot(tail(mads$mad, 15), names=tail(mads$species, 15), ylab='MAD', main='MAD Ordered by Species')
#dev.off()
##
#for( s in sppGold ){
#	#
#	pps = pp[pp$species==s,]
#	#
#	portMarg = aggPerf(pps, c('port'))
#	colnames(portMarg)[1] = sprintf('marginal%s', s)
#	gearMarg = aggPerf(pps, c('gear'))
#	colnames(gearMarg)[1] = sprintf('marginal%s', s)
#	yearMarg = aggPerf(pps, c('year'))
#	colnames(yearMarg)[1] = sprintf('marginal%s', s)
#	yearMarg[,1] = as.character(yearMarg[,1])
#	qtrMarg  = aggPerf(pps, c('qtr'))
#	colnames(qtrMarg)[1] = sprintf('marginal%s', s)
#	qtrMarg[,1] = sprintf('Q%s', qtrMarg[,1])
#	#
#	marginals = rbind(portMarg, gearMarg, yearMarg, qtrMarg)
#	plotPerfMod(marginals, col=c('black'), pch=c(19), level=nominal, save=T)
#	write.csv(marginals, file=sprintf('marginal%s/marginal%s68.csv', s, s), row.names=F)
#}
##
#nominal = 0.95
#pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
#plotPerfMod(pp, col=c('black'), pch=c(19), level=nominal, save=T)
#write.csv(pp, file='disaggregated95.csv', row.names=F)
#plotPerfMod(aggPerf(pp, c('year', 'gear', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
#write.csv(aggPerf(pp, c('year', 'gear', 'species')), file='gearYearSpp95.csv', row.names=F)
#plotPerfMod(aggPerf(pp, c('year', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
#write.csv(aggPerf(pp, c('year', 'species')), file='yearSpp95.csv', row.names=F)
##
#mads = aggMad(pp, by=list(species=pp$species), nominal)
#mads = mads[order(mads$mad),]
#write.csv(mads, file='sppMad95.csv', row.names=F)
#pdf(sprintf('sppMad95.pdf'), width=15)
#barplot(tail(mads$mad, 15), names=tail(mads$species, 15), ylab='MAD', main='MAD Ordered by Species')
#dev.off()
##
#for( s in sppGold ){
#	#
#	pps = pp[pp$species==s,]
#	#
#	portMarg = aggPerf(pps, c('port'))
#	colnames(portMarg)[1] = sprintf('marginal%s', s)
#	gearMarg = aggPerf(pps, c('gear'))
#	colnames(gearMarg)[1] = sprintf('marginal%s', s)
#	yearMarg = aggPerf(pps, c('year'))
#	colnames(yearMarg)[1] = sprintf('marginal%s', s)
#	yearMarg[,1] = as.character(yearMarg[,1])
#	qtrMarg  = aggPerf(pps, c('qtr'))
#	colnames(qtrMarg)[1] = sprintf('marginal%s', s)
#	qtrMarg[,1] = sprintf('Q%s', qtrMarg[,1])
#	#
#	marginals = rbind(portMarg, gearMarg, yearMarg, qtrMarg)
#	plotPerfMod(marginals, col=c('black'), pch=c(19), level=nominal, save=T)
#	write.csv(marginals, file=sprintf('marginal%s/marginal%s95.csv', s, s), row.names=F)
#}







#plotPerfMod(aggPerf(pp, c('year', 'gear', 'port')), aggPerf(pp, c('year', 'gear', 'port')), aggPerf(pp, c('year', 'gear', 'port')), col=c('black', 'red', 'blue'), pch=c(19, 18, 10), level=nominal)


