rm(list=ls())

#
source('dataFunk.r')
source('predictFunk.r')
source('modelFunk.r')

#
#KNOBS
#

#
mcat = 250            
minYear = 1978        
maxYear = 1982  
#
dataFile = sprintf('%sdata%sTo%s_2018-06-08.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4))
samplePath = sprintf('./%s%s%s/', mcat, minYear, maxYear)
#gold standards for defining strata
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
#portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET') #c('HKL', 'TWL', 'FPT', 'NET', 'MDT')

#
#DATA
#

#
#Draw = getRawData(mcat, minYear, maxYear, save=T)
Draw = read.csv(dataFile)
#now I define sppGold from the data
sppGold  = unique(Draw$species)
#add implied multinomial species structure
D = makeD(sppGold, Draw)
#add predictive structure 
DPred = addPredStrat(sppGold, portGold, gearGold, yearGold, qtrGold, D)

#
#MODEL
#

#interactions
DPred$YQ = as.character(interaction(DPred$year, DPred$qtr))
DPred$SP = as.character(interaction(DPred$species, DPred$port))
DPred$SG = as.character(interaction(DPred$species, DPred$gear))
#model
modelDef = weight~species+gear+port+f(YQ)
fit = runModel(modelDef, DPred, 48)
#sample
sampleTime = system.time(sampler(fit, portGold, gearGold, qtrGold, yearGold, DPred, M=10^4, samplePath=samplePath, cores=3))

#
#DIAGNOSTICS
#

#
nominal = 0.68
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
plotPerfMod(pp, col=c('black'), pch=c(19), level=nominal, save=T)
write.csv(pp, file='disaggregated68.csv', row.names=F)
plotPerfMod(aggPerf(pp, c('year', 'gear', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
write.csv(aggPerf(pp, c('year', 'gear', 'species')), file='gearYearSpp68.csv', row.names=F)
plotPerfMod(aggPerf(pp, c('year', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
write.csv(aggPerf(pp, c('year', 'species')), file='yearGear68.csv', row.names=F)

#
nominal = 0.95
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
plotPerfMod(pp, col=c('black'), pch=c(19), level=nominal, save=T)
write.csv(pp, file='disaggregated95.csv', row.names=F)
plotPerfMod(aggPerf(pp, c('year', 'gear', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
write.csv(aggPerf(pp, c('year', 'gear', 'species')), file='gearYearSpp95.csv', row.names=F)
plotPerfMod(aggPerf(pp, c('year', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
write.csv(aggPerf(pp, c('year', 'species')), file='yearGear95.csv', row.names=F)




#plotPerfMod(aggPerf(pp, c('year', 'gear', 'port')), aggPerf(pp, c('year', 'gear', 'port')), aggPerf(pp, c('year', 'gear', 'port')), col=c('black', 'red', 'blue'), pch=c(19, 18, 10), level=nominal)


