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
Draw = read.csv('data78To83_2018-06-06.csv')
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
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, 0.68, avgPath)

#
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, 0.95, avgPath)








