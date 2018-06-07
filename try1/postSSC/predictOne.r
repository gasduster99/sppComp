rm(list=ls())

#
source('dataFunk.r')
source('predictFunk.r')
source('modelFunk.r')

#
#START
#

##
#stuff = read.csv('tuned.hand')
#for(i in 1:dim(stuff)[1]){
##
mcat = 250	#stuff$mcat[i]		
minYear = 1978	#stuff$minYear[i]	
maxYear = 1983	#stuff$maxYear[i]
samplePath = sprintf('./%s%s%s/', mcat, minYear, maxYear)
#gold standards for defining strata
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET') #c('HKL', 'TWL', 'FPT', 'NET', 'MDT')

#
#DATA
#

##call to database
#Draw = getRawData(mcat, minYear, maxYear, save=T)
Draw = read.csv('data78To83_2018-06-06.csv') #data78To82_2018-04-04.csv')
#now I define sppGold from the data
sppGold  = unique(Draw$species)
#add implied multinomial species structure
D = makeD(sppGold, Draw)
#add predictive structure 
DPred = addPredStrat(sppGold, portGold, gearGold, yearGold, qtrGold, D)

#
#MODEL
#

#
DPred$YQ = as.character(interaction(DPred$year, DPred$qtr))
DPred$SP = as.character(interaction(DPred$species, DPred$port))
fit = runModel(weight~species+gear+port+f(YQ), DPred, 48)
samTime = system.time(sampler(fit, portGold, gearGold, qtrGold, yearGold, DPred, M=10^4, samplePath=samplePath, cores=2))
#postSamples = inla.posterior.sample(10^3, fit)
#hypeSamples = inla.hyperpar.sample(10^3, fit)
#sampleModel(fit, portGold, gearGold, qtrGold, yearGold, DPred, samplePath='./M4/')


##path to samples
#path = '/media/nick/extraBig/fullTimeComplete/'
#avgPath = sprintf("%s/%sto%s/MCAT%d/Top/avgModel/", path, substring(minYear, 3, 4), substring(maxYear, 3, 4), mcat)
#
###
##nominal = 0.5 #0.2 #0.5 #0.68
##pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
##actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
##print(actAgg)
##plotPerf(aggPerf(pp, c('year', 'gear')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
##plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
###plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#
##
#nominal = 0.68 #0.2 #0.5 #0.68
#pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
#actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
#print(actAgg)
#plotPerf(aggPerf(pp, c('year', 'gear')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
##plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear', 'species')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#
##
#nominal = 0.90 #0.2 #0.5 #0.68
#pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
#actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
#print(actAgg)
#plotPerf(aggPerf(pp, c('year', 'gear')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
##plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#
##
#nominal = 0.95 #0.2 #0.5 #0.68
#pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
#actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
#print(actAgg)
#plotPerf(aggPerf(pp, c('year', 'gear')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
##plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#
##
#nominal = 0.99 #0.2 #0.5 #0.68
#pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
#actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
#print(actAgg)
#plotPerf(aggPerf(pp, c('year', 'gear')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.1, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
##plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
##}














