rm(list=ls())

##
#suppressMessages(library(RJDBC, quietly=FALSE)) #requires "./sqljdbc4.jar"
#suppressMessages(library(getPass, quietly=FALSE))
#library(doParallel)
#library(KernSmooth)
#library(HDInterval)
#
source('dataFunk.r')
source('predictFunk.r')

#
#MAIN
#

#
mcat = 250
minYear = 1978 #
maxYear = 1982 #
#gold standards for defining strata
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET') #c('HKL', 'TWL', 'FPT', 'NET', 'MDT')

##call to database
Draw = getRawData(mcat, minYear, maxYear, save=T)
#D = read.csv('data78To82_2018-04-04.csv')
#right now I define species them from the data
sppGold  = unique(Draw$species)
#add implied multinomial species structure
D = makeD(sppGold, Draw)
##add predictive structure 
##D = addPredStrat(sppGold, portGold, gearGold, yearGold, qtrGold, D)

#path to samples
path = '/media/nick/extraBig/fullTimeComplete/'
avgPath = sprintf("%s/%sto%s/MCAT%d/Top/avgModel/", path, substring(minYear, 3, 4), substring(maxYear, 3, 4), mcat)

#
nominal = 0.5 #0.2 #0.5 #0.68
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
print(actAgg)
plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))

#
nominal = 0.68 #0.2 #0.5 #0.68
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
print(actAgg)
plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))

#
nominal = 0.90 #0.2 #0.5 #0.68
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
print(actAgg)
plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))

#
nominal = 0.95 #0.2 #0.5 #0.68
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
print(actAgg)
plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))

#
nominal = 0.99 #0.2 #0.5 #0.68
pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath) #, 10, 4/12)
actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
print(actAgg)
plotPerf(aggPerf(pp, c('year', 'gear', 'species')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
#plotPerf(aggPerf(pp, c('year', 'qtr', 'port', 'gear')), level=nominal, llv=0.07, save=T, saveString=sprintf('-%s-%s', mcat, minYear))
















