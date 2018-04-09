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
minYear = 1978
maxYear = 1982
#gold standards for defining strata
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET') #c('HKL', 'TWL', 'FPT', 'NET', 'MDT')

###call to database
##D = getRawData(mcat, minYear, maxYear, save=T)
#D = read.csv('data78To82_2018-04-04.csv')
##right now I define species them from the data
#sppGold  = unique(D$species)
##add implied multinomial species structure
#D = makeD(sppGold, D)
###add predictive structure 
##D = addPredStrat(sppGold, portGold, gearGold, yearGold, qtrGold, D)
#
##path to samples
#path = '/media/nick/extraBig/fullTimeComplete/'
#avgPath = sprintf("%s/%sto%s/MCAT%d/Top/avgModel/", path, substring(minYear, 3, 4), substring(maxYear, 3, 4), mcat)
##
#nominal = 0.95 #0.68
#pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, avgPath, 10, 4/12)
#actAgg = sum(pp$coverage*pp$n)/sum(pp$n)
##

#
#PLOT
#

#
#plotPerf(aggPerf(pp, by=list(port=pp$port)), level=nominal)
l = data.frame(
	Port = portGold, 
	Year = rep(yearGold, 2),
	Spp  = rep('BCAC', 10),
	#year = rep(yearGold, 2),
	#year = rep(yearGold, 2),
	n = rep(10, 10),
	coverage = rbeta(10, 1/2, 1/2), 
	n = 1:10 #rep(10, 10)	
)
plotPerf(l, level=0.5)

















