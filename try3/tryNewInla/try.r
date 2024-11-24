rm(list=ls())

#
source('dataFunk.r')
source('modelFunk.r')

#
#KNOBS
#

#
cores = 8

#
mcat = 250
minYear = 1978 #1984 #1991
maxYear = 1983 #1990 #2001

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
#dataFile = sprintf('%sdata%sTo%s_2018-06-08.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4))
raw = getRawData(250, minYear, maxYear) #1991, 2001) #, save=T)
#define sppGold from the data
sppGold  = unique(raw$species)
#
D = makeD(sppGold, raw)

#add predictive structure 
DPred = addPredStrat(sppGold, portGold, gearGold, yearGold, qtrGold, D)

#interactions
DPred$YQ = as.character(interaction(DPred$year, DPred$qtr))
DPred$SP = as.character(interaction(DPred$species, DPred$port))
DPred$SG = as.character(interaction(DPred$species, DPred$gear))

#
#MODEL
#

#model
modelDef = weight~species+gear+port+f(YQ)

#
out = inla(modelDef, family="betabinomial", data=DPred, num.threads=cores, Ntrials=DPred$nBB,
                control.inla=list(
                        int.strategy='ccd'#, #eb',
                        #tolerance=1e-10,
                        #h=1e-10 #,
                        #lincomb.derived.only=F lincomb.derived.correlation.matrix
                ),
                control.compute=list(
                        config=T,
                        mlik=T,
                        dic=T,
                        waic=T,
			return.marginals.predictor=T
                ),
                verbose=F
                #control.fixed = list(
                #        expand.factor.strategy='inla'
                #),
                #control.mode = list(
                #        result=starter,
                #        restart=T
                #)
)

#
#EXPORT
#

#sam = sampler(out, portGold, gearGold, qtrGold, yearGold, D, M=10^4, cores=8, samplePath='./')

#dataFile = sprintf('%sdata%sTo%s_2018-06-08.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4))
#samplePath = sprintf('./%s%s%s/', mcat, minYear, maxYear)





