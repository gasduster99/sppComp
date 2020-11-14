rm(list=ls())

#
#78-82
#

#
minYear = 1978
maxYear = 1982
yearGold = minYear:maxYear
portGold = c('OSB', 'OLA', 'OSD')

#
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'source', 'V2'), stringsAsFactors=F)
land$weight = land$weight/2204.62
#
land = land[land$year%in%yearGold,]
land = land[land$mcat%in%mcats,]
land = land[land$port%in%portGold,]
#
nomLand = land[land$source%in%c('Nominal', 'NOMINAL', "NOMINAL(a)"),]
nomYearS = aggregate(nomLand$weight, by=list(year=nomLand$year), FUN=sum)

#
yearWeightS = cbind(aggregate(land$weight, by=list(year=land$year), FUN=sum), nomYearS[,-1])

#
#83-90 SOUTH
#

#
minYear = 1983
maxYear = 1990
yearGold = minYear:maxYear
portGold = c('OSB', 'OLA', 'OSD')

#
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'source', 'V2'), stringsAsFactors=F)
land$weight = land$weight/2204.62
#
land = land[land$year%in%yearGold,]
land = land[land$mcat%in%mcats,]
land = land[land$port%in%portGold,]
#
nomLand = land[land$source%in%c('Nominal', 'NOMINAL', "NOMINAL(a)"),]
nomYearS = aggregate(nomLand$weight, by=list(year=nomLand$year), FUN=sum)

#
yearWeightS = rbind(yearWeightS, cbind(aggregate(land$weight, by=list(year=land$year), FUN=sum), nomYearS[,-1]))

#
#78-82 NORTH
#

#
minYear = 1978
maxYear = 1982
yearGold = minYear:maxYear
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')

#
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'source', 'V2'), stringsAsFactors=F)
land$weight = land$weight/2204.62
#
land = land[land$year%in%yearGold,]
land = land[land$mcat%in%mcats,]
land = land[land$port%in%portGold,]
#
nomLand = land[land$source%in%c('Nominal', 'NOMINAL', "NOMINAL(a)"),]
nomYearN = aggregate(nomLand$weight, by=list(year=nomLand$year), FUN=sum)

#
yearWeightN = cbind(aggregate(land$weight, by=list(year=land$year), FUN=sum), nomYearN[,-1])

#
#83-90 NORTH
#

#
minYear = 1983
maxYear = 1990
yearGold = minYear:maxYear
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')

#
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'source', 'V2'), stringsAsFactors=F)
land$weight = land$weight/2204.62
#
land = land[land$year%in%yearGold,]
land = land[land$mcat%in%mcats,]
land = land[land$port%in%portGold,]
#
nomLand = land[land$source%in%c('Nominal', 'NOMINAL', "NOMINAL(a)"),]
nomYearN = aggregate(nomLand$weight, by=list(year=nomLand$year), FUN=sum)

#
yearWeightN = rbind(yearWeightN, cbind(aggregate(land$weight, by=list(year=land$year), FUN=sum), nomYearN[,-1]))


