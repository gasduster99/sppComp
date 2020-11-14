rm(list=ls())

#
#
#

#
minYear = 1978
maxYear = 1982
#
globPath = sprintf("/home/nick/Documents/sppComp/inla/hotWired%sto%s", minYear, maxYear)
runPaths = Sys.glob(sprintf("%s//*%s%s/", globPath, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))
lives = sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 4, 4)})
compFiles = sprintf("%s/MNT/TWL/2/%s/sppComp.csv", runPaths, minYear) 
#
i = 1
ii = 1
sppList = data.frame(tStart=integer(), tEnd=integer(), mcat=integer(), live=character(), spp=character(), stringsAsFactors=F)
for(name in compFiles){
	#
	spp = read.table(compFiles[i], header=F, sep=',', stringsAsFactors=F, nrows=1)
	for(s in spp){
		sppList[ii,] = c(minYear, maxYear, mcats[i], lives[i], s) 
		ii = ii+1	
	}
	i = i+1
}

#
minYear = 1983
maxYear = 1990
#
globPath = sprintf("/home/nick/Documents/sppComp/inla/hotWired%sto%s", minYear, maxYear)
runPaths = Sys.glob(sprintf("%s//*%s%s/", globPath, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))
lives = sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 4, 4)})
compFiles = sprintf("%s/MNT/TWL/2/%s/sppComp.csv", runPaths, minYear) 
#
i = 1
for(name in compFiles){
	#
	spp = read.table(compFiles[i], header=F, sep=',', stringsAsFactors=F, nrows=1)
	for(s in spp){
		sppList[ii,] = c(minYear, maxYear, mcats[i], lives[i], s) 
		ii = ii+1	
	}
	i = i+1
}

#
minYear = 1991
maxYear = 2001
#
globPath = sprintf("/home/nick/Documents/sppComp/inla/hotWired%sto%s", minYear, maxYear)
runPaths = Sys.glob(sprintf("%s//*%s%s/", globPath, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))
lives = sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 4, 4)})
compFiles = sprintf("%s/MNT/TWL/2/%s/sppComp.csv", runPaths, minYear) 
#
i = 1
for(name in compFiles){
	#
	spp = read.table(compFiles[i], header=F, sep=',', stringsAsFactors=F, nrows=1)
	for(s in spp){
		sppList[ii,] = c(minYear, maxYear, mcats[i], lives[i], s) 
		ii = ii+1	
	}
	i = i+1
}

#
write.csv(sppList, 'sppList.csv', row.names=F, quote=F)
