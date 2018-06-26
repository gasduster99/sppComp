rm(list=ls())

#
source('../dataFunk.r')
source('../predictFunk.r')
source('../modelFunk.r')

#
#KNOBS
#

#
globPath = "/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s/2*M*/", globPath))
#
for(run in runPaths){
	baseDir = getwd()
	print(run)
	#
	mcat = substring(run, 23, 25)
	minYear = substring(run, 26, 29)
	maxYear = substring(run, 30, 33)
	##
	#mcat = 250
	#minYear = 1978
	#maxYear = 1982
	##
	dataFile = sprintf('../%sdata%sTo%s_2018-06-08.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4))
	samplePath = sprintf('%s/%s%s%s/', run, mcat, minYear, maxYear)
	#gold standards for defining strata
	#portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
	portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')
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
	sppGold  = as.character(unique(Draw$species))
	#add implied multinomial species structure
	D = makeD(sppGold, Draw)
	
	#
	#DIAGNOSTICS
	#
	
	#
	setwd(baseDir)
	nominal = 0.68
	pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
	setwd(run)
	#
	plotPerfMod(pp, col=c('black'), pch=c(19), level=nominal, save=T)
	write.csv(pp, file='disaggregated68.csv', row.names=F)
	plotPerfMod(aggPerf(pp, c('year', 'gear', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
	write.csv(aggPerf(pp, c('year', 'gear', 'species')), file='gearYearSpp68.csv', row.names=F)
	plotPerfMod(aggPerf(pp, c('year', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
	write.csv(aggPerf(pp, c('year', 'species')), file='yearSpp68.csv', row.names=F)
	#
	mads = aggMad(pp, by=list(species=pp$species), nominal)
	mads = mads[order(mads$mad),]
	write.csv(mads, file='sppMad68.csv', row.names=F)
	pdf(sprintf('sppMad68.pdf'), width=15)
	barplot(tail(mads$mad, 15), names=tail(mads$species, 15), ylab='MAD', main='MAD Ordered by Species')
	dev.off()
	#
	for( s in sppGold ){
	        #
	        pps = pp[pp$species==s,]
	        #
	        portMarg = aggPerf(pps, c('port'))
	        colnames(portMarg)[1] = sprintf('marginal%s', s)
	        gearMarg = aggPerf(pps, c('gear'))
	        colnames(gearMarg)[1] = sprintf('marginal%s', s)
	        yearMarg = aggPerf(pps, c('year'))
	        colnames(yearMarg)[1] = sprintf('marginal%s', s)
	        yearMarg[,1] = as.character(yearMarg[,1])
	        qtrMarg  = aggPerf(pps, c('qtr'))
	        colnames(qtrMarg)[1] = sprintf('marginal%s', s)
	        qtrMarg[,1] = sprintf('Q%s', qtrMarg[,1])
	        #
	        marginals = rbind(portMarg, gearMarg, yearMarg, qtrMarg)
	        plotPerfMod(marginals, col=c('black'), pch=c(19), level=nominal, save=T)
	        write.csv(marginals, file=sprintf('marginal%s/marginal%s68.csv', s, s), row.names=F)
	}
	#
	setwd(baseDir)
	nominal = 0.95
	pp = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
	setwd(run)
	#
	plotPerfMod(pp, col=c('black'), pch=c(19), level=nominal, save=T)
	write.csv(pp, file='disaggregated95.csv', row.names=F)
	plotPerfMod(aggPerf(pp, c('year', 'gear', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
	write.csv(aggPerf(pp, c('year', 'gear', 'species')), file='gearYearSpp95.csv', row.names=F)
	plotPerfMod(aggPerf(pp, c('year', 'species')), col=c('black'), pch=c(19), level=nominal, save=T)
	write.csv(aggPerf(pp, c('year', 'species')), file='yearSpp95.csv', row.names=F)
	#
	mads = aggMad(pp, by=list(species=pp$species), nominal)
	mads = mads[order(mads$mad),]
	write.csv(mads, file='sppMad95.csv', row.names=F)
	pdf(sprintf('sppMad95.pdf'), width=15)
	barplot(tail(mads$mad, 15), names=tail(mads$species, 15), ylab='MAD', main='MAD Ordered by Species')
	dev.off()
	#
	for( s in sppGold ){
	        #
	        pps = pp[pp$species==s,]
	        #
	        portMarg = aggPerf(pps, c('port'))
	        colnames(portMarg)[1] = sprintf('marginal%s', s)
	        gearMarg = aggPerf(pps, c('gear'))
	        colnames(gearMarg)[1] = sprintf('marginal%s', s)
	        yearMarg = aggPerf(pps, c('year'))
	        colnames(yearMarg)[1] = sprintf('marginal%s', s)
	        yearMarg[,1] = as.character(yearMarg[,1])
	        qtrMarg  = aggPerf(pps, c('qtr'))
	        colnames(qtrMarg)[1] = sprintf('marginal%s', s)
	        qtrMarg[,1] = sprintf('Q%s', qtrMarg[,1])
	        #
	        marginals = rbind(portMarg, gearMarg, yearMarg, qtrMarg)
	        plotPerfMod(marginals, col=c('black'), pch=c(19), level=nominal, save=T)
	        write.csv(marginals, file=sprintf('marginal%s/marginal%s95.csv', s, s), row.names=F)
	}
	#
	setwd(baseDir)
}

