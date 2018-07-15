rm(list=ls())

#
source('../dataFunk.r')
source('../predictFunk.r')
source('../modelFunk.r')

#
#FUNCTIONS
#

#
substrRight = function(x, n){
	substr(x, n, nchar(x))
}

#
#KNOBS
#

#
mcat = 250
minYear = 1978
maxYear = 1982
#
dataFile = sprintf('../%sdata%sTo%s_2018-06-08.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4))
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
#COMBINE
#

#
legend = c()
baseDir = getwd()
#
run = "/media/nick/extraBig//25019781982M4"
legend = c(legend, substrRight(strsplit(run, '//')[[1]][2], 12))
samplePath = sprintf('%s/%s%s%s/', run, mcat, minYear, maxYear)
runDir = sprintf('../retune/%s', strsplit(run, '//')[[1]][2])
setwd(runDir)
nominal = 0.68
ppM3 = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
setwd(baseDir)
#
run = "/media/nick/extraBig//25019781982M4IGSG"
legend = c(legend, substrRight(strsplit(run, '//')[[1]][2], 12))
samplePath = sprintf('%s/%s%s%s/', run, mcat, minYear, maxYear)
runDir = sprintf('../retune/%s', strsplit(run, '//')[[1]][2])
setwd(runDir)
nominal = 0.68
ppM4 = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
setwd(baseDir)
#
run = "/media/nick/extraBig//25019781982M4IGSP"
legend = c(legend, substrRight(strsplit(run, '//')[[1]][2], 12))
samplePath = sprintf('%s/%s%s%s/', run, mcat, minYear, maxYear)
runDir = sprintf('../retune/%s', strsplit(run, '//')[[1]][2])
setwd(runDir)
nominal = 0.68
ppM6 = predPerf(D, portGold, gearGold, yearGold, qtrGold, nominal, samplePath)
setwd(baseDir)

#
#PLOT
#

#
plotPerfMod(ppM3, ppM4, ppM6, 
	col=c('black', 'red', 'blue'),
	legend=legend,
	pch=c(19, 19, 19), 
	level=nominal, 
	save=T
)
#write.csv(pp, file='disaggregated68.csv', row.names=F)
plotPerfMod(aggPerf(ppM3, c('species', 'gear', 'year')), aggPerf(ppM4, c('species', 'gear', 'year')), aggPerf(ppM6, c('species', 'gear', 'year')),
	col=c('black', 'red', 'blue'),
	legend=legend,
	pch=c(19, 19, 19), 
	level=nominal, 
	save=T
)
#write.csv(aggPerf(pp, c('year', 'gear', 'species')), file='gearYearSpp68.csv', row.names=F)
plotPerfMod(aggPerf(ppM3, c('species', 'year')), aggPerf(ppM4, c('species', 'year')), aggPerf(ppM6, c('species', 'year')),
	col=c('black', 'red', 'blue'), 
	legend=legend,
	pch=c(19, 19, 19), 
	level=nominal, 
	save=T
)
#write.csv(aggPerf(pp, c('year', 'species')), file='yearSpp68.csv', row.names=F)
##
#mads = aggMad(pp, by=list(species=pp$species), nominal)
#mads = mads[order(mads$mad),]
#write.csv(mads, file='sppMad68.csv', row.names=F)
#pdf(sprintf('sppMad68.pdf'), width=15)
#barplot(tail(mads$mad, 15), names=tail(mads$species, 15), ylab='MAD', main='MAD Ordered by Species')
#dev.off()
##
for( s in sppGold ){ 
	#
	pps = ppM3[ppM3$species==s,]
        portMarg = aggPerf(pps, c('port'))
        colnames(portMarg)[1] = sprintf('marg%s', s)
        gearMarg = aggPerf(pps, c('gear'))
        colnames(gearMarg)[1] = sprintf('marg%s', s)
        yearMarg = aggPerf(pps, c('year'))
        colnames(yearMarg)[1] = sprintf('marg%s', s)
        yearMarg[,1] = as.character(yearMarg[,1])
        qtrMarg  = aggPerf(pps, c('qtr'))
        colnames(qtrMarg)[1] = sprintf('marg%s', s)
        qtrMarg[,1] = sprintf('Q%s', qtrMarg[,1])
   	marginalsM3 = rbind(portMarg, gearMarg, yearMarg, qtrMarg)
	#
	pps = ppM4[ppM4$species==s,]
        portMarg = aggPerf(pps, c('port'))
        colnames(portMarg)[1] = sprintf('marg%s', s)
        gearMarg = aggPerf(pps, c('gear'))
        colnames(gearMarg)[1] = sprintf('marg%s', s)
        yearMarg = aggPerf(pps, c('year'))
        colnames(yearMarg)[1] = sprintf('marg%s', s)
        yearMarg[,1] = as.character(yearMarg[,1])
        qtrMarg  = aggPerf(pps, c('qtr'))
        colnames(qtrMarg)[1] = sprintf('marg%s', s)
        qtrMarg[,1] = sprintf('Q%s', qtrMarg[,1])
        marginalsM4 = rbind(portMarg, gearMarg, yearMarg, qtrMarg)
	#
        pps = ppM6[ppM4$species==s,]
        portMarg = aggPerf(pps, c('port'))
        colnames(portMarg)[1] = sprintf('marg%s', s)
        gearMarg = aggPerf(pps, c('gear'))
        colnames(gearMarg)[1] = sprintf('marg%s', s)
        yearMarg = aggPerf(pps, c('year'))
        colnames(yearMarg)[1] = sprintf('marg%s', s)
        yearMarg[,1] = as.character(yearMarg[,1])
        qtrMarg  = aggPerf(pps, c('qtr'))
        colnames(qtrMarg)[1] = sprintf('marg%s', s)
        qtrMarg[,1] = sprintf('Q%s', qtrMarg[,1])
        marginalsM6 = rbind(portMarg, gearMarg, yearMarg, qtrMarg)
	#
        plotPerfMod(marginalsM3, marginalsM4, marginalsM6, 
		col=c('black', 'red', 'blue'), 
		pch=c(19, 19, 19),
		legend=legend,
		level=nominal, 
		save=T
	)
        #write.csv(marginals, file=sprintf('marginal%s/marginal%s68.csv', s, s), row.names=F)
}


