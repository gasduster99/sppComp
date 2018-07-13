rm(list=ls())

#
#FUNCTIONS
#

#
goodRatios = function( loggedStuff ){
        #loggedStuff: the log(x) of the values (x) to compute the following ratio
        #       r_i = x_i/sum(x)

        #
        if(any(is.na(loggedStuff))){
                #
                warning("'NA' present in loggedStuff")
                naWhere = which(is.na(loggedStuff))
                loggedStuff = loggedStuff[!is.na(loggedStuff)]
                #
                c = max(loggedStuff)
                stand = exp(loggedStuff-c)
                sStand = sum(stand)
                out = stand/sStand
                #
                for(na in naWhere){ out=append(out, 0, na-1) }
                return( matrix(out) )

        }
        #
        c = max(loggedStuff)
        stand = exp(loggedStuff-c)
        sStand = sum(stand)
        out = stand/sStand
        #
        return( out )
}

#
deltaForm = function(icVec){
	#icVec	: a vectos of information criteriorion to delta
	icVec-min(icVec)
}

#
#MAIN
#

#
runs = Sys.glob('26919781982M4*')[c(1,4,5)]
R = length(runs)

#
runMetrics = matrix(NA, nrow=R, ncol=4)
for(r in 1:R){
	met = read.csv(sprintf('%s/metrics.csv', runs[r]))
	runMetrics[r,] = t(t(met))
}
colnames(runMetrics) = colnames(met)
rownames(runMetrics) = runs

#
runMetricsDelta = runMetrics
runMetricsDelta[,'waic'] = round(deltaForm(runMetrics[,'waic']), 2)
runMetricsDelta[,'dic'] = round(deltaForm(runMetrics[,'dic']), 2)
runMetricsDelta[,'mlik'] = round(goodRatios(runMetrics[,'mlik']), 2)
#
writeLines(sprintf('\\(\\Delta\\) DIC & %s \\\\', paste(runMetricsDelta[,'dic'], collapse=' & ')))
writeLines(sprintf('\\(\\Delta\\) WAIC & %s \\\\', paste(runMetricsDelta[,'waic'], collapse=' & ')))
writeLines(sprintf('\\(pr(M|y)\\) & %s \\\\ \\hline', paste(runMetricsDelta[,'mlik'], collapse=' & ')))







