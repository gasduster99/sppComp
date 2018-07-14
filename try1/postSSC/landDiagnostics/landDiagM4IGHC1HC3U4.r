rm(list=ls())

#
library(RJDBC)
library(RColorBrewer)

#
#FUNCTIONS
#

#
addDF = function(df1, df2){
	#
	nome1 = colnames(df1)
	nome2 = colnames(df2)
	#
	nome = unique(c(nome1, nome2))
	df = matrix(0, nrow=dim(df1)[1], ncol=length(nome))
	colnames(df) = nome
	#
	for(n in nome1){ df[,n] = df[,n] + df1[,n] }
	for(n in nome2){ df[,n] = df[,n] + df2[,n] }
	#
	return(df)
}

#
subRight = function(x, i){
	substr(x, i, nchar(x))
}

#
expandDistByMcatYear = function(landings, runPaths, portGold, gearGold, yearGold, qtrGold){
	#
	landAgg = aggregate(landings$weight, by=list(mcat=landings$mcat, year=landings$year, qtr=landings$qtr, gear=landings$gear, port=landings$port), FUN=sum)
	colnames(landAgg)[dim(landAgg)[2]] = 'weight'
	#
	mySum = list()
	mygSum = list()
	for(run in runPaths){
		#
	        mcat = substring(strsplit(run, '//')[[1]][2], 1, 3)
		#
		runP = sprintf('%s%s%s', mcat, min(yearGold), max(yearGold))		

		#
	        yearEff = sort(unique(landAgg[landAgg$mcat==mcat,'year']))
	        qtrEff = as.numeric(sort(unique(landAgg[landAgg$mcat==mcat,'qtr'])))
	        gearEff = unique(landAgg[landAgg$mcat==mcat,'gear'])
	        portEff = unique(landAgg[landAgg$mcat==mcat,'port'])
	        #
	        yearEff = yearEff[yearEff%in%yearGold]
	        qtrEff = qtrEff[qtrEff%in%qtrGold]
	        gearEff = gearEff[gearEff%in%gearGold]
	        portEff = portEff[portEff%in%portGold]
	      	
	        #
	        mySum[[mcat]] = list()
	        mygSum[[mcat]] = list()
	        for(y in yearEff){mySum[[mcat]][[y]]=0; mygSum[[mcat]][[y]]=list();
	        for(g in gearEff){mygSum[[mcat]][[y]][[g]]=0
	        for(p in portEff){
	        for(q in  qtrEff){
	                #
	                sp = read.csv(sprintf('%s%s/%s/%s/%s/%s/sppComp.csv', run, runP, p, g, q, y))
	                #sp = sp[,colnames(sp)%in%sppList]
	                l = landAgg[landAgg$mcat==mcat & landAgg$port==p & landAgg$gear==g & landAgg$qtr==q & landAgg$year==y,'weight']
	                if(length(l)==0){ l=0 }
	                ld = sp*l
	                #
	                mySum[[mcat]][[y]] = mySum[[mcat]][[y]] + ld
	                mygSum[[mcat]][[y]][[g]] = mygSum[[mcat]][[y]][[g]] + ld
	        }}}} 
	}
	#
	return( list(mySum=mySum, mygSum=mygSum) )
}

#
sumDistByYear = function(expandDists, yearGold){
	#
	ySum = lapply(yearGold, function(x){data.frame(BCAC=rep(0, 10000), WDOW=rep(0, 10000))})
	names(ySum) = yearGold
	ygSum = lapply(yearGold, function(x){list(
		TWL = data.frame(BCAC=rep(0, 10000), WDOW=rep(0, 10000)),
		HKL = data.frame(BCAC=rep(0, 10000), WDOW=rep(0, 10000)),
		NET = data.frame(BCAC=rep(0, 10000), WDOW=rep(0, 10000))
	)})
	names(ygSum) = yearGold
	#
	for(mcat in names(expandDists$mySum)){
		years = names(expandDists$mySum[[mcat]])	
		for(y in years){
			ySum[[y]] = addDF(ySum[[y]], expandDists$mySum[[mcat]][[y]])
			gears = names(expandDists$mygSum[[mcat]][[y]])
			for(g in gears){
				ygSum[[y]][[g]] = addDF(ygSum[[y]][[g]], expandDists$mygSum[[mcat]][[y]][[g]])
			}
		}
	}
	#
	return( list(ySum=ySum, ygSum=ygSum) )
}

#
summarizeByYear = function(yearDists, yearGold){
	#
	ySummary = list()
	ygSummary = list()
	for(y in as.character(yearGold)){
	        #
	        ySummary[[y]] = apply(yearDists$ySum[[y]], 2, function(x){c(quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9)), mean(x))})
	        #
		gears = names(yearDists$ygSum[[y]])
	        ygSummary[[y]] = list()
	        for(g in gears){
	                ygSummary[[y]][[g]] = apply(yearDists$ygSum[[y]][[g]], 2, function(x){c(quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9)), mean(x))})
	        }
	}
	#
	return( list(ySummary=ySummary, ygSummary=ygSummary) )
}

#
plotLands = function(..., dirName, yearGoldF=yearGold, comSppYearF=comSppYear, comSppGearYearF=comSppGearYear, sppListF=sppList, col='blue', alpha=50, legend='', calcomCol='black'){
	#
	dir = dirName	
	dir.create(dir)
	#
	legend = c(legend, 'CALCOM')
	#
	cols = col
	summarizeDistYs = list(...)
	K = length(summarizeDistYs)
	for(s in sppListF){
		maxer = c()
		for(k in 1:K){
			#
			summarizeDistY = summarizeDistYs[[k]]
                        #
                        ySummary = summarizeDistY$ySummary
                        ygSummary = summarizeDistY$ygSummary
			toptop = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['10%',s]})
			top = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['25%',s]})
			med = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['50%',s]})
			mean = sapply(as.character(yearGoldF), function(x){ySummary[[x]][6,s]})
			blu = comSppYearF[comSppYearF$species==s, 'x']
			bot = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['75%',s]})
			botbot = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['90%',s]})
			#
			#maxer = c(maxer, toptop, top, med, mean, blu, bot, botbot)
			maxer = c(maxer, max(toptop), max(top), max(med), max(mean), max(blu), max(bot), max(botbot))
		}
		for(k in 1:K){
			#	
			col = cols[k]
			summarizeDistY = summarizeDistYs[[k]]
			#
			ySummary = summarizeDistY$ySummary
			ygSummary = summarizeDistY$ygSummary		
			##10%, 25%, 50%, 75%, 90%, mean
			#sapply(as.character(yearGoldF), function(x){ySummary[[x]]['50%','BCAC']})
			toptop = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['10%',s]})
			top = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['25%',s]})
			med = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['50%',s]})
			mean = sapply(as.character(yearGoldF), function(x){ySummary[[x]][6,s]})
			blu = comSppYearF[comSppYearF$species==s, 'x']
			bot = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['75%',s]})
			botbot = sapply(as.character(yearGoldF), function(x){ySummary[[x]]['90%',s]})
			#
			if(k==1){
				#	
				dir.create(sprintf('%s%s', dir, s))
				pdf(sprintf('%s%s/year%s.pdf', dir, s, s))
				plot(yearGoldF, mean, type='l', 
					ylab='Landings (mt)',
					xlab='Year',
					col=col, #adjustcolor(col, alpha),
					main=s,
					ylim=c(0, max(maxer))
				)
			}
			polygon(c(yearGoldF, rev(yearGoldF)), c(toptop, rev(botbot)), border=NA,
				col = rgb(col2rgb(col)[1], col2rgb(col)[2], col2rgb(col)[3], alpha=alpha, maxColorValue=255) #adjustcolor(col, alpha)
			)
			#polygon(c(yearGoldF, rev(yearGoldF)), c(top, rev(bot)), col='grey30', border=NA)
			lines(yearGoldF, mean, lwd=3, col=col)
			lines(yearGoldF, med, lty=2, lwd=3, col=col)
			lines(comSppYearF[comSppYearF$species==s, 'year'], blu, col=calcomCol, lwd=3)
			points(comSppYearF[comSppYearF$species==s, 'year'], blu, col=calcomCol, pch=20, cex=2)	
		}
		legend('topleft', legend=legend, fill=c(cols, calcomCol))
		dev.off()
	}	
	#
	for(s in sppListF){
	for(g in gearGold){
		maxer = c()
		for(k in 1:K){
			#
			summarizeDistY = summarizeDistYs[[k]]
                        #
                        ySummary = summarizeDistY$ySummary
                        ygSummary = summarizeDistY$ygSummary
			#
			toptop = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['10%',s]})
			top = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['25%',s]})
			med = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['50%',s]})
			mean = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]][6,s]})
			blu = comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'x']
			bot = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['75%',s]})
			botbot = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['90%',s]})
			#
			maxer = c(maxer, max(toptop), max(top), max(med), max(mean), max(blu), max(bot), max(botbot))
		}
		for(k in 1:K){
			#	
			col = cols[k]
			summarizeDistY = summarizeDistYs[[k]]
			#
			ySummary = summarizeDistY$ySummary
			ygSummary = summarizeDistY$ygSummary
		
			##10%, 25%, 50%, 75%, 90%, mean
			#sapply(as.character(yearGoldF), function(x){ySummary[[x]]['50%','BCAC']})
			toptop = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['10%',s]})
			top = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['25%',s]})
			med = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['50%',s]})
			mean = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]][6,s]})
			blu = comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'x']
			bot = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['75%',s]})
			botbot = sapply(as.character(yearGoldF), function(x){ygSummary[[x]][[g]]['90%',s]})
			#
			if(k==1){
				#       
				pdf(sprintf('%s%s/yearGear%s-%s.pdf', dir, s, s, g))
				plot(yearGoldF, mean, type='l',
				        ylab='Landings (mt)',
				        xlab='Year',
				        col=col, #adjustcolor(col, alpha),
					main=sprintf('%s:%s', s, g),
				        ylim=c(0, max(maxer))
				)
			}
			polygon(c(yearGoldF, rev(yearGoldF)), c(toptop, rev(botbot)), border=NA,
				col = rgb(col2rgb(col)[1], col2rgb(col)[2], col2rgb(col)[3], alpha=alpha, maxColorValue=255) #adjustcolor(col, alpha)
			)
			#polygon(c(yearGoldF, rev(yearGoldF)), c(top, rev(bot)), col='grey30', border=NA)
			lines(yearGoldF, mean, lwd=3, col=col)
			lines(yearGoldF, med, lty=2, lwd=3, col=col)
			lines(comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'year'], blu, col=calcomCol, lwd=3)
			points(comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'year'], blu, col=calcomCol, pch=20, cex=2)
		}
		legend('topleft', legend=legend, fill=c(cols, calcomCol))
		dev.off()
	}}
}


#
#KNOBS
#

#
minYear = 1978
maxYear = 1982
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')
sppList = c('WDOW', 'BCAC', 'CLPR', 'BANK', 'YTRK', 'BLGL', 'DBRK', 'CNRY', 'SNOS', 'CWCD', 'POP', 'BRNZ', 'MXRF') #'CMEL')
#
mcats = c(250, 253, 269)

#
#LANDINGS
#

#driver
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', '../sqljdbc4.jar', identifier.quote="'");
#connection
ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=COMX_DB', 'nick.grunloh', 'Nmfsswfsc!2018')
#call
land = dbGetQuery(ch,
        sprintf("
        select
               mark_cat as mcat, 
               year, 
               quarter as qtr,
               gear_grp as gear, 
               port_complex as port, 
               species, 
               pounds as weight 

        FROM [COMX_DB].[dbo].[COM_LANDS]

        where year >= %d and year <= %d and live='N'
        ", minYear, maxYear)
)

#
land = land[land$mcat%in%mcats,]
land = land[land$gear%in%gearGold,]
land = land[land$port%in%portGold,]
land = land[land$year%in%yearGold,]
land = land[land$qtr%in%qtrGold,]
#convert landings to metric tons
land$weight = land$weight/2204.62

#
comSppYear = aggregate(land$weight, by=list(year=land$year, species=land$species), FUN=sum)
comSppGearYear = aggregate(land$weight, by=list(year=land$year, species=land$species, gear=land$gear), FUN=sum)

#
#APPLY
#

#
legend = c()
cols = brewer.pal(9, 'Set1')
globPath = "/media/nick/extraBig/"

##
#runPaths = Sys.glob(sprintf("%s/*%s%sM2/", globPath, minYear, maxYear))
#legend = c(legend, subRight(runPaths[1], 34))
#print(legend)
##
#expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold)
#sumDistY = sumDistByYear(expDistMY, yearGold)
#summarizeDistY00 = summarizeByYear(sumDistY, yearGold)

#
runPaths = Sys.glob(sprintf("%s/*%s%sM4/", globPath, minYear, maxYear))
legend = c(legend, subRight(runPaths[1], 34))
print(legend)
#
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY0 = summarizeByYear(sumDistY, yearGold)

#
runPaths = Sys.glob(sprintf("%s/*%s%sM4HC1/", globPath, minYear, maxYear))
legend = c(legend, subRight(runPaths[1], 34))
print(legend)
#
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY1 = summarizeByYear(sumDistY, yearGold)

#
runPaths = Sys.glob(sprintf("%s/*%s%sM4HC3/", globPath, minYear, maxYear))
legend = c(legend, subRight(runPaths[1], 34))
print(legend)
#
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY2 = summarizeByYear(sumDistY, yearGold)

#
runPaths = Sys.glob(sprintf("%s/*%s%sM4U4/", globPath, minYear, maxYear))
legend = c(legend, subRight(runPaths[1], 34))
print(legend)
#
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY3 = summarizeByYear(sumDistY, yearGold)

#
dirName = 'M4IGHC1HC3U4/' #subRight(runPaths[1], 34)
plotLands(summarizeDistY0, summarizeDistY1, summarizeDistY2, summarizeDistY3, dirName=dirName, col=cols[1:length(legend)], legend=legend)








#
#JUNK YARD
#





##runP = sapply(runPaths, function(x){ substr(strsplit(x, '//')[[1]][2], 1, 11) }) 
#comSppYear = aggregate(land$weight, by=list(year=land$year, species=land$species), FUN=sum)
#comSppGearYear = aggregate(land$weight, by=list(year=land$year, species=land$species, gear=land$gear), FUN=sum)


##
#landAgg = aggregate(land$weight, by=list(mcat=land$mcat, year=land$year, qtr=land$qtr, gear=land$gear, port=land$port), FUN=sum)
#colnames(landAgg)[dim(landAgg)[2]] = 'weight'
#
##
#mySum = list()
#mygSum = list()
#for(run in runPaths){
#	#
#	mcat = substr(runP[run], 1, 3)
#	#
#	yearEff = sort(unique(landAgg[landAgg$mcat==mcat,'year']))
#	qtrEff = as.numeric(sort(unique(landAgg[landAgg$mcat==mcat,'qtr'])))
#	gearEff = unique(landAgg[landAgg$mcat==mcat,'gear'])
#	portEff = unique(landAgg[landAgg$mcat==mcat,'port'])
#	##
#	#print(yearEff)
#	#print(qtrEff)
#	#print(gearEff)
#	#print(portEff)
#	##
#	yearEff = yearEff[yearEff%in%yearGold]
#	qtrEff = qtrEff[qtrEff%in%qtrGold]
#	gearEff = gearEff[gearEff%in%gearGold]
#	portEff = portEff[portEff%in%portGold]
#	##
#	#print(yearEff)
#	#print(qtrEff)
#	#print(gearEff)
#	#print(portEff)
#	
#	#
#	mySum[[mcat]] = list()
#	mygSum[[mcat]] = list()
#	for(y in yearEff){mySum[[mcat]][[y]]=0; mygSum[[mcat]][[y]]=list();
#	for(g in gearEff){mygSum[[mcat]][[y]][[g]]=0
#	for(p in portEff){
#	for(q in  qtrEff){
#		#
#		sp = read.csv(sprintf('%s%s/%s/%s/%s/%s/sppComp.csv', run, runP[run], p, g, q, y))
#		#sp = sp[,colnames(sp)%in%sppList]
#		l = landAgg[landAgg$mcat==mcat & landAgg$port==p & landAgg$gear==g & landAgg$qtr==q & landAgg$year==y,'weight']
#		if(length(l)==0){ l=0 }
#		ld = sp*l
#		#
#		mySum[[mcat]][[y]] = mySum[[mcat]][[y]] + ld
#		mygSum[[mcat]][[y]][[g]] = mygSum[[mcat]][[y]][[g]] + ld
#	}}}}	
#}


##
#ySum = lapply(yearGold, function(x){data.frame(BCAC=rep(0, 10000), WDOW=rep(0, 10000))})
#names(ySum) = yearGold
#ygSum = lapply(yearGold, function(x){list(
#	TWL = data.frame(BCAC=rep(0, 10000), WDOW=rep(0, 10000)),
#	HKL = data.frame(BCAC=rep(0, 10000), WDOW=rep(0, 10000)),
#	NET = data.frame(BCAC=rep(0, 10000), WDOW=rep(0, 10000))
#)})
#names(ygSum) = yearGold
#for(mcat in names(mySum)){
#	years = names(mySum[[mcat]])	
#	for(y in years){
#		ySum[[y]] = addDF(ySum[[y]], mySum[[mcat]][[y]])
#		gears = names(mygSum[[mcat]][[y]])
#		for(g in gears){
#			ygSum[[y]][[g]] = addDF(ygSum[[y]][[g]], mygSum[[mcat]][[y]][[g]])
#		}
#	}
#}


##
#ySummary = list()
#ygSummary = list()
#for(y in as.character(yearGold)){
#	#
#	ySummary[[y]] = apply(ySum[[y]], 2, function(x){c(quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9)), mean(x))})
#	#
#	ygSummary[[y]] = list()
#	for(g in gearGold){
#		ygSummary[[y]][[g]] = apply(ygSum[[y]][[g]], 2, function(x){c(quantile(x, c(0.1, 0.25, 0.5, 0.75, 0.9)), mean(x))})
#	}
#}


##
#dir = sprintf('%sComx/', strsplit(dir, '/')[[1]])
#dir.create(dir)
#for(s in sppList){
#	##10%, 25%, 50%, 75%, 90%, mean
#	#sapply(as.character(yearGold), function(x){ySummary[[x]]['50%','BCAC']})
#	toptop = sapply(as.character(yearGold), function(x){ySummary[[x]]['10%',s]})
#	top = sapply(as.character(yearGold), function(x){ySummary[[x]]['25%',s]})
#	med = sapply(as.character(yearGold), function(x){ySummary[[x]]['50%',s]})
#	mean = sapply(as.character(yearGold), function(x){ySummary[[x]][6,s]})
#	#blu = comSppYear[comSppYear$species==s, 'x']
#	bot = sapply(as.character(yearGold), function(x){ySummary[[x]]['75%',s]})
#	botbot = sapply(as.character(yearGold), function(x){ySummary[[x]]['90%',s]})
#	#	
#	dir.create(sprintf('%s%s', dir, s))
#	pdf(sprintf('%s%s/year%s.pdf', dir, s, s))
#	#
#	plot(yearGold, mean, type='l', 
#		ylab='Landings (mt)',
#		xlab='Year',
#		main=s,
#		ylim=c(0, max(top, toptop, bot, botbot, med, mean))#, blu))
#	)
#	polygon(c(yearGold, rev(yearGold)), c(toptop, rev(botbot)), col='grey', border=NA)
#	polygon(c(yearGold, rev(yearGold)), c(top, rev(bot)), col='grey30', border=NA)
#	lines(yearGold, mean, lwd=3)
#	lines(yearGold, med, lty=2, lwd=3)
#	#lines(comSppYear[comSppYear$species==s, 'year'], blu, col='blue', lwd=3)
#	dev.off()
#	#
#	for(g in gearGold){
#		##10%, 25%, 50%, 75%, 90%, mean
#        	#sapply(as.character(yearGold), function(x){ySummary[[x]]['50%','BCAC']})
#        	toptop = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['10%',s]})
#        	top = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['25%',s]})
#        	med = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['50%',s]})
#        	mean = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]][6,s]})
#		#blu = comSppGearYear[comSppGearYear$species==s & comSppGearYear$gear==g, 'x']
#        	bot = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['75%',s]})
#        	botbot = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['90%',s]})
#		#       
#        	pdf(sprintf('%s%s/yearGear%s-%s.pdf', dir, s, s, g))
#        	#
#        	plot(yearGold, mean, type='l',
#        	        ylab='Landings (mt)',
#        	        xlab='Year',
#        	        main=sprintf('%s:%s', s, g),
#        	        ylim=c(0, max(top, toptop, bot, botbot, med, mean))#, blu))
#        	)
#        	polygon(c(yearGold, rev(yearGold)), c(toptop, rev(botbot)), col='grey', border=NA)
#        	polygon(c(yearGold, rev(yearGold)), c(top, rev(bot)), col='grey30', border=NA)
#        	lines(yearGold, mean, lwd=3)
#        	lines(yearGold, med, lty=2, lwd=3)
#		#lines(comSppGearYear[comSppGearYear$species==s & comSppGearYear$gear==g, 'year'], blu, col='blue', lwd=3)
#        	dev.off()
#	}
#}


###
#dir = subRight(runPaths[1], 34)	
#dir.create(dir)
#for(s in sppList){
#	##10%, 25%, 50%, 75%, 90%, mean
#	#sapply(as.character(yearGold), function(x){ySummary[[x]]['50%','BCAC']})
#	toptop = sapply(as.character(yearGold), function(x){ySummary[[x]]['10%',s]})
#	top = sapply(as.character(yearGold), function(x){ySummary[[x]]['25%',s]})
#	med = sapply(as.character(yearGold), function(x){ySummary[[x]]['50%',s]})
#	mean = sapply(as.character(yearGold), function(x){ySummary[[x]][6,s]})
#	blu = comSppYear[comSppYear$species==s, 'x']
#	bot = sapply(as.character(yearGold), function(x){ySummary[[x]]['75%',s]})
#	botbot = sapply(as.character(yearGold), function(x){ySummary[[x]]['90%',s]})
#	#	
#	dir.create(sprintf('%s%s', dir, s))
#	pdf(sprintf('%s%s/year%s.pdf', dir, s, s))
#	#
#	plot(yearGold, mean, type='l', 
#		ylab='Landings (mt)',
#		xlab='Year',
#		main=s,
#		ylim=c(0, max(top, toptop, bot, botbot, med, mean, blu))
#	)
#	polygon(c(yearGold, rev(yearGold)), c(toptop, rev(botbot)), col='grey', border=NA)
#	#polygon(c(yearGold, rev(yearGold)), c(top, rev(bot)), col='grey30', border=NA)
#	lines(yearGold, mean, lwd=3)
#	lines(yearGold, med, lty=2, lwd=3)
#	lines(comSppYear[comSppYear$species==s, 'year'], blu, col='blue', lwd=3)
#	dev.off()
#	#
#	for(g in gearGold){
#		##10%, 25%, 50%, 75%, 90%, mean
#        	#sapply(as.character(yearGold), function(x){ySummary[[x]]['50%','BCAC']})
#        	toptop = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['10%',s]})
#        	top = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['25%',s]})
#        	med = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['50%',s]})
#        	mean = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]][6,s]})
#		blu = comSppGearYear[comSppGearYear$species==s & comSppGearYear$gear==g, 'x']
#        	bot = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['75%',s]})
#        	botbot = sapply(as.character(yearGold), function(x){ygSummary[[x]][[g]]['90%',s]})
#		#       
#        	pdf(sprintf('%s%s/yearGear%s-%s.pdf', dir, s, s, g))
#        	#
#        	plot(yearGold, mean, type='l',
#        	        ylab='Landings (mt)',
#        	        xlab='Year',
#        	        main=sprintf('%s:%s', s, g),
#        	        ylim=c(0, max(top, toptop, bot, botbot, med, mean, blu))
#        	)
#        	polygon(c(yearGold, rev(yearGold)), c(toptop, rev(botbot)), col='grey', border=NA)
#        	polygon(c(yearGold, rev(yearGold)), c(top, rev(bot)), col='grey30', border=NA)
#        	lines(yearGold, mean, lwd=3)
#        	lines(yearGold, med, lty=2, lwd=3)
#		lines(comSppGearYear[comSppGearYear$species==s & comSppGearYear$gear==g, 'year'], blu, col='blue', lwd=3)
#        	dev.off()
#	}
#}
