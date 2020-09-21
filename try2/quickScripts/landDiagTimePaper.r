rm(list=ls())

#
library(purrr)
library(RJDBC)
library(RColorBrewer)
suppressMessages(library(foreach, quietly=FALSE))
suppressMessages(library(doParallel, quietly=FALSE))

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
expandDistByMcatYear = function(landings, runPaths, portGold, gearGold, yearGold, qtrGold, threads=48){
	#NOTE: add live here
	landAgg = aggregate(landings$weight, by=list(live=landings$live, mcat=landings$mcat, year=landings$year, qtr=landings$qtr, gear=landings$gear, port=landings$port), FUN=sum)
	colnames(landAgg)[dim(landAgg)[2]] = 'weight'
	
	##
	#mySum = list()
	#mygSum = list()
	#for(run in runPaths){
	#
	cats = sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 4)})
	registerDoParallel(cores=threads)
	out = foreach( i=1:length(cats) )%dopar%{
		#
		cat  = cats[i]
	        mcat = substring(cat, 1, 3)	
		live = substring(cat, 4, 4)
		#
		#cat  = substring(strsplit(run, '//')[[1]][2], 1, 4)
		#runP = sprintf('%s%s%s', mcat, min(yearGold), max(yearGold))

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
	      	
	        ##
	        #mySum[[cat]] = list()
	        #mygSum[[cat]] = list()
		mySum = list()
		mygSum = list()
	        for(y in as.character(yearEff)){ mySum[[y]]=c(0); mygSum[[y]]=list(); #mySum[[cat]][[y]]=c(0); mygSum[[cat]][[y]]=list(); #
	        for(g in gearEff){ mygSum[[y]][[g]]=c(0) #mygSum[[cat]][[y]][[g]]=c(0) #
	        for(p in portEff){
	        for(q in  qtrEff){
	                #
	                sp = read.csv(sprintf('%s/%s/%s/%s/%s/sppComp.csv', runPaths[i], p, g, q, y))
	                #sp = sp[,colnames(sp)%in%sppList]
	                l = landAgg[landAgg$live==live & landAgg$mcat==mcat & landAgg$port==p & landAgg$gear==g & landAgg$qtr==q & landAgg$year==y,'weight']
	                if(length(l)==0){ l=0 }
	                ld = sp*l
			
	                ##
	                #mySum[[cat]][[y]] = mySum[[cat]][[y]] + ld
	                #mygSum[[cat]][[y]][[g]] = mygSum[[cat]][[y]][[g]] + ld
	        	#
	                mySum[[y]] = mySum[[y]] + ld
	                mygSum[[y]][[g]] = mygSum[[y]][[g]] + ld
		}}}}
		#
		return( list(mySum=mySum, mygSum=mygSum) )
	}
	#
	names(out) = cats
	out = out %>% transpose()	
	#
	return( out ) 
	#return( list(mySum=mySum, mygSum=mygSum) )
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
	for(cat in names(expandDists$mySum)){
		years = names(expandDists$mySum[[cat]])	
		for(y in years){
			ySum[[y]] = addDF(ySum[[y]], expandDists$mySum[[cat]][[y]])
			gears = names(expandDists$mygSum[[cat]][[y]])
			for(g in gears){
				ygSum[[y]][[g]] = addDF(ygSum[[y]][[g]], expandDists$mygSum[[cat]][[y]][[g]])
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
fillSummary = function(summarizeDist, yearGoldF=yearGold, gearGoldF=gearGold, sppListF=sppList){
	#years given as a vector of strings

	#
	for(y in yearGoldF){
		#
		ySum = summarizeDist$ySummary[[y]]
		if( !length(ySum) ){
			#
			fill = matrix(0, 6, length(sppListF))
			colnames(fill) = sppNot
			#
			summarizeDist$ySummary[[y]] = fill
			summarizeDist$ygSummary[[y]] = list() #NOTE: I think I need this
			for(g in gearGoldF){ summarizeDist$ygSummary[[y]][[g]]=fill }
			#
			next
		}
		#
		sppNot = sppListF[!sppListF%in%colnames(ySum)]
		fill = matrix(0, nrow(ySum), length(sppNot))
		colnames(fill) = sppNot
		#
		summarizeDist$ySummary[[y]] = cbind(ySum, fill)

		#
		for(g in gearGoldF){
			#
                        summarizeDist$ygSummary[[y]][[g]] = cbind(summarizeDist$ygSummary[[y]][[g]], fill)
		}
	}
	#
	return( summarizeDist )
}

#
plotLands = function(..., dirName, yearGoldF=yearGold, comSppYearF=comSppYear, comSppGearYearF=comSppGearYear, sppListF=sppList, col='blue', alpha=50, legend='', calcomCol=NULL){
	#calcomCol:	NULL turns off calcom line; a color string changes color
	#
	dir = dirName	
	dir.create(dir)
	#
	legend = c(legend, 'CALCOM')
	#
	if( is.null(calcomCol) ){
		#
		legend = legend[-length(legend)]
		#
		comSppYearF$x = -1000
		comSppGearYearF$x = -1000
	}
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
			#
			yearLoop = names(ySummary)
			toptop = unlist(sapply(yearLoop, function(x){ySummary[[x]]['10%',s]})) #print(x); print(s); print(head(ySummary[[x]])); 
			top = unlist(sapply(yearLoop, function(x){ySummary[[x]]['25%',s]}))
			med = unlist(sapply(yearLoop, function(x){ySummary[[x]]['50%',s]}))
			mean = unlist(sapply(yearLoop, function(x){ySummary[[x]][6,s]}))
			blu = comSppYearF[comSppYearF$species==s, 'x']
			bot = unlist(sapply(yearLoop, function(x){ySummary[[x]]['75%',s]}))
			botbot = unlist(sapply(yearLoop, function(x){ySummary[[x]]['90%',s]}))
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
			#ygSummary = summarizeDistY$ygSummary	
			#
			##10%, 25%, 50%, 75%, 90%, mean
			#sapply(as.character(yearGoldF), function(x){ySummary[[x]]['50%','BCAC']})
			yearLoop = names(ySummary)
			toptop = unlist(sapply(yearLoop, function(x){ySummary[[x]]['10%',s]}))
			top = unlist(sapply(yearLoop, function(x){ySummary[[x]]['25%',s]}))
			med = unlist(sapply(yearLoop, function(x){ySummary[[x]]['50%',s]}))
			mean = unlist(sapply(yearLoop, function(x){ySummary[[x]][6,s]}))
			blu = comSppYearF[comSppYearF$species==s, 'x']
			bot = unlist(sapply(yearLoop, function(x){ySummary[[x]]['75%',s]}))
			botbot = unlist(sapply(yearLoop, function(x){ySummary[[x]]['90%',s]}))
			#
			years = as.numeric(names(toptop))	
			if(k==1){
				#	
				dir.create(sprintf('%s%s', dir, s))
				pdf(sprintf('%s%s/year%s.pdf', dir, s, s))
				plot(comSppYearF[comSppYearF$species==s, 'year'], blu, 
					type='l', 
					ylab='Landings (mt)',
					xlab='Year',
					col=col, #adjustcolor(col, alpha),
					main=s,
					ylim=c(0, max(maxer)),
					xlim=c(min(yearGoldF), max(yearGoldF))
				)
			}	
			polygon(c(years, rev(years)), c(toptop, rev(botbot)), border=NA,
				col = rgb(col2rgb(col)[1], col2rgb(col)[2], col2rgb(col)[3], alpha=alpha, maxColorValue=255) #adjustcolor(col, alpha)
			)
			#polygon(c(yearGoldF, rev(yearGoldF)), c(top, rev(bot)), col='grey30', border=NA)
			lines(years, mean, lwd=3, col=col)
			lines(years, med, lty=2, lwd=3, col=col)
			lines(comSppYearF[comSppYearF$species==s, 'year'], blu, col=calcomCol, lwd=3)
			points(comSppYearF[comSppYearF$species==s, 'year'], blu, col=calcomCol, pch=20, cex=2)	
		}
		legend('topright', legend=legend, fill=c(cols, calcomCol))
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
			yearLoop = names(ygSummary)
			toptop = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['10%',s]}))
			top = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['25%',s]}))
			med = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['50%',s]}))
			mean = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]][6,s]}))
			blu = comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'x']
			bot = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['75%',s]}))
			botbot = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['90%',s]}))
			#
			maxer = c(maxer, max(toptop), max(top), max(med), max(mean), max(blu), max(bot), max(botbot))
			#print(maxer)
		}
		for(k in 1:K){
			#	
			col = cols[k]
			summarizeDistY = summarizeDistYs[[k]]
			#
			ySummary = summarizeDistY$ySummary
			ygSummary = summarizeDistY$ygSummary
			#
			yearLoop = names(ygSummary)
			toptop = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['10%',s]}))
			top = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['25%',s]}))
			med = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['50%',s]}))
			mean = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]][6,s]}))
			blu = comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'x']
			bot = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['75%',s]}))
			botbot = unlist(sapply(yearLoop, function(x){ygSummary[[x]][[g]]['90%',s]}))
			#
			years = as.numeric(names(toptop))
			if(k==1){
				#       
				pdf(sprintf('%s%s/yearGear%s-%s.pdf', dir, s, s, g))
				plot(comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'year'], blu,
					type='l',
				        ylab='Landings (mt)',
				        xlab='Year',
				        col=col, #adjustcolor(col, alpha),
					main=sprintf('%s:%s', s, g),
				        ylim=c(0, max(maxer)),
					xlim=c(min(yearGoldF), max(yearGoldF))
				)
			}
			polygon(c(years, rev(years)), c(toptop, rev(botbot)), border=NA,
				col = rgb(col2rgb(col)[1], col2rgb(col)[2], col2rgb(col)[3], alpha=alpha, maxColorValue=255) #adjustcolor(col, alpha)
			)
			#polygon(c(yearGoldF, rev(yearGoldF)), c(top, rev(bot)), col='grey30', border=NA)
			lines(years, mean, lwd=3, col=col)
			lines(years, med, lty=2, lwd=3, col=col)
			lines(comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'year'], blu, col=calcomCol, lwd=3)
			points(comSppGearYearF[comSppGearYearF$species==s & comSppGearYearF$gear==g, 'year'], blu, col=calcomCol, pch=20, cex=2)
		}
		legend('topright', legend=legend, fill=c(cols, calcomCol))
		dev.off()
	}}
}

#
#1978-1982 NORTH
#

#NOTE: change years and port appropriatly (?spp?)
minYear = 1978
maxYear = 1982
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')
sppList = c('WDOW', 'BCAC', 'CLPR', 'BANK', 'YTRK', 'BLGL', 'DBRK', 'CNRY', 'SNOS', 'CWCD', 'POP', 'BRNZ', 'MXRF') #'CMEL')
##NOTE: expand MCATS or remove 
#mcats = c(250, 253, 269)
#
cols = brewer.pal(9, 'Set1')
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
#LANDINGS
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'V1', 'V2'), stringsAsFactors=F)
where = land$year>=minYear & land$year<=maxYear  #land$live=='N' & #NOTE: remove live where 
land = land[where, c('live', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'weight')]

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
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold, threads=16)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY7882 = summarizeByYear(sumDistY, yearGold)

#
writeLines('1978-1982 Done.')

#
#1983-1990 NORTH
#

#NOTE: change years and port appropriatly (?spp?)
minYear = 1983
maxYear = 1990
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')#, 'OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')
sppList = c('WDOW', 'BCAC', 'CLPR', 'BANK', 'YTRK', 'BLGL', 'DBRK', 'CNRY', 'SNOS', 'CWCD', 'POP', 'BRNZ', 'MXRF') #'CMEL')
##NOTE: expand MCATS or remove 
#mcats = c(250, 253, 269)
#
cols = brewer.pal(9, 'Set1')
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
#LANDINGS
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'V1', 'V2'), stringsAsFactors=F)
where = land$year>=minYear & land$year<=maxYear  #land$live=='N' & #NOTE: remove live where 
land = land[where, c('live', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'weight')]

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
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold, threads=16)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY8390 = summarizeByYear(sumDistY, yearGold)

#
writeLines('1983-1990 Done.')

#
#1991-2001 NORTH
#

#NOTE: change years and port appropriatly (?spp?)
minYear = 1991
maxYear = 2001
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO')#, 'OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')
sppList = c('WDOW', 'BCAC', 'CLPR', 'BANK', 'YTRK', 'BLGL', 'DBRK', 'CNRY', 'SNOS', 'CWCD', 'POP', 'BRNZ', 'MXRF') #'CMEL')
##NOTE: expand MCATS or remove 
#mcats = c(250, 253, 269)
#
cols = brewer.pal(9, 'Set1')
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
#LANDINGS
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'V1', 'V2'), stringsAsFactors=F)
where = land$year>=minYear & land$year<=maxYear  #land$live=='N' & #NOTE: remove live where 
land = land[where, c('live', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'weight')]

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
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold, threads=16)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY9101 = summarizeByYear(sumDistY, yearGold)

#
writeLines('1991-2001 Done.')

#
#COMBINE NORTH
#


summarizeDistY7801 = summarizeDistY7882
for(x in as.character(1983:1990)){
        #
        summarizeDistY7801$ySummary[[x]] = summarizeDistY8390$ySummary[[x]]
        for(g in gearGold){
                summarizeDistY7801$ygSummary[[x]][[g]] = summarizeDistY8390$ygSummary[[x]][[g]]
        }
}
for(x in as.character(yearGold)){
        #
        summarizeDistY7801$ySummary[[x]] = summarizeDistY9101$ySummary[[x]]
        for(g in gearGold){
                summarizeDistY7801$ygSummary[[x]][[g]] = summarizeDistY9101$ygSummary[[x]][[g]]
        }
}

#
#1983-1990 SOUTH
#

#NOTE: change years and port appropriatly (?spp?)
minYear = 1983
maxYear = 1990
portGold = c('OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')
sppList = c('WDOW', 'BCAC', 'CLPR', 'BANK', 'YTRK', 'BLGL', 'DBRK', 'CNRY', 'SNOS', 'CWCD', 'POP', 'BRNZ', 'MXRF') #'CMEL')
##NOTE: expand MCATS or remove 
#mcats = c(250, 253, 269)
#
cols = brewer.pal(9, 'Set1')
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
#LANDINGS
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'V1', 'V2'), stringsAsFactors=F)
where = land$year>=minYear & land$year<=maxYear  #land$live=='N' & #NOTE: remove live where 
land = land[where, c('live', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'weight')]

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
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold, threads=16)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY8390 = summarizeByYear(sumDistY, yearGold)

#
writeLines('1983-1990 South Done.')

#
#1991-2001 SOUTH
#

#NOTE: change years and port appropriatly (?spp?)
minYear = 1991
maxYear = 2001
portGold = c('OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')
sppList = c('WDOW', 'BCAC', 'CLPR', 'BANK', 'YTRK', 'BLGL', 'DBRK', 'CNRY', 'SNOS', 'CWCD', 'POP', 'BRNZ', 'MXRF') #'CMEL')
##NOTE: expand MCATS or remove 
#mcats = c(250, 253, 269)
#
cols = brewer.pal(9, 'Set1')
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#
#LANDINGS
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'V1', 'V2'), stringsAsFactors=F)
where = land$year>=minYear & land$year<=maxYear  #land$live=='N' & #NOTE: remove live where 
land = land[where, c('live', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'weight')]

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
expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold, threads=16)
sumDistY = sumDistByYear(expDistMY, yearGold)
summarizeDistY9101 = summarizeByYear(sumDistY, yearGold)

#
writeLines('1991-2001 South Done.')

#
#COMBINE SOUTH
#

#fill in holes
summarizeDistY8390 = fillSummary(summarizeDistY8390, yearGoldF=as.character(1983:1990))
summarizeDistY9101 = fillSummary(summarizeDistY9101, yearGoldF=as.character(1991:2001))

#
summarizeDistY8301S = summarizeDistY8390

#combine justified versions of 83-90 with 91-01
for(x in as.character(1991:2001)){
	#
	summarizeDistY8301S$ySummary[[x]] = summarizeDistY9101$ySummary[[x]]
	#
	for(g in gearGold){ 
		summarizeDistY8301S$ygSummary[[x]][[g]] = summarizeDistY9101$ygSummary[[x]][[g]] 
	}
}

#
#PLOT
#

#
dirName = 'M4SPIG78to01Split/' #subRight(runPaths[1], 34)
legend = c('North', 'South') 
plotLands(summarizeDistY7801, summarizeDistY8301S, dirName=dirName, yearGoldF=1978:2001, col=cols[1:length(legend)], legend=legend, calcomCol=NULL) 

#
summarizeDistY8390$ySummary[['1983']] = NULL
summarizeDistY8390$ygSummary[['1983']] = NULL
#
dirName = 'M4SPIG78to01SplitNo83/' #subRight(runPaths[1], 34)
legend = c('North', 'South')
plotLands(summarizeDistY7801, summarizeDistY8301S, dirName=dirName, yearGoldF=1978:2001, col=cols[1:length(legend)], legend=legend, calcomCol=NULL)










#
#JUNK
#


##combine data structures
#summarizeDistY7801 = summarizeDistY9101
#summarizeDistY7801$ySummary$`1978` = summarizeDistY7882$ySummary$`1978`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1978`[[g]] = summarizeDistY7882$ygSummary$`1978`[[g]] }
#summarizeDistY7801$ySummary$`1979` = summarizeDistY7882$ySummary$`1979`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1979`[[g]] = summarizeDistY7882$ygSummary$`1979`[[g]] }
#summarizeDistY7801$ySummary$`1980` = summarizeDistY7882$ySummary$`1980`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1980`[[g]] = summarizeDistY7882$ygSummary$`1980`[[g]] }
#summarizeDistY7801$ySummary$`1981` = summarizeDistY7882$ySummary$`1981`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1981`[[g]] = summarizeDistY7882$ygSummary$`1981`[[g]] }
#summarizeDistY7801$ySummary$`1982` = summarizeDistY7882$ySummary$`1982`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1982`[[g]] = summarizeDistY7882$ygSummary$`1982`[[g]] }
#summarizeDistY7801$ySummary$`1983` = summarizeDistY8390$ySummary$`1983`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1983`[[g]] = summarizeDistY8390$ygSummary$`1983`[[g]] }
#summarizeDistY7801$ySummary$`1984` = summarizeDistY8390$ySummary$`1984`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1984`[[g]] = summarizeDistY8390$ygSummary$`1984`[[g]] }
#summarizeDistY7801$ySummary$`1985` = summarizeDistY8390$ySummary$`1985`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1985`[[g]] = summarizeDistY8390$ygSummary$`1985`[[g]] }
#summarizeDistY7801$ySummary$`1986` = summarizeDistY8390$ySummary$`1986`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1986`[[g]] = summarizeDistY8390$ygSummary$`1986`[[g]] }
#summarizeDistY7801$ySummary$`1987` = summarizeDistY8390$ySummary$`1987`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1987`[[g]] = summarizeDistY8390$ygSummary$`1987`[[g]] }
#summarizeDistY7801$ySummary$`1988` = summarizeDistY8390$ySummary$`1988`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1988`[[g]] = summarizeDistY8390$ygSummary$`1988`[[g]] }
#summarizeDistY7801$ySummary$`1989` = summarizeDistY8390$ySummary$`1989`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1989`[[g]] = summarizeDistY8390$ygSummary$`1989`[[g]] }
#summarizeDistY7801$ySummary$`1990` = summarizeDistY8390$ySummary$`1990`; for(g in gearGold){ summarizeDistY7801$ygSummary$`1990`[[g]] = summarizeDistY8390$ygSummary$`1990`[[g]] }






	      #summarizeDistY8301S = summarizeDistY9101
#summarizeDistY8301S$ySummary = sapply(as.character(yearGold), function(x){ summarizeDistY8301S$ySummary[[x]][,colnames(summarizeDistY8390$ySummary[["1983"]])] })
#summarizeDistY8301S$ygSummary = sapply(as.character(yearGold), function(x){ for(g in gearGold){ summarizeDistY8301S$ygSummary[[x]][[g]][,colnames(summarizeDistY8390$ySummary[["1983"]])] } })
#summarizeDistY8301S$ySummary$`1983` = summarizeDistY8390$ySummary$`1983`; for(g in gearGold){ summarizeDistY8301S$ygSummary$`1983`[[g]] = summarizeDistY8390$ygSummary$`1983`[[g]] }
#summarizeDistY8301S$ySummary$`1984` = summarizeDistY8390$ySummary$`1984`; for(g in gearGold){ summarizeDistY8301S$ygSummary$`1984`[[g]] = summarizeDistY8390$ygSummary$`1984`[[g]] }
#summarizeDistY8301S$ySummary$`1985` = summarizeDistY8390$ySummary$`1985`; for(g in gearGold){ summarizeDistY8301S$ygSummary$`1985`[[g]] = summarizeDistY8390$ygSummary$`1985`[[g]] }
#summarizeDistY8301S$ySummary$`1986` = summarizeDistY8390$ySummary$`1986`; for(g in gearGold){ summarizeDistY8301S$ygSummary$`1986`[[g]] = summarizeDistY8390$ygSummary$`1986`[[g]] }
#summarizeDistY8301S$ySummary$`1987` = summarizeDistY8390$ySummary$`1987`; for(g in gearGold){ summarizeDistY8301S$ygSummary$`1987`[[g]] = summarizeDistY8390$ygSummary$`1987`[[g]] }
#summarizeDistY8301S$ySummary$`1988` = summarizeDistY8390$ySummary$`1988`; for(g in gearGold){ summarizeDistY8301S$ygSummary$`1988`[[g]] = summarizeDistY8390$ygSummary$`1988`[[g]] }
#summarizeDistY8301S$ySummary$`1989` = summarizeDistY8390$ySummary$`1989`; for(g in gearGold){ summarizeDistY8301S$ygSummary$`1989`[[g]] = summarizeDistY8390$ygSummary$`1989`[[g]] }
#summarizeDistY8301S$ySummary$`1990` = summarizeDistY8390$ySummary$`1990`; for(g in gearGold){ summarizeDistY8301S$ygSummary$`1990`[[g]] = summarizeDistY8390$ygSummary$`1990`[[g]] }



##LANDINGS
#land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'V1', 'V2'), stringsAsFactors=F)
#where = land$year>=1978 & land$year<=2001  #land$live=='N' & #NOTE: remove live where 
#land = land[where, c('live', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'weight')]
##
#land = land[land$mcat%in%mcats,]
#land = land[land$gear%in%gearGold,]
#land = land[land$port%in%portGold,]
#land = land[land$qtr%in%qtrGold,]
##convert landings to metric tons
#land$weight = land$weight/2204.62
##
#comSppYear7801 = aggregate(land$weight, by=list(year=land$year, species=land$species), FUN=sum)



#
#runPaths = Sys.glob(sprintf("%s/*%s%sM4HC1/", globPath, minYear, maxYear))
#legend = c(legend, subRight(runPaths[1], 34))
#print(legend)
##
#expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold)
#sumDistY = sumDistByYear(expDistMY, yearGold)
#summarizeDistY1 = summarizeByYear(sumDistY, yearGold)
#
##
#runPaths = Sys.glob(sprintf("%s/*%s%sM4HC3/", globPath, minYear, maxYear))
#legend = c(legend, subRight(runPaths[1], 34))
#print(legend)
##
#expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold)
#sumDistY = sumDistByYear(expDistMY, yearGold)
#summarizeDistY2 = summarizeByYear(sumDistY, yearGold)
#
##
#runPaths = Sys.glob(sprintf("%s/*%s%sM4U4/", globPath, minYear, maxYear))
#legend = c(legend, subRight(runPaths[1], 34))
#print(legend)
##
#expDistMY = expandDistByMcatYear(land, runPaths, portGold, gearGold, yearGold, qtrGold)
#sumDistY = sumDistByYear(expDistMY, yearGold)
#summarizeDistY3 = summarizeByYear(sumDistY, yearGold)
#
##turn off Calcom line
#comSppYear$x = 0
#comSppGearYear$x = 0
##
#dirName = 'M4IGHC1HC3U4/' #subRight(runPaths[1], 34)
#plotLands(summarizeDistY0, summarizeDistY1, summarizeDistY2, summarizeDistY3, dirName=dirName, col=cols[1:length(legend)], legend=legend, calcomCol=NULL) #rgb(0,0,0, max=255, alpha=0)) #comSppYearF=NULL, )








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
