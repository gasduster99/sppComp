rm(list=ls())

#
library(RJDBC)
library(foreach)
library(parallel)
library(doParallel)
library(KernSmooth)
library(HDInterval)
#
source('predictFunc.r')

##
##DON COMPS
##
#
##
#mcats = c(245, 250, 253, 259, 262, 269, 270, 663, 667, 956, 959, 960, 961) # c(195, 250, 253, 262, 265, 269, 270, 956, 959, 961) #
##
#minYear = 1983 # 1978 #
#maxYear = 1990 # 1982 #
#
##driver
#drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
##connection
#ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=calcom', 'nick.grunloh', 'Nmfsswfsc2017')
##call
#lands = dbGetQuery(ch,
#        sprintf("
#        select
#		source, 
#		mark_cat, 
#		year, 
#		quarter,
#		gear_grp, 
#		port_complex, 
#		species, 
#		pounds 
#
#        FROM [calcom].[dbo].[COM_LANDS]
#
#        where year >= %d and year <= %d and live='N'
#        ", minYear, maxYear)
#)
#lands = lands[lands$mark_cat%in%mcats,]
##
#yearEff = unique(lands$year)
#qtrEff  = unique(lands$quarter)
#portEff = unique(lands$port_complex)
#gearEff = unique(lands$gear_grp)
##
#landsTotals = aggregate(lands$pounds, by=list(lands$mark_cat, lands$year, lands$quarter, lands$gear_grp, lands$port_complex), FUN=sum)
#colnames(landsTotals) = c('mcat', 'year', 'qtr', 'gear', 'port', 'weight')
##
#nowComps = numeric(0) #matrix(NA, ncol=3)
##nowComps = mclapply( mcats, FUN = function(m){
#for(m in mcats){
#	for(y in yearEff){ #print(y)
#	for(q in qtrEff ){ #print(q)
#	for(g in gearEff){ #print(g)
#	for(p in portEff){ #print(p)
#		landsWhere = which(
#			lands$mark_cat==m 	&
#			lands$year==y		&
#			lands$quarter==q	&
#			lands$port_complex==p	&
#			lands$gear_grp==g		
#		)
#		landsTotalsWhere = which(
#			landsTotals$mcat==m 	&
#			landsTotals$year==y	&
#			landsTotals$qtr==q	&
#			landsTotals$port==p	&
#			landsTotals$gear==g
#		)
#		#
#		#print(landsWhere)
#		#print(landsTotalsWhere)
#		#
#		if( length(landsWhere)>0 & length(landsTotalsWhere)>0 ){
#			comp = lands[landsWhere,'pounds']/landsTotals[landsTotalsWhere,'weight']	
#			#gear = lands[lands$mark_cat==m,'gear_group']
#			#year = lands[lands$mark_cat==m,'year']
#			#port = lands[lands$mark_cat==m,'port_complex']
#			#mcat = rep(m, length(comp))
#			#qtr = lands[lands$mark_cat==m,'quarter']
#			src = lands[landsWhere,'source']	
#			spp = lands[landsWhere,'species']
#			#	
#			nowComps = rbind(nowComps, cbind(src, m, y, q, g, p, spp, comp))
#		}
#	}}}}
#}
##}, mc.cores=length(mcats))
##nowComps = do.call(rbind, nowComps)
#colnames(nowComps) = c('source', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'comp')
#nowComps = data.frame(nowComps)
#save(yearEff, qtrEff, gearEff, portEff, mcats, minYear, maxYear, nowComps, file=sprintf('%sto%sDonComps.RData', minYear, maxYear))

#load('1978to1982DonComps.RData')
load('1983to1990DonComps.RData')

#
#JUSTIFY STRATA
#

#78-82:
#	195:	25.7354213751997:
#c(1, 1, 1)
#	250:	90.4605741740214:
#c(0.763736263736264, 0.888888888888889, 0.913003663003663)
#	253:	552.446757838051:
#c(0.842185128983308, 0.893778452200303, 0.893778452200303)
#	262:	474.723559474155:
#c(0.925196850393701, 0.94488188976378, 0.968503937007874)
#	265:	494.084135677841:
#c(0.6784140969163, 0.955947136563877, 1)
#	269:	26.7392001252358:
#c(0.742857142857143, 0.914285714285714, 0.942857142857143)
#	270:	143.41214333129:
#c(0.897590361445783, 0.933734939759036, 0.987951807228916)
#	956:	550.400375834313:
#c(0.806167400881057, 0.863436123348018, 0.876651982378855)
#	959:	645.950609349151:
#c(0.494736842105263, 0.757894736842105, 0.894736842105263)
#	961:	211.93300620584:
#c(0.630434782608696, 0.891304347826087, 1)

#
#83-90:
#	245:	480.645936256886:
#c(0.610972568578554, 0.952618453865337, 0.99002493765586) 
#	250: 	149.173461830064:
#c(0.755274261603376, 0.858649789029536, 0.886286919831224)
#	253:	111.411302674679:
#c(0.852941176470588, 0.903361344537815, 0.911764705882353)
#	259:	19.3071796403047:
#c(0.696113074204947, 0.932862190812721, 0.954063604240283)
#	262:	114.278051835428:
#c(0.771618625277162, 0.911308203991131, 0.953436807095344)
#	269:	87.2936357296876:
#c(0.787709497206704, 0.878957169459963, 0.905027932960894)
#	270:	25.7354213751997:
#c(0.5, 1, 1)
#	663:	25.3657336174068:
#c(0.679611650485437, 0.961165048543689, 0.980582524271845)
#	667:	41.3561643141262:
#c(0.667560321715818, 0.900804289544236, 0.906166219839142)
#	956:	102.434264346351:
#c(0.741711809317443, 0.881906825568797, 0.909425785482123)
#	959:	1089.76024908955:
#c(0.734405531339245, 0.815271306177664, 0.874643018187284)
#	960:	760.431652502663:
#c(0.68525641025641, 0.783333333333333, 0.847435897435897)
#	961:	701.25972263191:
#c(0.784615384615385, 0.887449392712551, 0.937651821862348)


#
threads = 8
search = c(-700, 1200) #c(-1225, 2000) #c(0, 1000)
#
yMin = substr(minYear, 3, 4)
yMax = substr(maxYear, 3, 4)
#mcat = mcats[1]
outs = list()
for(mcat in mcats){
	#
	avgPath = sprintf("/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/%sto%s/MCAT%d/Top/avgModel/", yMin, yMax, mcat)
	dynPath = avgPath
	#
	ports = list.dirs(dynPath, recursive=F)
	ports = unlist(lapply(strsplit(ports, '//'), function(x){x[2]}))
	dynPath = sprintf("%s%s/", dynPath, ports[1])
	# 
	gears = list.dirs(dynPath, recursive=F);
	gears = unlist(lapply(strsplit(gears, '//'), function(x){x[2]}))
	dynPath = sprintf("%s%s/", dynPath, gears[1])
	#
	qtrs = list.dirs(dynPath, recursive=F);
	qtrs = unlist(lapply(strsplit(qtrs, '//'), function(x){x[2]}))
	dynPath = sprintf("%s%s/", dynPath, qtrs[1])
	#
	years = list.dirs(dynPath, recursive=F);
	years = unlist(lapply(strsplit(years, '//'), function(x){x[2]}))
	
	#
	#EVALUATE
	#
	
	#
	#out = optim(100, opt)
	writeLines( sprintf('%s:', mcat) )
	outs[[mcat]] = optimize(postOpt, search, tol=1, mcat=mcat)
	#if( out$converge==0 ){ outs[[mcat]]=out }
}

#
save.image(sprintf('%sto%sOut.RData', yMin, yMax))












##
#adj = 166 #4.1/12
#prob = 0.99
#threads = 8
##
#preds = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
#registerDoParallel(cores=threads)
#preds = foreach( p=ports )%dopar%{
##for(p in ports){
#	#
#        end  = 1
#        pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
#        #
#	for(g in gears){
#	for(q in qtrs ){
#	for(y in years){
#		#
#		dp = nowComps[nowComps[,'mcat']==mcat & nowComps[,'year']==y & nowComps[,'qtr']==q & nowComps[,'gear']==g & nowComps[,'port']==p,]
#		donSpp = dp[,'species'] #<<<< I WAS HERE
#		dp = as.numeric(dp[,'comp'])
#		names(dp) = donSpp
#		#
#		lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y))
#		lp = lp[!is.na(lp[,1]),]
#		#boxplot(lp, ylim=c(0, 0.3))
#		for(s in donSpp[donSpp%in%colnames(lp)]){ #NOTE: donSpp){ some species that are not in my numbers
#			#
#			spIntHDI = hdi(density(lp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
#                        spIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#			#
#			#spHDI
#	                inOut = 0
#			for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(dp[s], spIntHDI[i,], rightmost.closed=T)==1 | inOut }         
#			#inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other interva
#                        #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
#                        cpHdiMean = mean(inOut)
#                        #
#                        #spCI
#                        inOut = 0
#			for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(dp[s], spIntCI[i,], rightmost.closed=T)==1 | inOut }
#			#inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other interval
#                        #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
#                        cpCiMean = mean(inOut)
#			#
#			pred[end,] = c(as.character(p), as.character(g), q, y, as.character(s), cpHdiMean, cpCiMean)
#			end = end+1
#		}
#	}}}
#	return(pred)
#	#preds = rbind(preds, pred)
#}
##reduce preds
#preds = do.call(rbind, preds)
#preds$propPostHDI = as.numeric(preds$propPostHDI)
#preds$propPostCI = as.numeric(preds$propPostCI)
##
#print(mean(preds$propPostHDI))

##
#opt = function(adj, mcat){
#	#
#	probs = c(0.68, 0.95, 0.99)
#	registerDoParallel(cores=threads)
#	#
#	sqErrs = c()
#	rates = c()
#	for(prob in probs){
#		preds = foreach( p=ports )%dopar%{
#		#for(p in ports){
#			#
#		        end  = 1
#		        pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
#		        #
#			for(g in gears){
#			for(q in qtrs ){
#			for(y in years){
#				#
#				dp = nowComps[nowComps[,'mcat']==mcat & nowComps[,'year']==y & nowComps[,'qtr']==q & nowComps[,'gear']==g & nowComps[,'port']==p,]
#				donSpp = dp[,'species'] #<<<< I WAS HERE
#				dp = as.numeric(dp[,'comp'])
#				names(dp) = donSpp
#				#
#				lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y))
#				lp = lp[!is.na(lp[,1]),]
#				#boxplot(lp, ylim=c(0, 0.3))
#				for(s in donSpp[donSpp%in%colnames(lp)]){ #NOTE: donSpp){ some species that are not in my numbers
#					#
#					spIntHDI = hdi(density(lp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
#		                        spIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#					#
#					#spHDI
#			                inOut = 0
#					for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(dp[s], spIntHDI[i,], rightmost.closed=T)==1 | inOut }         
#					#inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other interva
#		                        #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
#		                        cpHdiMean = mean(inOut)
#		                        #
#		                        #spCI
#		                        inOut = 0
#					for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(dp[s], spIntCI[i,], rightmost.closed=T)==1 | inOut }
#					#inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other interval
#		                        #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
#		                        cpCiMean = mean(inOut)
#					#
#					pred[end,] = c(as.character(p), as.character(g), q, y, as.character(s), cpHdiMean, cpCiMean)
#					end = end+1
#				}
#			}}}
#			return(pred)
#			#preds = rbind(preds, pred)
#		}
#		#reduce preds
#		preds = do.call(rbind, preds)
#		preds$propPostHDI = as.numeric(preds$propPostHDI)
#		preds$propPostCI = as.numeric(preds$propPostCI)
#		#
#		sqErrs = c(sqErrs, (prob-mean(preds$propPostHDI))^2)
#		rates = c(rates, mean(preds$propPostHDI))
#	}
#	#
#	writeLines(sprintf('%s:', adj))
#	print(rates)
#	writeLines('')
#	#
#	return( mean(sqErrs) )
#}













