rm(list=ls())

#
library(RJDBC)
library(foreach)
library(parallel)
library(doParallel)
library(KernSmooth)
library(HDInterval)

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

load('1978to1982DonComps.RData')

#
#JUSTIFY STRATA
#

#
yMin = substr(minYear, 3, 4)
yMax = substr(maxYear, 3, 4)
mcat = mcats[1]
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
#COMPARE
#

#
adj = 1#4.1/12
prob = 0.99
threads = 8
#
preds = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
#registerDoParallel(cores=threads)
#preds = foreach( p=ports )%dopar%{
for(p in ports){
	#
        end  = 1
        pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
        #
	for(g in gears){
	for(q in qtrs ){
	for(y in years){
		#
		dp = nowComps[nowComps[,'mcat']==mcat & nowComps[,'year']==y & nowComps[,'qtr']==q & nowComps[,'gear']==g & nowComps[,'port']==p,]
		donSpp = dp[,'species'] #<<<< I WAS HERE
		dp = as.numeric(dp[,'comp'])
		names(dp) = donSpp
		#
		lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y))
		lp = lp[!is.na(lp[,1]),]
		#boxplot(lp, ylim=c(0, 0.3))
		for(s in donSpp){#colnames(lp)){
			#
			spIntHDI = hdi(density(lp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
                        spIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
			#
			#spHDI
	                inOut = 0
			for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(dp[s], spIntHDI[i,])==1 | inOut }         
			#inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other interva
                        #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
                        cpHdiMean = mean(inOut)
                        #
                        #spCI
                        inOut = 0
			for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(dp[s], spIntCI[i,])==1 | inOut }
			#inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other interval
                        #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
                        cpCiMean = mean(inOut)
			#
			pred[end,] = c(as.character(p), as.character(g), q, y, as.character(s), cpHdiMean, cpCiMean)
			end = end+1
		}
	}}}
	#return(pred)
	preds = rbind(preds, pred)
	
}
##reduce preds
#preds = do.call(rbind, preds)



