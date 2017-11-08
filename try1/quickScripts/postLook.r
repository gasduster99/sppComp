rm(list=ls())

#
library(RJDBC)
library(parallel)

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
#save(yearEff, qtrEff, gearEff, portEff, mcats, minYear, maxYear, nowComps, file=sprintf('%sto%sDonComps.RData', minYear, maxYear))

load('1978to1982DonComps.RData')

#
#COMPARE
#

#




