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
##'WDOW' #BCAC
#spp = "CLPR" #"CNRY" #"YTRK" #"CWCD" #"YEYE" #"BLGL" #"GSRK" #"SNOS" #"BDRK" # 
##
#mcats = c(195, 250, 253, 262, 265, 269, 270, 956, 959, 961)
#minYear = 1978
#maxYear = 1982
###245 in 1983,
##mcats = c(250, 253, 259, 262, 269, 270, 663, 667, 956, 959, 960, 961)
##minYear = 1983 
##maxYear = 1990 
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
#lands = lands[!lands$port_complex=='OCA',]
#lands = lands[lands$mark_cat%in%mcats,]
##aggregate gear to OTH=UNK+FPT; TWL=TWL+MDT; HKL; NET
#lands$gear_grp[lands$gear_grp=='MDT'] = 'TWL'
#lands$gear_grp[lands$gear_grp=='FPT'] = 'OTH'
#lands$gear_grp[lands$gear_grp=='UNK'] = 'OTH'
##
#yearEff = sort(unique(lands$year))
#qtrEff  = sort(unique(lands$quarter))
#portEff = unique(lands$port_complex)
#gearEff = unique(lands$gear_grp)
#
##
#threads = length(gearEff) #10 #8
#registerDoParallel(cores=threads)
##
#M = 10000
# #'BCAC'
##gear = 'TWL'
#yMin = substr(minYear, 3, 4)
#yMax = substr(maxYear, 3, 4)
##      
#sLand = lands[lands$species==spp,]
#gyLand = aggregate(sLand$pounds, by=list(sLand$gear_grp, sLand$year), FUN=sum)
#colnames(gyLand) = c('gear', 'year', 'land')
##
#mpgyqLand = aggregate(lands$pounds, by=list(lands$mark_cat, lands$port_complex, lands$gear_grp, lands$year, lands$quarter), FUN=sum)
#colnames(mpgyqLand) = c('mcat', 'port', 'gear', 'year', 'qtr', 'land')
##
#runByGearYearPost = list()
#runByGearYearPred = list()
#for(g in gearEff){
#	runByGearYearPost[[g]] = list()
#        runByGearYearPred[[g]] = list()
#	for(y in yearEff){
#	        runByGearYearPost[[g]][[y]] = 0
#	        runByGearYearPred[[g]][[y]] = 0
#	}
#}
##
#for(mcat in mcats){
#	avgPath = sprintf("/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/%sto%s/MCAT%d/Top/avgModel/", yMin, yMax, mcat)
#	dynPath = avgPath
#	#
#	ports = list.dirs(dynPath, recursive=F)
#	ports = unlist(lapply(strsplit(ports, '//'), function(x){x[2]}))
#	dynPath = sprintf("%s%s/", dynPath, ports[1])
#	# 
#	gears = list.dirs(dynPath, recursive=F);
#	gears = unlist(lapply(strsplit(gears, '//'), function(x){x[2]}))
#	dynPath = sprintf("%s%s/", dynPath, gears[1])
#	#
#	qtrs = list.dirs(dynPath, recursive=F);
#	qtrs = unlist(lapply(strsplit(qtrs, '//'), function(x){x[2]}))
#	dynPath = sprintf("%s%s/", dynPath, qtrs[1])
#	#
#	years = list.dirs(dynPath, recursive=F);
#	years = unlist(lapply(strsplit(years, '//'), function(x){x[2]}))
#	#
#	runByYearGear = foreach( g=gears )%dopar%{
#        #for(p in ports){
#                #
#		runByYearPost = list()
#		runByYearPred = list()
#		for(y in years){
#			runByYearPost[[y]] = 0
#                        runByYearPred[[y]] = 0
#		}
#              	#
#                for(p in ports){
#                for(q in qtrs ){
#                for(y in years){
#			#
#			land = mpgyqLand[mpgyqLand$mcat==mcat & mpgyqLand$port==p & mpgyqLand$gear==g & mpgyqLand$year==y & mpgyqLand$qtr==q,]
#			#	
#                        lPost = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y)) 
#			if( dim(land)[1]>0 & spp%in%colnames(lPost) ){
#				#
#				land = as.numeric(land['land'])
#				#
#         	       		lPost = lPost[,spp]
#	                	lPred = read.csv(sprintf('%s/%s/%s/%s/%s/sppComp.csv', avgPath, p, g, q, y))[,spp] 
#				#
#				lPostExp = lPost[!is.na(lPost)]*land
#				lPostExp = head(rep(lPostExp, ceiling(M/length(lPostExp))), M)
#				lPredExp = lPred[!is.na(lPred)]*land
#				lPredExp = head(rep(lPredExp, ceiling(M/length(lPredExp))), M)
#				#
#				runByYearPost[[y]] = runByYearPost[[y]] + lPostExp
#				runByYearPred[[y]] = runByYearPred[[y]] + lPredExp
#			}
#		}}}
#		#
#		return( list(post=runByYearPost, pred=runByYearPred) )
#	}
#	names(runByYearGear) = gears
#	#
#	for(g in gears){
#		for(y in years){
#			runByGearYearPost[[g]][[y]] = runByGearYearPost[[g]][[y]] + runByYearGear[[g]][['post']][[y]]
#			runByGearYearPred[[g]][[y]] = runByGearYearPred[[g]][[y]] + runByYearGear[[g]][['pred']][[y]]
#		}
#	}
#}
#
##
#save.image(sprintf('%sto%s%s.RData', minYear, maxYear, spp))

#
#JOIN
#

#
spp = 'BDRK' #'SNOS' #'GSRK' #'BLGL' #'CWCD' #'YTRK' #'CNRY' #'CLPR' #'BCAC' #'WDOW' 
#
e = new.env()
load(sprintf('1978to1982%s.RData', spp), envir=e)
gyLand = e$gyLand
runByGearYearPost = e$runByGearYearPost
runByGearYearPred = e$runByGearYearPred
#
load(sprintf('1983to1990%s.RData', spp), envir=e) 
gyLand = rbind(gyLand, e$gyLand)
for(g in e$gears){
        for(y in e$years){
                runByGearYearPost[[g]][[y]] = e$runByGearYearPost[[g]][[y]]
                runByGearYearPred[[g]][[y]] = e$runByGearYearPred[[g]][[y]]
        }
}
#
prob = 0.95
bit = (1-prob)/2
#gear = 'OTH' #'NET' #'OTH' #'HKL' #'TWL'
#
gears = c('TWL', 'NET', 'HKL', 'OTH')
for(gear in gears){
	plotLines = matrix(NA, nrow=7, ncol=length(names(runByGearYearPost[[g]])))
	colnames(plotLines) = names(runByGearYearPost[[g]])
	rownames(plotLines) = c('predU', 'postU', 'mean', 'postL', 'predL', 'predSD', 'postSD')
	for(y in names(runByGearYearPost[[g]])){
		#
		predMean = mean(runByGearYearPred[[gear]][[y]])
		postMean = mean(runByGearYearPost[[gear]][[y]])
		mDiff = predMean-postMean
		#
		plotLines['predU', y] = quantile(runByGearYearPred[[gear]][[y]], prob+bit)
		plotLines['postU', y] = quantile(runByGearYearPost[[gear]][[y]], prob+bit)+mDiff
		plotLines['mean', y] = predMean #mean(runByGearYearPred[[gear]][[y]])
		plotLines['postL', y] = max(quantile(runByGearYearPost[[gear]][[y]], bit)+mDiff, 0)
		plotLines['predL', y] = quantile(runByGearYearPred[[gear]][[y]], bit)
		plotLines['predSD', y] = sd(runByGearYearPred[[gear]][[y]])
		plotLines['postSD', y] = sd(runByGearYearPost[[gear]][[y]])
	}
	
	#
	#PLOT
	#
	
	#
	comPlot = cbind( gyLand[gyLand$gear==gear,c('year')], gyLand[gyLand$gear==gear,'land'] )
	years = as.numeric(colnames(plotLines))
	#
	pdf(sprintf('%s%sLandPic.pdf', spp, gear))
	plot(years, plotLines['mean',], 'l', 
		lwd=3, 
		col='black',
		ylim=c(0, max(c(max(plotLines), gyLand[gyLand$gear==gear,'land']))),
		main=sprintf('%s: %s', spp, gear),
		ylab='Expanded Landings',
		xlab='Year'
	)
	polygon(c(years, rev(years)), c(plotLines['predU',], rev(plotLines['predL',])), col='grey50')
	#polygon(c(years, rev(years)), c(plotLines['postU',], rev(plotLines['postL',])), col='grey30')
	lines(years, plotLines['mean',], lwd=3, col='black')
	lines(comPlot, lwd=2)
	points(comPlot, pch=19)
	dev.off()
}












