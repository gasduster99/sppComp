rm(list=ls())

#
library(maps)
library(foreach)
library(doParallel)
library(HDInterval)
library(KernSmooth)
library(RColorBrewer)
#
source("predictFunc.r")

#
#FUNCTIONS
#

#
makeMap = function(cols){
        #
        texts = c(expression(bold('CRS')), expression(bold('ERK')), expression(bold('BRG')), expression(bold('BDG')), expression(bold('OSF')), expression(bold('MNT')), expression(bold('MRO')), expression(bold('OSB')), expression(bold('OLA')), expression(bold
('OSD'))) 
        #
        top = 0.98
        bot = 0.12
        #
        map(database="state", regions="california",
               projection="gilbert", 
               orientation=c(120, 0, 240),
               fill=T,
               col='grey'
        )
        #
        mtext(texts,
               side=2,
               adj=seq(top, bot, -(top-bot)/(length(texts)-1)),
               cex=2.5,
               col=cols,
               line=0
        )
}

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
#MAIN
#

#
path = "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/"
#78-82
# [1] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT195/"
# [2] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT250/"
# [3] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT253/"
# [4] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT262/"
# [5] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT265/"
# [6] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT269/"
# [7] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT270/"
# [8] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT956/"
# [9] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT959/"
#[10] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT961/"
#83-90
# [1] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT245/"
# [2] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/"
# [3] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT253/"
# [4] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT259/"
# [5] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT262/"
# [6] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT269/"
# [7] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT270/"
# [8] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT663/"
# [9] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT667/"
#[10] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT956/"
#[11] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT959/"        
#[12] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT960/"        
#[13] "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT961/"
gos = c(
	#78-82
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT250/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT253/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT269/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT956/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT265/",
	#83-90
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT956/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT269/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT959/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT961/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT960/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT667/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT253/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT259/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT663/"
)
#
perf = list()
timePeriods = Sys.glob(sprintf('%s*to*/', path))
aCount = 4
adjusts = c(
	#78-82:*250,    *253,   265,  *269,  *956
	       4.1/12,3.4/12,1.65/16,5.48/12,3.1/12,
	#83-90:_*250,   253,   259,   269,   663,  _667,  *956, _*959,  *960, _*961
	      4.4/12,1.85/12,2.0/12,9.7/12,1.5/12,6.2/12,4.4/12,2.8/12,2.3/12,2.35/12
)
for( tp in timePeriods[1] ){
	#
	yearsStr = strsplit(tp, '/')[[1]][6]
	##these data do not include the implied zeros
	#dat = read.csv(sprintf('../data%s.csv', yearsStr), stringsAsFactors=F)	
	#
	mcats = Sys.glob(sprintf('%sMCAT*/', tp))
	for( mcat in mcats[6] ){ #foreach(){
		##skippers
		#if( mcat=="/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/" ){ next }	
		if( !(mcat%in%gos) ){ next }
		#
		load(sprintf('%s/Top/Space274/Space274.RData', mcat))
		#print(mcat)
		#print( sprintf('n:%d, spp:%d', dim(dataClean)[1], length(sppGold)) )
		##	
		#mets = Sys.glob(sprintf('%sTop/Space*/metrics.csv', mcat))
		#metDF = matrix(NA, nrow=length(mets), ncol=4)
		##
		#i = 1;
		#rNames = matrix(NA, nrow=length(mets), ncol=1)
		#for( met in mets ){
		#	#
		#	metDF[i,c(1,2,3)] = unlist(read.csv(met)[1:3])
		#	rNames[i] = strsplit(met, '/')[[1]][9]
		#	#
		#	i = i+1
		#}
		#rownames(metDF) = rNames
		#colnames(metDF) = c('mlik', 'waic', 'dic', 'weight')
		##
		#metDF[,4] = goodRatios(metDF[,'mlik'])
		#metDF = metDF[order(metDF[,'mlik']),] #, decreasing=T),]
		##
		#tailSize = 10
		#cols = c(brewer.pal(9,"Set1"), 'black')
		###MCAT195 weird
		##if( mcat=="/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT195/" ){ 
		##	print(metDF) 
		##}
		#
		##
		##BARPLOTS
		##

		##
		#nameSplit = strsplit(mcat, '/')[[1]]
		#barName = sprintf('%s%s.pdf', nameSplit[6], nameSplit[7])
		#plotThing = tail(metDF[!is.na(metDF[,'mlik']),'weight'], tailSize)
		##
		#pdf(barName, width=16, height=5)
		#barplot(plotThing,
		#	cex.axis=1.7, 
		#	axisnames=FALSE
		#)
		#dev.off()
		#
		##
		##MAPS
		##
		#
		#i = 1
		#spaceNames = names(plotThing)
		#for( sn in spaceNames){ 
		#	#	
		#	load( sprintf('%s/Top/%s/%s.RData', mcat, sn, sn) )	
		#	col = rep(NA, length(portGold))
		#	for( j in 1:length(portGold) ){ col[j]=cols[grep(portGold[j], portEff)] }
		#	#
		#	mapName = sprintf('%s%sMap%d.pdf', nameSplit[6], nameSplit[7], i)
		#	pdf(mapName, width=7, height=15)
		#	makeMap(col)
		#	dev.off()
		#	#
		#	i = i+1
		#}
		
		#
		#SPP COMPS
		#
		
		#
		print(mcat)
		datMcatFill = cleanZero(mcat, 8)
		#print(head(datMcatFill, 3))	
		#NOTE: MCAT190 is SABL only; and does not return any preds!!!!!	
		avgPath = sprintf('%sTop/avgModel/', mcat)
		preds = predPerf(datMcatFill, 0.99, avgPath, 8, adjusts[aCount])	
		perf[[mcat]] = preds
		#
		#print(nameSplit[7])
		#print(preds$ciAvgAcc)
		print(preds$hdiAvgAcc)
		writeLines('')
		#
		aCount = aCount+1
	}
}




##
#sp = read.csv(sprintf('%s/%s/sppComp.csv', path, nome))#'MCAT959/Top/avgModel/%s/sppComp.csv', nome))
#ms = sort(colMeans(sp), decreasing=T)
#sName = names(ms)
#M = length(ms)
##
#qs = matrix(NA, nrow=M, ncol=2)
#rownames(qs) = sName
#for(n in sName){
#        qs[n,] = quantile(sp[,n], c(0.025, 0.975))
#}
##
#howMany = 18#20
#c = min(M, howMany)
##
#pdf('bocBoxMax.pdf', width=19, height=5.5)
##dev.new(width=15, height=5)
#opar = par(ps=18)
#plot(head(ms, c),
#        cex=1.7,
#        pch=19,
#        xaxt="n",
#        xlab='',
#        ylim=c(0, 1),
#        ylab='Proportion',
#        main='' #'Species Composition'
#)
#axis(1, at=1:c, ctions, A, of thelabels=head(names(ms), c))
#for(i in 1:c){ segments(i, qs[i,1], i, qs[i,2], lwd=5) }
##
#opar
#dev.off()





#ls = list.files(sprintf('%s/*to*/MCAT*/Top/metrics.csv' /78to82/MCAT*/Top/Space*/metrics.csv, path), recursive=T) 

#83to90 MCAT250: Failed/Requires re-run:  186 runs finished
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space116/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space118/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space119/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space120/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space121/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space122/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space124/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space130/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space133/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space135/
#/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/Top/Space13/



#mcats = Sys.glob(sprintf('%s/MCAT*', path))
##
#ports = c('BDG', 'BRG', 'CRS', 'ERK', 'OSF')
##ports = c('MNT', 'MRO', 'OSB', 'OLA', 'OSD')
#gears = c('HKL', 'NET', 'OTH', 'TWL')
#qtrs  = 1:4
#years = 1983:1990
##
#P = length(ports)
#G = length(gears)
#Q = length(qtrs)
#Y = length(years)
##
##matrix(NA, ncol=1, nrow=P*G*Q*Y)
##gr = expand.grid(years, qtrs, gears, ports)
##rownames(l) = paste(gr$Var4, gr$Var3, gr$Var2, gr$Var1, sep="/")
##
#registerDoParallel(cores=4)
#out = foreach( mcat=mcats[c(-1, -8, -9)] )%dopar%{ #for(mcat in mcats[c(-1, -8, -9)]){
#	print( sprintf('%s', mcat) )
#	l = c()
#	for(p in ports){
#	for(g in gears){
#	for(q in qtrs ){
#	for(y in years){
#		#
#		nome = sprintf('%s/Top/avgModel/%s/%s/%s/%s', mcat, p, g, q, y)	
#		#nome = sprintf('%s/Bottom/avgModel/%s/%s/%s/%s', mcat, p, g, q, y)
#		sp = read.csv(sprintf('%s/sppComp.csv', nome))
#		ms = sort(colMeans(sp), decreasing=T) #colMeans(sp) 
#		#& which(names(ms)=='CLPR')%in%1:5 #CNRY
#		if( which(names(ms)=='BCAC')%in%1:5 ){ #& which(names(ms)=='CNRY')%in%1:5 ){
#			l[nome] = max(ms)#['BCAC'] #which(names(ms)=='BCAC')
#		}
#	}}}}
#	#
#	return(l)
#}
#out = rev(sort(unlist(out)))
#print( head(names(out), 20) )
