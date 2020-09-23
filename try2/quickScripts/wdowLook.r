rm(list=ls())

#
library(boot)
library(mclust)
library(tmvtnorm)
library(latex2exp)
library(fitdistrplus)

#
#DATA
#

#NOTE: change years and port appropriatly (?spp?)
minYear = 1983 #1978 #
maxYear = 1990 #1982 #
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')
sppList = c('WDOW', 'BCAC', 'CLPR', 'BANK', 'YTRK', 'BLGL', 'DBRK', 'CNRY', 'SNOS', 'CWCD', 'POP', 'BRNZ', 'MXRF') #'CMEL')

#
globPath = "/home/nick/Documents/sppComp/inla/hotWired" #"/media/nick/extraBig/"
runPaths = Sys.glob(sprintf("%s%sto%s//*%s%s/", globPath, minYear, maxYear, minYear, maxYear))
mcats = as.numeric(sapply(runPaths, function(run){substring(strsplit(run, '//')[[1]][2], 1, 3)}))

#SAMPLE
sam = read.table(sprintf('./dataMatters/data%dTo%d.csv', minYear, maxYear), sep=',', header=T, stringsAsFactors=F)
ss = aggregate(sam$weight, by=list(species=sam$species, portComplex=sam$portComplex, year=sam$year, qtr=sam$qtr, gearGroup=sam$gearGroup, marketCategory=sam$marketCategory, live=sam$live), FUN=sum)
colnames(ss) = c('species', 'portComplex', 'year', 'qtr', 'gearGroup', 'marketCategory', 'live', 'weight')
tt = aggregate(ss$weight, by=list(portComplex=ss$portComplex, year=ss$year, qtr=ss$qtr, gearGroup=ss$gearGroup, marketCategory=ss$marketCategory, live=ss$live), FUN=sum)

#LANDINGS
land = read.table('./dataMatters/comLands.csv', sep=',', col.names=c('year', 'qtr', 'live', 'mcat', 'gear', 'port', 'species', 'weight', 'V1', 'V2'), stringsAsFactors=F)
where = land$year>=minYear & land$year<=maxYear  #land$live=='N' & #NOTE: remove live where 
land = land[where, c('live', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'weight')]
#
land = land[land$mcat%in%mcats,]
land = land[land$gear%in%gearGold,]
land = land[land$port%in%portGold,]
land = land[land$qtr%in%qtrGold,]
#convert landings to metric tons
land$weight = land$weight/2204.62
#
colnames(land) = c('live', 'marketCategory', 'year', 'qtr', 'gearGroup', 'portComplex', 'species', 'weight')
#remove comlands speciation
ll = aggregate(land$weight, by=list(portComplex=land$portComplex, year=land$year, qtr=land$qtr, gearGroup=land$gearGroup, marketCategory=land$marketCategory, live=land$live), FUN=sum)
colnames(ll) = c('portComplex', 'year', 'qtr', 'gearGroup', 'marketCategory', 'live', 'weight')

#merge in the landings and focus on WDOW
dd = merge(ss, tt, by=c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live'), all.x=T)
colnames(dd) = c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live', 'species', 'sampleWeight', 'sampleTotal')
dd = merge(dd, ll, by=c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live'), all.x=T)
colnames(dd) = c('year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live', 'species', 'sampleWeight', 'sampleTotal', 'lands')
dd = dd[!is.na(dd$lands),] 
dd[,'bins'] = round(log(dd$lands,10))
dd = dd[dd$species=='WDOW',]

#
#SPACE
#

#
spaceLand = aggregate(dd$lands, by=list(port=dd$port), FUN=sum)
spaceLand = spaceLand[match(portGold, spaceLand$port),]
#
spaceSamW = aggregate(dd$sampleWeight, by=list(port=dd$port), FUN=sum)
spaceSamW = spaceSamW[match(portGold, spaceSamW$port),]
#
spaceSamT = aggregate(dd$sampleTotal, by=list(port=dd$port), FUN=sum)
spaceSamT = spaceSamT[match(portGold, spaceSamT$port),]
#
spaceChart = cbind(spaceLand, spaceSamW$x/spaceSamT$x)
colnames(spaceChart) = c('Port', 'Landings (MT)', '% WDOW Sampled')

#
#SPACE/TIME
#

#
#NOTE: change years and port appropriatly (?spp?)
minYear = 1983
maxYear = 1990
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET')

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

##
#land = land[land$mcat%in%mcats,]
land = land[land$gear%in%gearGold,]
land = land[land$port%in%portGold,]
land = land[land$year%in%yearGold,]
land = land[land$qtr%in%qtrGold,]
#convert landings to metric tons
land$weight = land$weight/2204.62

#
tablue = aggregate(land$weight, by=list(port=land$port, year=land$year, mcat=land$mcat), FUN=sum)

#
#CORRELATION
#

#
corr = cor(dd$lands, dd$sampleWeight/dd$sampleTotal)
#
#reg0 = glm(dd$sampleWeight/dd$sampleTotal~dd$lands, family="binomial")#, weights=dd$sampleTotal)
reg1 = glm(dd$sampleWeight/dd$sampleTotal~dd$lands, family="binomial", weights=dd$sampleTotal)
reg2 = glm(cbind(dd$sampleWeight, dd$sampleTotal-dd$sampleWeight)~dd$lands, family="binomial")

#
isOne = dd$sampleWeight/dd$sampleTotal==1
jd = cbind(log(dd$lands), logit(dd$sampleWeight/dd$sampleTotal))[!isOne,] #-0.001
joint = mle.tmvnorm( jd, 
	start = list(
		mu = colMeans(jd),
		sigma = cov(jd)
	)
)
m1 = fitdist(jd[,1], dist='norm')
m2 = fitdist(jd[,2], dist='norm')

#
d = Mclust(jd, G=1, modelNames='VVV')

#
corTest = cor.test(jd[,1], jd[,2])

#
png(sprintf('%d%dJoint.png', minYear, maxYear))
plot(d, what = "uncertainty", xlab='log(Landings)', ylab='logit(Species Composition)') 
mtext(sprintf('%d-%d', minYear, maxYear), cex=1.5, font=2, line=1) #side = 3, line = -21, outer = TRUE)
mtext(TeX(sprintf('$\\hat{\\rho} = %0.3f$   $p-value=%0.2f$', corTest$estimate, corTest$p.value)), line=-3, at=min(jd[,1])+2, cex=1.2)
dev.off()





#
#JUNK YARD
#




#plot(jd, xlab='log(Landings)', ylab='logit(Species Composition)')i
##
#plot(log(dd$lands), logit(dd$sampleWeight/dd$sampleTotal))

##
#plot(dd$lands, dd$sampleWeight/dd$sampleTotal, xlim=c(-1000, 5000))
##funk0 = function(x){ inv.logit(cbind(rep(1, length(x)), x)%*%reg0$coefficients) }
#funk1 = function(x){ inv.logit(cbind(rep(1, length(x)), x)%*%reg1$coefficients) }
##curve(funk0, -1000, 5000, col='red', add=T)
#curve(funk1, -1000, 5000, add=T)




#
#topYear = aggregate(dat$weight, by=list(year=dat$year, bin=dat$bins, species=dat$species), FUN=sum)
#botYear = aggregate(topYear$x, by=list(year=topYear$year, bin=topYear$bin), FUN=sum) 
#topYear = topYear[topYear$species=='WDOW',]
##
#top = aggregate(dat$weight, by=list(bin=dat$bins, species=dat$species), FUN=sum)
#bot = aggregate(top$x, by=list(bin=top$bin), FUN=sum) 
#top = top[top$species=='WDOW',]
#ps = top$x/bot$x
#psChart = cbind(top$bin, bot$x, ps)
#colnames(spaceChart) = c('log(total landing bin)', 'total landing (mt)', '% sampled WDOW of total')




#tSpace = aggregate(dd$, by=list(port=topSpace$port), FUN=sum)
#tSpace = tSpace[match(portGold, tSpace$port),]
#space = aggregate(botSpace$x, by=list(port=botSpace$port), FUN=sum)
#space = space[match(portGold, space$port),]
#spaceChart = cbind(tSpace$port, tSpace$x, tSpace$x/space$x)
#colnames(spaceChart) = c('port', 'WDOW mt', '% of total')
#


#
#topSpace = aggregate(dd$weight, by=list(port=dd$portComplex, bin=dd$bins, species=dd$species), FUN=sum)
#botSpace = aggregate(topSpace$x, by=list(port=topSpace$port, bin=topSpace$bin), FUN=sum)
#topSpace = topSpace[topSpace$species=='WDOW',]



##add the required adjustment to account for extra landings in the merging process NOTE: add live loop if you do this beyond 1990
#for(m in mcats){
#for(y in as.character(yearGold)){ 
#for(g in gearGold){
#for(p in portGold){
#for(q in  qtrGold){
#	#
#	where = dat$gearGroup==g & dat$marketCategory==m & dat$portComplex==p & dat$year==y & dat$qtr==q              
#	#
#	if( sum(where)==0 ){ next }
#	#
#	print()
#	n = length(unique(dat[where, 'sampleNumber']))
#	adjLand = dat[where, 'comLands']/n
#	dat[where, 'comLands'] = adjLand
#	#
#	dat[where,'bins'] = round(log(sum(adjLand),10))
#}}}}}




##agg = aggregate(land$weight, by=list(year=land$year, species=land$species), FUN=sum)
#s = sam[
#	sam$species=='WDOW'     & 
#	sam$gearGroup=='TWL'    & 
#	sam$marketCategory==250 & 
#	sam$portComplex=='MNT'  & 
#	sam$year==1982		&
#	sam$qtr==4		&
#	sam$live=='N',
#]
#l = land[
#     	land$species=='WDOW'    &
#	land$gearGroup=='TWL'   & 
#	land$marketCategory==250&  
#	land$portComplex=='MNT' &
#	land$year==1982 	&
#	land$qtr==1		&
#	land$live=='N',
#]


