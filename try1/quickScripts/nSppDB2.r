rm(list=ls())

library(RJDBC)
library(RColorBrewer)

#
#FUNCTIONS
#


#
#MAIN
#


#
minYear = 1978 # 1983 # 1991 # 2000 #
maxYear = 1982 # 1990 # 1999 # 2015 #
#mcat = 250

#driver
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
#connection
ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=calcom', 'nick.grunloh', 'Nmfsswfsc!2018')
#data query
data = dbGetQuery(ch,
        sprintf("
        select
                master_clusts.sample_no,
                clust_no, 
                rtrim(ltrim(species)),
                weight,
                DATEPART(yyyy, sample_date),
                DATEPART(QUARTER, sample_date),
                cal_port,
                port_complex,
                gear_grp,
                mark_cat,
                total_wgt,
                live_fish
        
        from master_samples inner join master_clusts
                ON master_samples.sample_no=master_clusts.sample_no
        
        where DATEPART(yyyy, sample_date) >= %d and DATEPART(yyyy, sample_date) <= %d
        ", minYear, maxYear)#, mcat) # and mark_cat=%d 
)
colnames(data) = c(
       'sampleNumber',
       'clusterNumber',
       'species',
       'weight',
       'year',
       'qtr',
       'calPort',
       'portComplex',
       'gearGroup',
       'mcat',
       'totalWeight',
       'live'
)
#
rckKey = dbGetQuery(ch,
        "
	select mark_cat, nominal_species

	FROM [calcom].[dbo].[MARKET_CATEGORIES]
	
	where species_grp='ROCKFISH'
	"
)
#
lands = dbGetQuery(ch,
        sprintf("
        select pounds, mark_cat

        FROM [calcom].[dbo].[COM_LANDS]

        where year >= %d and year <= %d
        ", minYear, maxYear) 
)
#
data = data[data$mcat%in%rckKey$mark_cat,]
lands = lands[lands$mark_cat%in%rckKey$mark_cat,]

#
landsTotals = aggregate(lands$pounds, by=list(lands$mark_cat), FUN=sum)
colnames(landsTotals) = c('mcat', 'weight')
#
samCounts = aggregate(data$sampleNumber, by=list(data$mcat), FUN=length)
colnames(samCounts) = c('mcat', 'n')
#
mcatSppTotals = aggregate(data$weight, by=list(data$mcat, data$species), FUN=mean)
colnames(mcatSppTotals) = c('mcat', 'species', 'weight')
#yearSppCount = aggregate(yearSppTotals$species, by=list(yearSppTotals$year), FUN=function(x){ length(unique(x)) })
#
tb = table(data$species, data$mcat)
for(r in 1:dim(tb)[1]){
	for(c in 1:dim(tb)[2]){
		species = rownames(tb)[r]
		mcat = colnames(tb)[c]
		avg = mcatSppTotals$weight[mcatSppTotals$mcat==mcat & mcatSppTotals$species==species]
		if(length(avg)>0){ tb[r, c]=tb[r,c]*mcatSppTotals$weight[mcatSppTotals$mcat==mcat & mcatSppTotals$species==species] }
	}
}
#
#mcatLBS = colSums(tb)
landsTotals = landsTotals[landsTotals$mcat%in%colnames(tb),]
sppLBS = rowSums(tb)
mcatOrd = order(landsTotals$weight, decreasing=T) #order(mcatLBS, decreasing=T)
sppOrd = order(sppLBS, decreasing=T)
#
prob = 0.99
mcatQProbs = cumsum(landsTotals$weight[mcatOrd])/sum(landsTotals$weight)#cumsum(mcatLBS[mcatOrd])/sum(mcatLBS)
mcatProbs = landsTotals$weight[mcatOrd]/sum(landsTotals$weight)
samProbs = samCounts$n[mcatOrd]/sum(samCounts$n)
names(mcatQProbs) = landsTotals$mcat[mcatOrd]
#
pThing = t(t(tb[sppOrd,mcatOrd][,mcatQProbs<prob])/colSums(tb[sppOrd,mcatOrd][,mcatQProbs<prob]))
#
special = c("BANK", "BCAC", "ARRA", "BLGL", "BLCK", "CLPR", "YTRK", "CNRY", "BLUR", "BRWN", "VRML", "WDOW")#, "BRNZ", "CWCD", "RDBD")
cols = brewer.pal(n = 12, name = "Paired")
bw = c('grey65', 'grey75') #rep('grey60', 2) #
#
pThing = rbind(pThing[rownames(pThing)%in%special,], pThing[!rownames(pThing)%in%special,])
#
j=0
pCol = rep(NA, dim(pThing)[1])
for(i in 1:dim(pThing)[1]){
	spp = rownames(pThing)[i]
	where = which(special==spp)
	if( length(where)==0 ){ 
		pCol[i] = bw[(j%%2)+1]
		j = j+1
	} else{
		pCol[i] = cols[where]
	}
}
##
#pdf(sprintf('%sto%sBar2.pdf', minYear, maxYear))
#par(mar = c(3,5,2,2))
#par(cex.axis=1.5, cex.lab=1.5, cex.main=1.5)
#layout(matrix(c(2,2,1,1,1), nrow=5, ncol=1))
#barplot( pThing,
#	ylim=c(0, 1), #ylim=c(0, 350000),
#	border=NA,
#	las=2,
#	col=pCol, 
#	#main=sprintf('%s-%s', minYear, maxYear), 
#	ylab='Proportion of Sampled Weight Within MCAT'
#)
#mtext(colSums(pThing>0), side=3, line=0.8, at=head(seq(0, 100, 1.2)+0.7, length(colSums(pThing>0))))
##par(new=T)
#par(mar = c(0,5,2,2))
#plot((1:sum(mcatQProbs<prob))+0.5, mcatProbs[mcatQProbs<prob], 
#	type='l', 
#	lty=1, 
#	axes=F, 
#	ylim=c(0, 1), 
#	xlim=c(1, sum(mcatQProbs<prob)+1),
#	ylab='Proportion',
#	xlab='',
#	main=sprintf('%s-%s', minYear, maxYear)
#	#xlim=c(1-0.5, sum(mcatQProbs<prob)+0.5), 
#	#xlab='Market Category'
#)
#points((1:sum(mcatQProbs<prob))+0.5, mcatProbs[mcatQProbs<prob], cex=0.7, pch=19)
#lines((1:sum(mcatQProbs<prob))+0.5, samProbs[mcatQProbs<prob], col='blue')
#points((1:sum(mcatQProbs<prob))+0.5, samProbs[mcatQProbs<prob], cex=0.7, pch=19, col='blue')
#axis(2, at=seq(0, 1, 0.2), las=1)
#legend('right', legend=c("Landed Weight", "# of Samples"), col=c('black', 'blue'), lty=c(1, 1), pch=c(19, 19), cex=1.5, bty='n')
#dev.off()

