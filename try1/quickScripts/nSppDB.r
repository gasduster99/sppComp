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
minYear = 2000 # 1978 # 1983 # 1991 # 
maxYear = 2015 # 1982 # 1990 # 1999 # 
#mcat = 250

#driver
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
#connection
ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=calcom', 'nick.grunloh', 'Nmfsswfsc2017')
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
names(mcatQProbs) = landsTotals$mcat[mcatOrd]

#
palette(brewer.pal(n = 8, name = "Set3"))#
#
pdf(sprintf('%sto%sBar.pdf', minYear, maxYear))
par(mar = c(5,7,2,5))
barplot( tb[sppOrd,mcatOrd][,mcatQProbs<prob],
	ylim=c(0, 350000),
	border=NA,
	las=2,
	col=1:length(sppOrd), 
	main=sprintf('%s-%s', minYear, maxYear), 
	ylab='Sampled Weight\n\n' 
)
par(new=T)
plot((1:sum(mcatQProbs<prob)), mcatProbs[mcatQProbs<prob], type='l', lty=1, axes=F, ylim=c(-0.6, 0.5), ylab='', xlim=c(1-0.5, sum(mcatQProbs<prob)+0.5), xlab='Market Category')
points((1:sum(mcatQProbs<prob)), mcatProbs[mcatQProbs<prob], cex=0.7, pch=19)
axis(4, at=seq(0,0.55,0.1))
mtext(side=4, line=3, '                                                              Proportion of Landed Weight')
dev.off()

