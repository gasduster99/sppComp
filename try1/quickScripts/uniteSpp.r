rm(list=ls())

library(RJDBC)
library(RColorBrewer)

#
#DATA
#

#
tp = 4
minYear = c(1978, 1983, 1991, 2000) 
maxYear = c(1982, 1990, 1999, 2015) 
#mcat = 250

#driver
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
#connection
ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=calcom', 'nick.grunloh', 'Nmfsswfsc2017')
#define rockfish
rckKey = dbGetQuery(ch,
        "
        select mark_cat, nominal_species

        FROM [calcom].[dbo].[MARKET_CATEGORIES]
        
        where species_grp='ROCKFISH'
        "
)

#
nn = 12
reward = seq(15, 1, -1)
##
#data = list()
#lands = list()
imp = list()
for(t in 1:tp){
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
	        ", minYear[t], maxYear[t])#, mcat) # and mark_cat=%d 
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
	lands = dbGetQuery(ch,
	        sprintf("
	        select pounds, mark_cat
	
	        FROM [calcom].[dbo].[COM_LANDS]
	
	        where year >= %d and year <= %d
	        ", minYear[t], maxYear[t])
	)
	data = data[data$mcat%in%rckKey$mark_cat,]
	lands = lands[lands$mark_cat%in%rckKey$mark_cat,]

	#
	landsTotals = aggregate(lands$pounds, by=list(lands$mark_cat), FUN=sum)
	colnames(landsTotals) = c('mcat', 'weight')
	#
	mcatSppTotals = aggregate(data$weight, by=list(data$mcat, data$species), FUN=mean)
	colnames(mcatSppTotals) = c('mcat', 'species', 'weight')
	#
	impSpp = mcatSppTotals
	for(i in 1:dim(landsTotals)[1]){
		for(j in 1:dim(mcatSppTotals[mcatSppTotals==landsTotals[i,1],])[1]){
			impSpp[impSpp$mcat==landsTotals[i, 1] & impSpp$species==mcatSppTotals[j, 2],3] = mcatSppTotals[j, 3]*landsTotals[i,2]
		}
	}
	imp[[t]] = aggregate(impSpp$weight, by=list(impSpp$species), FUN=sum)
	colnames(imp[[t]]) = c('species', 'weight')
	imp[[t]] = imp[[t]][order(imp[[t]][,'weight'], decreasing=T),'species']
	#tb = table(data$species, data$mcat)
	#for(r in 1:dim(tb)[1]){
	#        for(c in 1:dim(tb)[2]){
	#                species = rownames(tb)[r]
	#                mcat = colnames(tb)[c]
	#                avg = mcatSppTotals$weight[mcatSppTotals$mcat==mcat & mcatSppTotals$species==species]
	#                if(length(avg)>0){ tb[r, c]=tb[r,c]*mcatSppTotals$weight[mcatSppTotals$mcat==mcat & mcatSppTotals$species==species] }
	#        }
	#}

}

#
n = unique(do.call(c, imp))
board = as.integer(rep(0, length(n)))
names(board) = n
for(t in 1:tp){
	#
	j = 1
	for(s in head(imp[[t]], nn)){
		#
		board[s] = board[s] + reward[j]
		j = j+1
	}
}
board = sort(board, decreasing=T)

#
#PLOT LEGEND
#

#
n = 12
nomes = names(head(board, n))
cols = brewer.pal(n=n, name="Paired")
#
pdf('barplotLegend.pdf', width=3.5, height=7.04)
plot.new()
#par(mar = c(1,1,1,1))
legend('top', legend=nomes, fill=cols, cex=2)
dev.off()
