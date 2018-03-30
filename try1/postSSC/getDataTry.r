#rm(list=ls())

#
suppressMessages(library(RJDBC, quietly=FALSE)) #requires "./sqljdbc4.jar"
suppressMessages(library(getPass, quietly=FALSE))

#
#FUNCTIONS
#

#
#SQL
#

#driver
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
#connection
ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=Grunloh_db', getPass('User:'), getPass('Password:')) #, 'nick.grunloh', 'Nmfsswfsc2016')
#data query
raw = dbGetQuery(ch,
        "
        select
                master_com_clusts.sample_no,
                clust_no, 
                rtrim(ltrim(species)),
                weight,
                DATEPART(yyyy, sample_date),
                DATEPART(QUARTER, sample_date),
                --cal_port,
                port_complex,
                gear_grp,
                mark_cat,
                total_wgt,
                live_fish
        
        from master_com_samples inner join master_com_clusts
                ON master_com_samples.sample_no=master_com_clusts.sample_no
        "
)
colnames(raw) = c(
       'sampleNumber',
       'clusterNumber',
       'species',
       'weight',
       'year',
       'qtr',
       #'calPort',
       'portComplex',
       'gearGroup',
       'marketCategory',
       'landingWeight',
       'live'
)

##save a local version of data for future reference
#write.csv(raw, 'data83To90.csv')
##raw = read.csv('./data83to90.csv', header=T)

#aggregate the categorical variables so we have one observation per species per sample (aggregate cluster samples)
D = aggregate( data.frame(raw$year, raw$qtr, raw$portComplex, raw$gearGroup, raw$marketCategory, raw$live), by=list(raw$sampleNumber, raw$species), FUN=unique )
#aggregate add weights such that there is one weight per species per sample (aggregate cluster samples)
D = cbind(D, aggregate(data.frame(raw$weight), by=list(raw$sampleNumber, raw$species), FUN=sum)[,3])
#preallocate for cluster size column
D = cbind( D, rep(NA, dim(D)[1]) )
#sum of weights of cluster in each sample
clustWeight = aggregate( data.frame(raw$weight), by=list(raw$sampleNumber), FUN=sum )
#merge(D, clustWeight, 
###match up total sampled weight with species weights by ids 
##for(id in clustWeight[,1]){ 
##	D[D[,1]==id, dim(D)[2]] = clustWeight[clustWeight[,1]==id, 2] 
##}
##rename D columns
#colnames(D) = c(
#                'id',
#                'species',
#                'year',
#                'qtr',
#                'port',
#                'gear',
#                'mcat',
#                'live',
#                'weight',
#                'clustSize'
#)
#D$port = as.character(D$port)
#D$gear = as.character(D$gear)
#
##aggregate gear to OTH=UNK+FPT; TWL=TWL+MDT; HKL; NET
#D$gear[D$gear=='MDT'] = 'TWL'
#D$gear[D$gear=='FPT'] = 'OTH'
#D$gear[D$gear=='UNK'] = 'OTH'


#D goes to the model
#raw data is 
