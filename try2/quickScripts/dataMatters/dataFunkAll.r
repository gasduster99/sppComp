#
#DEPENDENCIES
#

#
suppressMessages(library(RJDBC, quietly=FALSE)) #requires "./sqljdbc4.jar"
suppressMessages(library(getPass, quietly=FALSE))

#
#FUNCTIONS
#

#
getRawDataAll = function(minYear, maxYear, save=F){
	#minYear: start of modeled time period 
	#maxYear: end of modeled time period 
	#
	#value	: a raw data.frame called from the data source 
	#

	#
	#SQL
	#
	
	#
	writeLines('\n****** Query *******')
	#driver
	drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
	#connection
	ch = dbConnect(drv, 'jdbc:sqlserver://161.55.237.17;databaseName=grunloh_db', 'nick.grunloh', 'Nmfsswfsc!2018') #getPass('User:'), getPass('Password:'))#
	#port sample data query
	raw = dbGetQuery(ch,
	        sprintf("
	        select
	                master_clusts.sample_no		as sampleNumber,
	                clust_no			as clusterNumber,
	                rtrim(ltrim(species))		as species,
	                weight				as weight,
	                DATEPART(yyyy, sample_date)	as year,
	                DATEPART(QUARTER, sample_date)	as qtr,
	                port_complex			as portComplex,
	                gear_grp			as gearGroup,
	                mark_cat			as marketCategory,
	                live_fish			as live
	        
	        from master_samples inner join master_clusts
	                ON master_samples.sample_no=master_clusts.sample_no
	       
		where 
			DATEPART(yyyy, sample_date) >= %d 	and 
			DATEPART(yyyy, sample_date) <= %d 	and 
			check_me='0'				and
			live_fish='N'				and
			gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')
	        ", minYear, maxYear)
	)
	#landings data
	lands = dbGetQuery(ch,
	        sprintf("
	        select
	                mark_cat 	as marketCategory, 
	                year		as year, 
	                quarter 	as qtr,
	                gear_grp 	as gearGroup, 
	                port_complex	as portComplex,
			species		as species,
			live		as live,
	                sum(pounds)	as comLands
		
	        FROM [COMX_DB].[dbo].[COM_LANDS]	
		
	        where 
                        year >= %d       and 
                        year <= %d       and 
                        live='N'	 and
                        gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')
		
		group by 
			mark_cat,
			year,
			quarter,
			gear_grp,
			port_complex,
			live,
			species
			
	        ", minYear, maxYear)
	)
	
	#merge
	raw = merge(raw, lands, by=c('species', 'year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live'), all.x=T)
	raw$comLands[is.na(raw$comLands)] = 0	

	#
	#SAVE
	#

	#	
	if( save ){	
		#save a local version of data for future reference
		write.csv(raw, sprintf('data%sTo%s_%s.csv', substring(minYear, 3, 4), substring(maxYear, 3, 4), Sys.Date()), 
			row.names=F,
			quote=F
		)
		#raw = read.csv('./data83to90.csv', header=T)		
	}	

	#
	writeLines('**** Complete ******\n')
	return( raw )
}

#
getRawData = function(mcat, minYear, maxYear, save=F){
	#mcat	: mcat number
	#minYear: start of modeled time period 
	#maxYear: end of modeled time period 
	#
	#value	: a raw data.frame called from the data source 
	#

	#
	#SQL
	#
	
	#
	writeLines('\n****** Query *******')
	#driver
	drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
	#connection
	ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=COMX_DB', 'nick.grunloh', 'Nmfsswfsc!2018') #getPass('User:'), getPass('Password:'))#
	#port sample data query
	raw = dbGetQuery(ch,
	        sprintf("
	        select
	                master_clusts.sample_no		as sampleNumber,
	                clust_no			as clusterNumber,
	                rtrim(ltrim(species))		as species,
	                weight				as weight,
	                DATEPART(yyyy, sample_date)	as year,
	                DATEPART(QUARTER, sample_date)	as qtr,
	                port_complex			as portComplex,
	                gear_grp			as gearGroup,
	                mark_cat			as marketCategory,
	                live_fish			as live
	        
	        from master_samples inner join master_clusts
	                ON master_samples.sample_no=master_clusts.sample_no
	       
		where 
			DATEPART(yyyy, sample_date) >= %d 	and 
			DATEPART(yyyy, sample_date) <= %d 	and 
			mark_cat=%d 				and
			check_me='0'				and
			live_fish='N'				and
			gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')
	        ", minYear, maxYear, mcat)
	)
	#landings data
	lands = dbGetQuery(ch,
	        sprintf("
	        select
	                mark_cat 	as marketCategory, 
	                year		as year, 
	                quarter 	as qtr,
	                gear_grp 	as gearGroup, 
	                port_complex	as portComplex,
			species		as species,
			live		as live,
	                sum(pounds)	as comLands
		
	        FROM [COMX_DB].[dbo].[COM_LANDS]	
		
	        where 
                        year >= %d       and 
                        year <= %d       and 
                        mark_cat=%d      and
                        live='N'	 and
                        gear_grp in ('HKL', 'TWL', 'FPT', 'NET', 'MDT')
		
		group by 
			mark_cat,
			year,
			quarter,
			gear_grp,
			port_complex,
			live,
			species
			
	        ", minYear, maxYear, mcat)
	)
	
	#merge
	raw = merge(raw, lands, by=c('species', 'year', 'qtr', 'portComplex', 'gearGroup', 'marketCategory', 'live'), all.x=T)
	raw$comLands[is.na(raw$comLands)] = 0	

	#
	#SAVE
	#

	#	
	if( save ){	
		#save a local version of data for future reference
		write.csv(raw, sprintf('%sdata%sTo%s_%s.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4), Sys.Date()), 
			row.names=F,
			quote=F
		)
		#raw = read.csv('./data83to90.csv', header=T)		
	}	

	#
	writeLines('**** Complete ******\n')
	return( raw )
}

makeD = function(sppGold, raw){
	#raw	: raw samples without and implied structure
	#
	#value	: a list of augmented samples aggregated across clusters with multinomial implied structure filling in implied zeros for unobserved species
	writeLines('makeD...\n')

	#
	#PREP D
	#
	
	#print( aggregate(raw$comLands, by=list(raw$port), sum) )
	#by = list(
	#	id	= raw$sampleNumber,
	#	species	= raw$species,
	#	year	= raw$year, 
	#	qtr	= raw$qtr, 
	#	port	= raw$portComplex, 
	#	gear	= raw$gearGroup, 
	#	live	= raw$live, 
	#	landing	= raw$comLands
	#)
	#THIS AGGREGATEION DROP LBS
	#D = aggregate(raw$weight, by=by, FUN=sum)		
	#print(tail(D))
	#print( aggregate(D$comLands, by=list(D$port), sum) )
	
	#data.frame(raw$year, raw$qtr, raw$portComplex, raw$gearGroup, raw$live, raw$comLands), by=list(raw$sampleNumber, raw$species), FUN=unique )
	#D2 = aggregate( raw$weight, by=list(raw$sampleNumber, raw$species), FUN=sum)

	##aggregate the categorical variables so we have one observation per species per sample (aggregate cluster samples)
	#D = aggregate( data.frame(raw$year, raw$qtr, raw$portComplex, raw$gearGroup, raw$live, raw$comLands), by=list(raw$sampleNumber, raw$species), FUN=unique )	
	##aggregate add weights such that there is one weight per species per sample (aggregate cluster samples)
	#D = cbind(D, aggregate( raw$weight, by=list(raw$sampleNumber, raw$species), FUN=sum)[,3] )	
	#print( aggregate(D$raw.comLands, by=list(D$raw.portComplex), sum) )
	
	#sum weight across cluster
	D = aggregate(raw$weight, raw[-which(colnames(raw)%in%c('clusterNumber', 'weight'))], sum)
	colnames(D) = c( 
                        'species',
                        'year',
                        'qtr',
                        'port',
                        'gear',
			'mcat',
                        'live',
			'id',
                        'landing',
                        'weight'
        )
	#sum of weights of cluster in each sample
	clustWeight = aggregate( raw$weight, by=list(id=raw$sampleNumber), FUN=sum )
	#match up total sampled weight with species weights by ids
	D = merge(D, clustWeight, by='id')
	colnames(D)[colnames(D)=='x'] = 'nBB'
	#
	D$live = as.character(D$live)
	D$port = as.character(D$port)
	D$gear = as.character(D$gear)

	#TWL=TWL+MDT; 'HKL'; 'FPT'; 'NET'
	D$gear[D$gear=='MDT'] = 'TWL'	
	
	#
	#ADD ZEROS
	#

	#its easier to fill D holes as a list
	D = as.list(D)
	#an index to grow D to fill holes	
	end = length(D$id)
	#fill implied zeroes from unobserved species in the samples
	for(id in unique(D$id)){
	        #
	        wid = which(D$id==id)
		#
	        nBB	= D$nBB[wid[1]]
	        port 	= D$port[wid[1]]
	        gear 	= D$gear[wid[1]]
	        year 	= D$year[wid[1]]
	        qtr  	= D$qtr[wid[1]]
		mcat	= D$mcat[wid[1]]
		live 	= D$live[wid[1]]
		#landing	= raw[Draw$port=='OSF' & Draw$gear=='TWL' & Draw$year==1980 & Draw$qtr==1 & raw$species=='CLPR','comLands'])
		#each sample should have at least a zero for each species
	        for(sn in sppGold[!sppGold%in%D$species[wid]]){
			#
	                end = end + 1
	                #
	                D$id[end]   	= id
	                D$nBB[end] 	= nBB
	                D$port[end] 	= port
	                D$gear[end] 	= gear
	                D$year[end] 	= year
	                D$qtr[end]  	= qtr
	                D$species[end]  = sn
			D$mcat[end]	= mcat
			D$live[end]	= live
			D$landing[end]  = max(0, raw[raw$port==port & raw$gear==gear & raw$year==year & raw$qtr==qtr & raw$species==sn,'comLands'])
			#
	                D$weight[end] 	= 0
	        }
	}
	#
	D = as.data.frame(D)
	D = D[, c('id', 'mcat', 'live', 'year', 'qtr', 'port', 'gear', 'species', 'nBB', 'landing', 'weight')]
	#
	return( D )
}

#
addPredStrat = function(sppGold, portGold, gearGold, yearGold, qtrGold, D){
	#sppGold	: a list of gold standard species 
	#portGold	: a list of gold standard ports
	#gearGold	: a list of gold standard gears
	#yearGold	: a list of gold standard years
	#qtrGold	: a list of gold standard qtrs
	#D		: a list to augment
	#
	#value		: an augmented list with unsampled strata added for use by the model to make predictions
	writeLines('addPredStrat...\n')

	#D is easyier to modify as a list
	D = as.list(D)
	D$port = as.character(D$port)
	D$gear = as.character(D$gear)
	D$species = as.character(D$species)
	#an index to grow D to fill holes       
        end = length(D$id)
	#prediction sum cluster size 
	fill = 100
	#fill unsampled strata that are internal to the range of the data for prediction
	for(p in portGold){
	for(g in gearGold){
	for(q in qtrGold ){
	for(y in yearGold){
	for(s in sppGold ){
	        #
	        wJoint = which(
	                D$port==p       &
	                D$gear==g       &
	                D$year==y       &
	                D$qtr==q        &
	                D$species==s
	        )
	        #
	        if( length(wJoint)==0 ){
	                #data grows by a single row with 0 weight
	                end = end + 1
	                #
	                D$id[end]     = NA
	                D$weight[end] = NA
	                D$nBB[end]    = fill
	                D$port[end]   = p
	                D$gear[end]   = g
	                D$year[end]   = y
	                D$qtr[end]    = q
	                D$species[end]= s
                        D$live[end]   = 'N'
			D$mcat[end]   = D$mcat[1]
			D$landing[end]= NA
	        }
	
	}}}}
	}
	#
	D = as.data.frame(D)
	D$port = as.character(D$port)
        D$gear = as.character(D$gear)
        D$species = as.character(D$species)
	return( D )
}


