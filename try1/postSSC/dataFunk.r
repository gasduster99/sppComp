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
	ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=calcom', 'nick.grunloh', 'Nmfsswfsc!2018') #getPass('User:'), getPass('Password:'))#
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
	                total_wgt			as sampledLanding,
	                live_fish			as live
	        
	        from master_samples inner join master_clusts
	                ON master_samples.sample_no=master_clusts.sample_no
	       
		where 
			DATEPART(yyyy, sample_date) >= %d 	and 
			DATEPART(yyyy, sample_date) <= %d 	and 
			mark_cat=%d 				and
			check_me='0'				and
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
	                sum(pounds) 	as totalLanding
	
	        FROM [calcom].[dbo].[COM_LANDS]	

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
			port_complex
	        ", minYear, maxYear, mcat)
	)
	#merge
	raw = cbind(raw, totalLanding=rep(NA, dim(raw)[1]))
	for(m in unique(lands$marketCategory)){
	for(y in unique(lands$year)){
	for(q in unique(lands$qtr)){
	for(g in unique(lands$gearGroup)){
	for(p in unique(lands$portComplex)){
		raw[
			raw$marketCategory==m	&
			raw$year==y		&
			raw$qtr==q		&
			raw$gearGroup==g	&
			raw$portComplex==p, 
			'totalLanding'
		] = lands [
			lands$marketCategory==m   &     
        	        lands$year==y             &     
        	        lands$qtr==q              &     
        	        lands$gearGroup==g        &     
        	        lands$portComplex==p,
			'totalLanding'
		]
	}}}}}	

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

makeD = function(sppGold, raw){
	#raw	: raw samples without and implied structure
	#
	#value	: a list of augmented samples aggregated across clusters with multinomial implied structure filling in implied zeros for unobserved species
	writeLines('makeD...\n')

	#
	#PREP D
	#

	#aggregate the categorical variables so we have one observation per species per sample (aggregate cluster samples)
	D = aggregate( data.frame(raw$year, raw$qtr, raw$portComplex, raw$gearGroup, raw$live, raw$totalLanding), by=list(raw$sampleNumber, raw$species), FUN=unique )
	#aggregate add weights such that there is one weight per species per sample (aggregate cluster samples)
	D = cbind(D, aggregate(data.frame(raw$weight), by=list(raw$sampleNumber, raw$species), FUN=sum)[,3])	
	#sum of weights of cluster in each sample
	clustWeight = aggregate( data.frame(raw$weight), by=list(raw$sampleNumber), FUN=sum )
	#match up total sampled weight with species weights by ids
	D = merge(D, clustWeight, by='Group.1')
	#rename D columns
	colnames(D) = c(
	                'id',
	                'species',
	                'year',
	                'qtr',
	                'port',
	                'gear',
	                'live',
			'landing',
	                'weight',
	                'aggClustSize'
	)
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
	        aggClustSize = D$aggClustSize[wid[1]]
	        port 	= D$port[wid[1]]
	        gear 	= D$gear[wid[1]]
	        year 	= D$year[wid[1]]
	        qtr  	= D$qtr[wid[1]]
		live 	= D$live[wid[1]]
		landing	= D$landing[wid[1]]
		#each sample should have at least a zero for each species
	        for(sn in sppGold[!sppGold%in%D$species[wid]]){
			#
	                end = end + 1
	                #
	                D$id[end]   	= id
	                D$aggClustSize[end] 	= aggClustSize
	                D$port[end] 	= port
	                D$gear[end] 	= gear
	                D$year[end] 	= year
	                D$qtr[end]  	= qtr
	                D$species[end]  = sn
			D$live[end]	= live
			D$landing[end]  = landing
	                #
	                D$weight[end] 	= 0
	        }
	}
	#
	D = as.data.frame(D)
	D = D[, c('id', 'species', 'year', 'qtr', 'port', 'gear', 'live', 'aggClustSize', 'landing',	'weight')]
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
	                D$aggClustSize[end]= fill
	                D$port[end]   = p
	                D$gear[end]   = g
	                D$year[end]   = y
	                D$qtr[end]    = q
	                D$species[end]= s
                        D$live[end]   = 'N'	
	        }
	
	}}}}
	}
	#
	return( as.data.frame(D) )
}


