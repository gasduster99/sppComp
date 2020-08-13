#
#DEPENDENCIES
#

#
suppressMessages(library(doParallel, quietly=FALSE))
suppressMessages(library(foreach, quietly=FALSE))
suppressMessages(library(RSQLite, quietly=FALSE))
suppressMessages(library(dplyr, quietly=FALSE))
suppressMessages(library(DBI, quietly=FALSE))
#
source('dataFunkFile.r')

#
#FUNCTIONS
#

#
balanceWork = function(minYear, maxYear, nodes, modThresh=15, user=getPass('User:'), password=getPass('Password:'), threads=detectCores()){
	#
	writeLines('balanceWork...\n')
	
	#	
	raw = getRawData(minYear, maxYear, user=user, password=password)
	mcatGold = unique(raw$marketCategory)	
	#
	liveGold = c('Y', 'N')
	#
	meshMcatLive = split(expand.grid(mcatGold, liveGold, stringsAsFactors=F), rep(1:(length(liveGold)*length(mcatGold))))
	registerDoParallel(cores=threads)
	mcoptions = list(preschedule=FALSE, set.seed=FALSE)
	costs = foreach( x=meshMcatLive, .options.multicore=mcoptions)%dopar%{
		#unpack inputs
		mcat = unlist(x[1])
		live = unlist(x[2])
		
		#subset to mcat/live
        	rawMcatLive = raw[raw$marketCategory==mcat & raw$live==live,]
		
		#now I define sppGold from the data
        	sppGold = unique(rawMcatLive$species)
        	#
        	if( length(sppGold)>1 & dim(rawMcatLive)[1]>0 ){
        	        #add implied multinomial species structure
        	        capture.output(D<-makeD(sppGold, rawMcatLive))
        	        #add predictive structure 
        	        capture.output(DPred<-addPredStrat(sppGold, portGold, gearGold, yearGold, qtrGold, D))
		        #15 parameters = 10 ports + 3 gears + 1 YQ + 1 SP
        	        #18 parameters = 10 ports + 4 qtrs + 3 gears + 1 year 
        	        #19 parameters = 10 ports + 4 qtrs + 3 gears + 1 year + 1 SP
        	        if( dim(D)[1]>modThresh ){
        	                #writeLines(sprintf('\n\nMCAT %d%s', mcat, live))
        	                #costs[[sprintf('%d%s', mcat, live)]] = dim(DPred)[1]*length(sppGold)
        	        	return( dim(DPred)[1]*length(sppGold) )
			}else{
				return( NA )
			}
        	}else{ return( NA ) }
	}
	names(costs) = sapply(meshMcatLive, function(x){sprintf('%s%s', x[1], x[2])} ) 
	costs = na.omit(unlist(costs))
	costs = sort(costs, decreasing=T)
	#
	sol = list()
	solCosts = rep(0, length(nodes))
	for(i in 1:length(costs)){
	       #
	       where = which(solCosts==min(solCosts))[1]
	       node = nodes[where]
	       sol[[node]] = c(sol[[node]], names(costs)[i])
	       #print(where)
	       #print(solCosts)
	       #print(costs) 
	       solCosts[where] = solCosts[where] + costs[i]
	}

	#
	return( list(solution=sol, solutionCosts=solCosts) )
}

#
exportMcat = function( samplePath, outFile, portGold, gearGold, yearGold, qtrGold, mcat, live, key, archive=F){ #, user=getPass('User:'), password=getPass('Password:') ){
	#crawl path and concatonate each draw from each stratum in a single flat file at outPath
	#
	writeLines("Export...\n")

	##
	##BUILD KEY
	##

	##make an encoding key to save space in the db
	#key = list(live=list(), gear=list(), port=list(), species=list())
	##pull db encoding
        #writeLines('******** Query ********')
        ##driver
        #drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
        ##connection
        #ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=COMX', user, password)	
	##fill key
	#liveCodes = dbGetQuery(ch, 'select [calcom_code], [internal_code] from [COMX].[dbo].[Internal_Live_fish_codes]' )
	#for(i in 1:nrow(liveCodes)){ key[['live']][[liveCodes[i,1]]]=liveCodes[i,2] }
	#gearCodes = dbGetQuery(ch, 'select [Gear_grp], [Internal_code] from [COMX].[dbo].[Internal_gear_grp_codes]')
	#for(i in 1:nrow(gearCodes)){ key[['gear']][[gearCodes[i,1]]]=gearCodes[i,2] }
	#portCodes = dbGetQuery(ch, 'select [Port_complex], [Internal_Code] from [COMX].[dbo].[Internal_Port_Complex_codes]')
	#for(i in 1:nrow(portCodes)){ key[['port']][[portCodes[i,1]]]=portCodes[i,2] }
	#sppCodes  = dbGetQuery(ch, 'select [pacfin_code], [Internal_Code] from [COMX].[dbo].[Internal_species_codes]')
	#for(i in 1:nrow(sppCodes)){ key[['species']][[trimws(sppCodes[i,1])]]=sppCodes[i,2] }	
	##
	#writeLines('****** Complete *******\n')	

	#
	#EXPORT
	#
	
	#
	for(p in portGold){
        for(g in gearGold){
        for(q in qtrGold ){
        for(y in yearGold){
                #read
                sp = read.csv(sprintf('%s/%s/%s/%s/%s/sppComp.csv', samplePath, p, g, q, y), stringsAsFactors=F)
		M = nrow(sp)
		#
		for(s in colnames(sp)){ #[!colnames(sp)%in%c('OWFS')]){
			#encode
			out = cbind( 
				rep(key$live[[toupper(trimws(live))]], M), 
				rep(mcat, M), 
				rep(key$port[[p]], M), 
				rep(key$gear[[g]], M), 
				rep(q, M), 
				rep(y, M), 
				rep(key$species[[s]], M), 
				sp[,s],
				1:M 
			)
			colnames(out) = c('live','mcat','port','gear','qtr','year','spp','comp','draw')
			#write
			if( file.exists(outFile) ){ 
				write.table(out, outFile, append=T, quote=F, row.names=F, col.names=F, sep=',')
			}else{ 	write.table(out, outFile, append=T, quote=F, row.names=F, col.names=T, sep=',') }
		}
	}}}}
	
	#
	#ARCHIVE
	#
	
	#
	if( archive ){
		#
		system(sprintf('zip -r %s/%s.zip %s', dirname(samplePath), basename(samplePath), samplePath))#, show.output.on.console=F)
		system(sprintf('rm -r %s', samplePath)) #, show.output.on.console=F)
	}
}

getKey = function( user=getPass('User:'), password=getPass('Password:') ){
	#

	#
	writeLines('getKey...\n')
	#make an encoding key to save space in the db
	key = list(live=list(Y=0, N=1), gear=list(), port=list(), species=list())
	#pull db encoding
	writeLines('******** Query ********')
	#driver
	drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
	#connection
	ch = dbConnect(drv, 'jdbc:sqlserver://161.55.237.17;databaseName=COMX', user, password)
	##fill key
	#liveCodes = dbGetQuery(ch, 'select [calcom_code], [internal_code] from [COMX].[dbo].[Internal_Live_fish_codes]' )
	#for(i in 1:nrow(liveCodes)){ key[['live']][[liveCodes[i,1]]]=liveCodes[i,2] }
	gearCodes = dbGetQuery(ch, 'select [gear_grp], [internal_code] from [COMX].[dbo].[GEAR_CODES]')
	for(i in 1:nrow(gearCodes)){ key[['gear']][[gearCodes[i,1]]]=gearCodes[i,2] }
	portCodes = dbGetQuery(ch, 'select [port_complex], [internal_code] from [COMX].[dbo].[port_codes]')
	for(i in 1:nrow(portCodes)){ key[['port']][[portCodes[i,1]]]=portCodes[i,2] }
	sppCodes  = dbGetQuery(ch, 'select [species], [internal_code] from [COMX].[dbo].[SPECIES_CODES]')
	for(i in 1:nrow(sppCodes)){ key[['species']][[trimws(sppCodes[i,1])]]=sppCodes[i,2] }
	#
	writeLines('****** Complete *******\n')

	#
	return( key )
}

#
lightConnect = function(dbName){
	#dbName: a string to use as a name for the database
	#
	#value: return a connection to the database	

	#
	if( !file.exists(dbName) ){
        	con = DBI::dbConnect(RSQLite::SQLite(), dbname=dbName, synchronous='full' )
        	dbExecute( con, "pragma journal_mode=wal;" )
	} else{
	        con = DBI::dbConnect(RSQLite::SQLite(), dbname=dbName, synchronous='full' )
	}
	
	#
	return(con)
}

#

dbExportMcat = function( samplePath, dbName, portGold, gearGold, yearGold, qtrGold, mcat, live, key, archive=F){ #, user=getPass('User:'), password=getPass('Password:') ){
	#crawl path and concatonate each draw from each stratum in a single flat file at outPath
	#
	writeLines("Export...\n")

	##
	##BUILD KEY
	##

	##make an encoding key to save space in the db
	#key = list(live=list(), gear=list(), port=list(), species=list())
	##pull db encoding
        #writeLines('******** Query ********')
        ##driver
        #drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
        ##connection
        #ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=COMX', user, password)	
	##fill key
	#liveCodes = dbGetQuery(ch, 'select [calcom_code], [internal_code] from [COMX].[dbo].[Internal_Live_fish_codes]' )
	#for(i in 1:nrow(liveCodes)){ key[['live']][[liveCodes[i,1]]]=liveCodes[i,2] }
	#gearCodes = dbGetQuery(ch, 'select [Gear_grp], [Internal_code] from [COMX].[dbo].[Internal_gear_grp_codes]')
	#for(i in 1:nrow(gearCodes)){ key[['gear']][[gearCodes[i,1]]]=gearCodes[i,2] }
	#portCodes = dbGetQuery(ch, 'select [Port_complex], [Internal_Code] from [COMX].[dbo].[Internal_Port_Complex_codes]')
	#for(i in 1:nrow(portCodes)){ key[['port']][[portCodes[i,1]]]=portCodes[i,2] }
	#sppCodes  = dbGetQuery(ch, 'select [pacfin_code], [Internal_Code] from [COMX].[dbo].[Internal_species_codes]')
	#for(i in 1:nrow(sppCodes)){ key[['species']][[trimws(sppCodes[i,1])]]=sppCodes[i,2] }	
	##
	#writeLines('****** Complete *******\n')	

	#
	#EXPORT
	#
	
	#
	con = lightConnect(dbName)
	#
	for(p in portGold){
        for(g in gearGold){
        for(q in qtrGold ){
        for(y in yearGold){
                #read
                sp = read.csv(sprintf('%s/%s/%s/%s/%s/sppComp.csv', samplePath, p, g, q, y), stringsAsFactors=F)
		M = nrow(sp)
		#
		for(s in colnames(sp)){ #[!colnames(sp)%in%c('OWFS')]){
			#encode
			out = cbind( 
				rep(key$live[[toupper(trimws(live))]], M), 
				rep(mcat, M), 
				rep(key$port[[p]], M), 
				rep(key$gear[[g]], M), 
				rep(q, M), 
				rep(y, M), 
				rep(key$species[[s]], M), 
				sp[,s],
				1:M 
			)
			colnames(out) = c('live','mcat','port','gear','qtr','year','spp','comp','draw')
			#write to db
			dbWriteTable(conn=con, name="export", value=as.data.frame(out), append=T)
			##write
			#if( file.exists(outFile) ){ 
			#	write.table(out, outFile, append=T, quote=F, row.names=F, col.names=F, sep=',')
			#}else{ 	write.table(out, outFile, append=T, quote=F, row.names=F, col.names=T, sep=',') }
		}
	}}}}
	
	#
	#ARCHIVE
	#
	
	#
	if( archive ){
		#
		system(sprintf('zip -r %s/%s.zip %s', dirname(samplePath), basename(samplePath), samplePath))#, show.output.on.console=F)
		system(sprintf('rm -r %s', samplePath)) #, show.output.on.console=F)
	}
}
