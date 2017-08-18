rm(list=ls())

#
suppressMessages(library(partitions, quietly=FALSE))
suppressMessages(library(RJDBC, quietly=FALSE)) #also requires "./sqljdbc4.jar"

#
#FUNCTIONS
#

#
stir = function(n, k){
 	#n: Total number of items to consider partitions amoung (scalar[integer])
	#k: The number of partitions (scalar[integer] -or- vector[integers])
	#
	#value:
	#The stirling number associated with n and k.
	#The number of different ways to partition a set on n items into k bits (scalar[integer] -or- vector[integers])
	
	#
        js = seq(0,k)
        out = factorial(k)^(-1) * sum( (-1)^(k-js) * choose(k,js) * js^n )
        #
        return( out )
}
stir = Vectorize(stir, c('k'))

#
Tk = function(N){
	#N: the number of items to consider partitions amoung (scalar[integer] -or- vector[integers])
	#
	#value:
	#the bell number associated with N (scalar[integer] -or- vector[integers])
	
        #
        return( sum(stir(N,seq(1,N))) )
}
Tk = Vectorize(Tk, c('N'))

#
isAdj = function(lst, jmp){
	#lst: a list of vectors defining partitions via indicies [integers] on some reference vector
	#jmp: the size of jumps between indicies (scalar[integer])
	#
	#value:
	#a boolean indicating whether the partition scheme defined in 'lst' is a valid partions given 'jmp'	

        #
        for(el in lst){
                el = sort(el)
                nel = length(el)
                if(nel>1){
                        for( i in seq(1, nel-1) ){
                                if( abs(el[i]-el[i+1])>jmp ){ return(F) }#!=1 ){ return(F) }
                        }
                }
        }
        #
        return(T)
}

#
isSml = function(lst, size){
        #lst: a list of vectors defining partitions via indicies [integers] on some reference vector
        #size: the size of super groups (scalar[integer])
        #
        #value:
        #a boolean indicating whether the partition scheme defined in 'lst' is a valid partions given 'size'     

        #
        for(el in lst){
		if( length(el)<=size ){ return(T) 
		}else{ return(F) } 
        }
}

#
#CLEAN DATA
#

##NOTE: add a call to this script in the conductor script
##NOTE: update to pull from a windows data base
#
##import '../../data/data83to90Full.csv'
#data = read.csv('./data83to90Full.csv', header=F) #'../../../data/data83to90.csv'
#colnames(data) = c(
#	'sampleNumber', 
#	'clusterNumber', 
#	'species', 
#	'weight', 
#	'year', 
#	'qtr', 
#	'calPort', 
#	'portComplex', 
#	'gearGroup', 
#	'marketCategory', 
#	'totalWeight', 
#	'live'
#)
#
##clean
#data$sampleNumber = as.character(data$sampleNumber)
#data$species = sapply(as.character(data$species), FUN=function(s){gsub(' ', '', s)})
#data$portComplex = as.character(data$portComplex)
#
##aggregate the categorical variables so we have one observation per species per sample (aggregate cluster samples)
#dataClean = aggregate( data.frame(data$year, data$qtr, data$portComplex, data$gearGroup, data$marketCategory, data$live), by=list(data$sampleNumber, data$species), FUN=unique )
##aggregate add weights such that there is one weight per species per sample (aggregate cluster samples)
#dataClean = cbind(dataClean, aggregate(data.frame(data$weight), by=list(data$sampleNumber, data$species), FUN=sum)[,3])
##preallocate for cluster size
#dataClean = cbind( dataClean, rep(NA, dim(dataClean)[1]) )
##sum of weights of cluster in each sample
#clustWeight = aggregate( data.frame(data$weight), by=list(data$sampleNumber), FUN=sum )
##match up total sampled weight with species weights 
#for(cw in 1:dim(clustWeight)[1]){ dataClean[dataClean[,1]==clustWeight[cw, 1], dim(dataClean)[2]]=clustWeight[cw, 2] }
##rename dataClean columns
#colnames(dataClean) = c(
#       	'sampleNumber', 
#       	'species', 
#       	'year', 
#       	'qtr', 
#       	'portComplex', 
#       	'gearGroup', 
#       	'marketCategory', 
#       	'live', 
#       	'weights',
#	'clustSize'
#)
#dataClean$portComplex = as.character(dataClean$portComplex)
#
##aggregate gear to Trawl, Line/Trap?, Net
#dataClean$gearGroup[dataClean$gearGroup=='MDT'] = 'TWL'
#dataClean$gearGroup[dataClean$gearGroup=='FPT'] = 'HKL'
#dataClean$gearGroup = as.character(dataClean$gearGroup)
#dataClean = dataClean[dataClean$gearGroup!='UNK',]
#
##other species
#dataClean$species[dataClean$species=="MISC"] = "OTHR"
##NOTE: URCK issue
##NOTE: resolve other species I don't currently have in Don's species list
##NOTE: window <=> brown / china <=> green data handeling
#
##NOTE: live/dead
#dataClean = dataClean[dataClean$live=='N',]

#
#DATA
#

#driver
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
#connection
ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=Grunloh_db', 'nick.grunloh', 'Nmfsswfsc2016')
#data query
data = dbGetQuery(ch,
        "
        select
                master_com_clusts.sample_no,
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
        
        from master_com_samples inner join master_com_clusts
                ON master_com_samples.sample_no=master_com_clusts.sample_no
        "
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
       'marketCategory', 
       'totalWeight', 
       'live'
)
#save a local version of data for future reference
write.csv(data, 'data83To90.csv')
#data = read.csv('./data83to90.csv', header=T)

#aggregate the categorical variables so we have one observation per species per sample (aggregate cluster samples)
dataClean = aggregate( data.frame(data$year, data$qtr, data$portComplex, data$gearGroup, data$marketCategory, data$live), by=list(data$sampleNumber, data$species), FUN=unique )
#aggregate add weights such that there is one weight per species per sample (aggregate cluster samples)
dataClean = cbind(dataClean, aggregate(data.frame(data$weight), by=list(data$sampleNumber, data$species), FUN=sum)[,3])
#preallocate for cluster size
dataClean = cbind( dataClean, rep(NA, dim(dataClean)[1]) )
#sum of weights of cluster in each sample
clustWeight = aggregate( data.frame(data$weight), by=list(data$sampleNumber), FUN=sum )
#match up total sampled weight with species weights 
for(cw in 1:dim(clustWeight)[1]){ dataClean[dataClean[,1]==clustWeight[cw, 1], dim(dataClean)[2]]=clustWeight[cw, 2] }
#rename dataClean columns
colnames(dataClean) = c(
               	'sampleNumber', 
               	'species', 
               	'year', 
               	'qtr', 
               	'portComplex', 
               	'gearGroup', 
               	'marketCategory', 
               	'live', 
               	'weights',
       		'clustSize'
)
dataClean$portComplex = as.character(dataClean$portComplex)
dataClean$gearGroup = as.character(dataClean$gearGroup)

#aggregate gear to OTH=UNK+FPT; TWL=TWL+MDT; HKL; NET
dataClean$gearGroup[dataClean$gearGroup=='MDT'] = 'TWL'
dataClean$gearGroup[dataClean$gearGroup=='FPT'] = 'OTH'
dataClean$gearGroup[dataClean$gearGroup=='UNK'] = 'OTH'

#
#GOLD STANDARDS
#

##define port data structure
#portGolds = cbind(c('OSF', 'BDG', 'BRG', 'ERK', 'CRS'), c('MNT', 'MRO', 'OSB', 'OLA', 'OSD'))
##a label to use for building the filesystem
#sudoTypes = c('Top', 'Bottom')
#colnames(portGolds) = sudoTypes
#define full state port data structure
portGolds = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
#define market categories	#NOTE: limit to MCAT250
mcatGold = 250 #956 #959 # 270 # 250 #			#unique(dataClean$marketCategory)
#define quarter data structure
qtrGold  = 1:4
qtrs = dataClean$qtr
nQtr = length(qtrGold)
#define gear data structure
#gearGold = dbGetQuery(ch, 
#	"	
#	select gear_grp
#	from [Grunloh_db].[dbo].[Internal_gear_grp_codes]
#	"
#)
gearGold = c('OTH', 'TWL', 'HKL', 'NET')
nGear = length(gearGold)
gears = dataClean$gearGroup
#define year data structure
yearGold = 1983:1990 #NOTE: eventually this will come from an sql table 
yearEff = yearGold
nYear = length(yearGold)
#for use to clear palette after each of following iteration
dataGold = dataClean

#
#TRANSFORM
#

#a name to use for building the filesystem
name = 'Space'
#
#combinoric indicies for quarters (no constraint: straight bell number)
qll = listParts( nQtr )
nqll = length(qll)

#combinoric indicies for quarters (no constraint: straight bell number)
gll = listParts( nGear )
ngll = length(gll)
#
write("MCAT,tb,n,p", file="./nominal.csv")

for(m in mcatGold){	
	#restrict to current market category
	dataClean = dataGold[dataGold$marketCategory==m,]
	#define species data structure
	sppGold = unique(dataClean$species) 
	sppEff = sppGold
	nSpp = length(sppGold)
	#
	st = 'Top' #for(st in sudoTypes){	
		#reset dataClean for top bottom
		dataClean = dataGold[dataGold$marketCategory==m,]
		##portCleaning dependent on Top/Bottom sudoType
		#portGold = portGolds[,st]
		portGold = portGolds
		dataClean = dataClean[dataClean$portComplex%in%portGold,]
		ports = dataClean$portComplex
		nPort = length(portGold)

		#combinatoric indices for ports (with spatial constraint)
		ll = listParts( nPort )
		nll = length(ll)
		#
		cstll = list()
		for(i in seq(1, nll)){
		       	#isAdj(xx, 1) : no leapfrogging
		       	#isAdj(xx, 2) : single port leapfrogging
		       	#isAdj(xx, 3) : double port leapfrogging
		       	#...
			#isSml(xx, 1): super groups 1 big (all separate)
			#isSml(xx, 2): super groups 2 big (1 port sharing)
			#isSml(xx, 3): super groups 3 big (2 port sharing)
			#...
		       	#the constraint    v		       v
			if( isAdj(ll[[i]], 1) & isSml(ll[[i]], 3) ){ cstll[[length(cstll)+1]] = ll[[i]] }
		}
		#
		ll = cstll
		nll = length(ll)
			
		#
		#MAKE SUPER-GROUPS
		#	
			
		#ports
		ii = 1
		flag = 1
		for(i in seq(1, nll)){	
			#reset
			dataClean$portComplex = ports 		
		
			#
			el = ll[[i]]
			nel = length(el)
			#
			portEff = c()
			for(j in seq(1, nel)){
				#
				l = el[[j]]
				nl = length(l)
				#
				superGroup = paste(portGold[l], collapse='/')
				dataClean$portComplex[ports%in%portGold[l]] = superGroup
				#
				portEff = unique(c(portEff, superGroup))
			}
			
			#
			gearEff = sort(unique(gears))
			qtrEff = sort(unique(qtrs))
			##gears
			#for(gi in seq(1, ngll)){
			#	#reset
			#	dataClean$gearGroup = gears
			#	
			#	#
		        #	gel = gll[[gi]]
		        #	gnel = length(gel)
			#	#
			#	gearEff = c()
			#	for(gj in seq(1, gnel)){ 
		        #        	#
		        #        	gl = gel[[gj]]
		        #        	gnl = length(gl)
		        #        	#
		        #        	superGroupG = paste(gearGold[gl], collapse='/')
		        #        	dataClean$gearGroup[gears%in%gearGold[gl]] = superGroupG
			#		#
			#		gearEff = unique(c(gearEff, superGroupG))
		        #	}	
			#	
			#	#quarters
			#	for(qi in seq(1, nqll)){		
			#		#reset
		        #        	dataClean$qtr = qtrs
			#		
			#		#
		        #        	qel = qll[[qi]]
		        #        	qnel = length(qel)
		        #        	#
		        #        	qtrEff = c()
		        #        	for(qj in seq(1, qnel)){
		        #        	        #
		        #        	        ql = qel[[qj]]
		        #        	        qnl = length(ql)
		        #        	        #
		        #        	        superGroupQ = paste(qtrGold[ql], collapse='/')
		        #        	        dataClean$qtr[qtrs%in%qtrGold[ql]] = superGroupQ
		        #        	        #
		        #        	        qtrEff = unique(c(qtrEff, superGroupQ))
		        #        	}

					#
					#SAVE
					#
					
					#only build models with ____ constraint
					nHat = dim(dataClean)[1]
					pHat = length(sppEff) + length(gearEff) + 4 #4=port+year+qtr+YQ
					#
					if( nHat>=pHat ){	
						#
						dir.create(sprintf('./MCAT%s', m), showWarnings = FALSE)
						dir.create(sprintf('./MCAT%s/%s', m, st), showWarnings = FALSE)	
						#create instance directory
						type = sprintf('%s%d', name, ii)
						path = sprintf('./MCAT%s/%s/%s/', m, st, type)
						dir.create(path, showWarnings = FALSE)	
		
						#save names to instance directory
						save(dataClean, path, name, type, 
							ll, portGold, portEff, nPort,
							qll, qtrGold, qtrEff, nQtr,
							gearGold, gearEff, nGear,
							yearGold, yearEff, nYear,
							sppGold, sppEff, nSpp,
						        file = sprintf('%s%s.RData', path, type)
						)
						
						#make makeDesign instance in instance directory
						fileStr = readChar('tempMakeDesign.r', file.info('tempMakeDesign.r')$size)
						#
						fileStr = gsub('<<type>>', sprintf('\'%s\'', type), fileStr)
						#fileStr = gsub('<<name>>', sprintf('\'%s\'', name), fileStr)
						#
						writeLines(fileStr, sprintf('%s%sMakeDesign.r', path, type))
						#build sampler
						file.copy('sampler.r', path)	
					}else{ if( flag ){
						write(sprintf("%s,%s,%d,%d", m, st, nHat, pHat), file="./nominal.csv", append=TRUE)
						flag = 0 
					}}
					
					#
					ii = ii+1 
				#}
			#}
			#build postProcess directory
			ppPath = sprintf('./MCAT%s/%s/', m, st)
			if( file.exists(ppPath) ){
				file.copy('postProcessScripts/', ppPath, recursive=T)
				#build postProcesser
				fileStr = readChar('tempPostProcess.r', file.info('tempPostProcess.r')$size)
				fileStr = gsub('<<name>>', sprintf('\'%s\'', name), fileStr)
				writeLines(fileStr, sprintf('%spostProcessScripts/postProcess.r', ppPath))
			}
		}
	#}
}















#
#JUNK YARD
#

#if( !any(list.files()==sprintf('%s%s', name, type)) ){
					#        system(sprintf('mkdir %s', path))
					#}
##for mcatGold
#	#for sudoTypes
##restrict to one market category
#dataClean = dataClean[dataClean$marketCategory==250,]
##
#dataClean = dataClean[dataClean$portComplex%in%portGold,]
#dataClean$portComplex = as.character(dataClean$portComplex)
#ports = dataClean$portComplex
#nPort = length(portGold)
##define quarter data structure
#qtrGold  = 1:4 #NOTE: string?
#qtrs = dataClean$qtr
#nQtr = length(qtrGold)
##define gear data structure
#gearGold = c("TWL", "NET", "HKL")
#nGear = length(gearGold)
#gears = dataClean$gearGroup
##define year data structure
#yearGold = 1983:1990 #NOTE: string?
#yearEff = yearGold
#nYear = length(yearGold)
##define species data structure
#sppGold = unique(dataClean$species) #NOTE: get a master species list?
#sppEff = sppGold
#nSpp = length(sppGold)
#
##combinatoric indices for ports (with spatial constraint)
#ll = listParts( nPort )
#nll = length(ll)
##
#cstll = list()
#for(i in seq(1, nll)){
#	#isAdj(xx, 1) : no leapfrogging
#	#isAdj(xx, 2) : single port leapfrogging
#	#isAdj(xx, 3) : double port leapfrogging
#	#...
#	#the constraint    v
#        if( isAdj(ll[[i]], 1) ){ cstll[[length(cstll)+1]] = ll[[i]] }
#}
##
#ll = cstll
#nll = length(ll)
#
##combinoric indicies for quarters (no constraint: straight bell number)
#qll = listParts( nQtr )
#nqll = length(qll)
#
##combinoric indicies for quarters (no constraint: straight bell number)
#gll = listParts( nGear )
#ngll = length(gll)
#
##
##MAKE SUPER-GROUPS
##	
#
##a name to use for building the filesystem
#name = 'Space'
##ports
#ii = 1
#for(i in seq(1, nll)){	
#	#reset
#	dataClean$portComplex = ports 		
#
#	#
#	el = ll[[i]]
#	nel = length(el)
#	#
#	portEff = c()
#	for(j in seq(1, nel)){
#		#
#		l = el[[j]]
#		nl = length(l)
#		#
#		superGroup = paste(portGold[l], collapse='/')
#		dataClean$portComplex[ports%in%portGold[l]] = superGroup
#		#
#		portEff = unique(c(portEff, superGroup))
#	}
#
#	#
#	gearEff = sort(unique(gears))
#	qtrEff = sort(unique(qtrs))
#	##gears
#	#for(gi in seq(1, ngll)){
#	#	#reset
#	#	dataClean$gearGroup = gears
#	#	
#	#	#
#        #	gel = gll[[gi]]
#        #	gnel = length(gel)
#	#	#
#	#	gearEff = c()
#	#	for(gj in seq(1, gnel)){ 
#        #        	#
#        #        	gl = gel[[gj]]
#        #        	gnl = length(gl)
#        #        	#
#        #        	superGroupG = paste(gearGold[gl], collapse='/')
#        #        	dataClean$gearGroup[gears%in%gearGold[gl]] = superGroupG
#	#		#
#	#		gearEff = unique(c(gearEff, superGroupG))
#        #	}	
#	#	
#	#	#quarters
#	#	for(qi in seq(1, nqll)){		
#	#		#reset
#        #        	dataClean$qtr = qtrs
#	#		
#	#		#
#        #        	qel = qll[[qi]]
#        #        	qnel = length(qel)
#        #        	#
#        #        	qtrEff = c()
#        #        	for(qj in seq(1, qnel)){
#        #        	        #
#        #        	        ql = qel[[qj]]
#        #        	        qnl = length(ql)
#        #        	        #
#        #        	        superGroupQ = paste(qtrGold[ql], collapse='/')
#        #        	        dataClean$qtr[qtrs%in%qtrGold[ql]] = superGroupQ
#        #        	        #
#        #        	        qtrEff = unique(c(qtrEff, superGroupQ))
#        #        	}
#			
#			#
#			#SAVE
#			#
#
#			#
#			type = sprintf('%s%d', sudoType, ii)
#			#create instance directory
#			path = sprintf('./%s%s/', name, type)
#			if( !any(list.files()==sprintf('%s%s', name, type)) ){
#			        system(sprintf('mkdir %s', path))
#			}
#
#			#save names to instance directory
#			save(dataClean, path, name, type, 
#				ll, portGold, portEff, nPort,
#				qll, qtrGold, qtrEff, nQtr,
#				gearGold, gearEff, nGear,
#				yearGold, yearEff, nYear,
#				sppGold, sppEff, nSpp,
#			        file = sprintf('%s%s%s.RData', path, name, type)
#			)
#			
#			#make makeDesign instance in instance directory
#			fileStr = readChar('tempMakeDesign.r', file.info('tempMakeDesign.r')$size)
#			#
#			fileStr = gsub('<<type>>', sprintf('\'%s\'', type), fileStr)
#			fileStr = gsub('<<name>>', sprintf('\'%s\'', name), fileStr)
#			#
#			writeLines(fileStr, sprintf('%s%s%sMakeDesign.r', path, name, type))
#			
#			#
#			ii = ii+1 
#		#}
#	#}
#}






