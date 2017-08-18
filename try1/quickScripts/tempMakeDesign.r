rm(list=ls())

#
#MODEL IDS
#

#
writeLines('\nRunning Design File\n...')
#defined in preProcess.r 
type = <<type>> 
#
load(sprintf('./%s.RData', type))
lss = ls() #for saving at end of this file
#T: 2-way only; F: add 3-way
#not3Way = T

#
#BUILD MODEL DATA STRUCTURE
#

#
end = length(dataClean$sampleNumber)
#
D = list()
D$id     = dataClean$sampleNumber
D$weight = dataClean$weights
D$off    = dataClean$clustSize
D$year   = dataClean$year
D$spp    = dataClean$species
#
if( length(portEff)>1 ){ D$port=dataClean$portComplex }else{ D$port=rep(1, end) }
if( length(gearEff)>1 ){ D$gear=dataClean$gearGroup   }else{ D$gear=rep(1, end) }
if( length(qtrEff)>1  ){ D$qtr=dataClean$qtr          }else{ D$qtr=rep(1, end)  }

#
#MAKE FULL DATA
#

#zero data
for(id in unique(D$id)){
        #
        wid = which(D$id==id)
        #
        off  = D$off[wid[1]]
        port = D$port[wid[1]]
        gear = D$gear[wid[1]]
        year = D$year[wid[1]]
        qtr  = D$qtr[wid[1]]
        #       
        for(sn in sppEff[!sppEff%in%D$spp[wid]]){
                #
                end = end + 1
                #
                D$id[end]   = id
                D$off[end]  = off
                D$port[end] = port
                D$gear[end] = gear
                D$year[end] = year
                D$qtr[end]  = qtr
                D$spp[end]  = sn
                #
                D$weight[end] = 0
        }
}

#
#UNOBSERVED DATA
#

#prediction sum cluster size 
fill = 100
#
if( length(portEff)>1 ){ pIt=portEff }else{ pIt=c(1) }
if( length(gearEff)>1 ){ gIt=gearEff }else{ gIt=c(1) }
if( length(yearEff)>1 ){ yIt=yearEff }else{ yIt=c(1) }
if( length( qtrEff)>1 ){ qIt=qtrEff  }else{ qIt=c(1) }
if( length( sppEff)>1 ){ sIt=sppEff  }else{ sIt=c(1) }
#
for(p in pIt){
for(g in gIt){
for(q in qIt){
for(y in yIt){
for(s in sIt){
        #
        wJoint = which(
                D$port==p       &
                D$gear==g       &
                D$year==y       &
                D$qtr==q        &
                D$spp==s
        )
        #
        if( length(wJoint)==0 ){
                #data grows by a single row with 0 weight
                end = end + 1
                #
                D$id[end]     = NA
                D$weight[end] = NA
                D$off[end]    = fill
                D$port[end]   = p
                D$gear[end]   = g
                D$year[end]   = y
                D$qtr[end]    = q
                D$spp[end]    = s

        }

}}}}
}

#
#INTERACTIONS
#

#I save disk space by not building the interactions which are not used in the final model.
#However, the following commented code will build many of the two and three way interactions that I played with.
#I start by building data structres for implimenting a fine-tooth heriarchical prior. 
#Later I aggregate across these fine-tooth data structures to build structures for more heirarchical sharing. 

##2-way interaction data structures
##port
#eval(parse( text=sprintf('D$pGiven%s=matrix(NA, nrow=end, ncol=1)', gsub("/", "_", gearEff)) ))
#eval(parse( text=sprintf('D$pGiven%s=matrix(NA, nrow=end, ncol=1)', gsub("/", "_", qtrEff)) ))
#eval(parse( text=sprintf('D$pGiven%s=matrix(NA, nrow=end, ncol=1)', yearEff) ))
#eval(parse( text=sprintf('D$pGiven%s=matrix(NA, nrow=end, ncol=1)', sppEff) ))
##gear
#eval(parse( text=sprintf('D$gGiven%s=matrix(NA, nrow=end, ncol=1)', gsub("/", "_", qtrEff)) ))
#eval(parse( text=sprintf('D$gGiven%s=matrix(NA, nrow=end, ncol=1)', yearEff) ))
#eval(parse( text=sprintf('D$gGiven%s=matrix(NA, nrow=end, ncol=1)', sppEff) ))
#qtr
eval(parse( text=sprintf('D$qGiven%s=matrix(NA, nrow=end, ncol=1)', yearEff) ))
#eval(parse( text=sprintf('D$qGiven%s=matrix(NA, nrow=end, ncol=1)', sppEff) ))
##3-way interaction data structures
##if( !not3Way ){
#       #
#       for(i in seq(1, nGear)){
#               eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', gearEff[i], yearEff) ))
#               eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', gearEff[i], qtrEff) ))
#               eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', gearEff[i], sppEff) ))
#       }
#       #
#       for(i in seq(1, nYear)){
#               eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', yearEff[i], qtrEff) ))
#               eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', yearEff[i], sppEff) ))
#       }
#       #
#       for(i in seq(1, nQtr)){
#               eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', qtrEff[i], sppEff) ))
#       }
##}

#fill structures
for(w in seq(1, end)){
        #
        #TWO-WAY
        #

        #
        if( length(portEff)>1 ){ p=D$port[w] }else{ p=portEff[1] }
        if( length(gearEff)>1 ){ g=D$gear[w] }else{ g=gearEff[1] }
        if( length(qtrEff)>1 ){ q=D$qtr[w] }else{ q=qtrEff[1] }
        if( length(sppEff)>1 ){ s=D$spp[w] }else{ s=sppEff[1] }
        ##
        #pGg = sprintf('%s|%s', p, g) #D$gear[w])       
        #pGq = sprintf('%s|%s', p, q) #FIX THE CASE WHERE THERE IS A SINGLE CATEGORY
        #pGy = sprintf('%s|%s', p, D$year[w])
        #pGs = sprintf('%s|%s', p, D$spp[w])
        ##
        #gGq = sprintf('%s|%s', g, q)
        #gGy = sprintf('%s|%s', g, D$year[w])
        #gGs = sprintf('%s|%s', g, D$spp[w])
        ##
        qGy = sprintf('%s|%s', q, D$year[w])
        #qGs = sprintf('%s|%s', q, D$spp[w])
        ##
        #eval(parse( text=sprintf('D$pGiven%s[w]=pGq', gsub("/", "_", q)) ))    
        #eval(parse( text=sprintf('D$pGiven%s[w]=pGg', gsub("/", "_", g)) ))
        #eval(parse( text=sprintf('D$pGiven%s[w]=pGy', D$year[w]) ))
        #eval(parse( text=sprintf('D$pGiven%s[w]=pGs', D$spp[w]) ))
        ##
        #eval(parse( text=sprintf('D$gGiven%s[w]=gGq', gsub("/", "_", q)) ))
        #eval(parse( text=sprintf('D$gGiven%s[w]=gGy', D$year[w]) ))
        #eval(parse( text=sprintf('D$gGiven%s[w]=gGs', D$spp[w]) ))
        ##
        eval(parse( text=sprintf('D$qGiven%s[w]=qGy', D$year[w]) ))
        #eval(parse( text=sprintf('D$qGiven%s[w]=qGs', D$spp[w]) ))

        ##
        ##THREE-WAY
        ##
        #
        ##
        ##if( !not3Way ){
        #       #
        #       pGgy = sprintf('%s|%s_%s', p, D$gear[w], D$year[w])
        #       pGgq = sprintf('%s|%s_%s', p, D$gear[w], D$qtr[w])
        #       pGgs = sprintf('%s|%s_%s', p, D$gear[w], D$spp[w])
        #       #
        #       pGyq = sprintf('%s|%s_%s', p, D$year[w], D$qtr[w])
        #       pGys = sprintf('%s|%s_%s', p, D$year[w], D$spp[w])
        #       #
        #       pGqs = sprintf('%s|%s_%s', p, D$qtr[w], D$spp[w])

        #       #
        #       eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGgy', D$gear[w], D$year[w]) ))
        #       eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGgq', D$gear[w], D$qtr[w]) ))
        #       eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGgs', D$gear[w], D$spp[w]) ))
        #       #
        #       eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGyq', D$year[w], D$qtr[w]) ))
        #       eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGys', D$year[w], D$spp[w]) ))
        #       #
        #       eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGqs', D$qtr[w], D$spp[w]) ))
        ##}
}

#
#TRIMMED STRUCTURES
#

##
#D$pGivenG = rep(NA, length(D[[1]]))
#for(g in gearEff){
#        #
#        who = eval(parse( text=sprintf('who = !is.na(D$pGiven%s)', gsub("/", "_", g)) ))
#        eval(parse( text=sprintf('D$pGivenG[who] = D$pGiven%s[who]', gsub("/", "_", g)) ))
#}
##
#D$gGivenQ = rep(NA, length(D[[1]]))
#D$pGivenQ = rep(NA, length(D[[1]]))
#for(q in qtrEff){
#        #
#        who = eval(parse( text=sprintf('who = !is.na(D$pGiven%s)', gsub("/", "_", q)) ))
#        eval(parse( text=sprintf('D$pGivenQ[who] = D$pGiven%s[who]', gsub("/", "_", q)) ))
#       #
#       gwho = eval(parse( text=sprintf('gwho = !is.na(D$gGiven%s)', gsub("/", "_", q)) ))
#        eval(parse( text=sprintf('D$gGivenQ[gwho] = D$gGiven%s[gwho]', gsub("/", "_", q)) ))
#}
##
D$qGivenY = rep(NA, end)
#D$gGivenY = rep(NA, length(D[[1]]))
#D$pGivenY = rep(NA, length(D[[1]]))
for(y in yearEff){
        ##
        #who = eval(parse( text=sprintf('who = !is.na(D$pGiven%s)', y) ))
        #eval(parse( text=sprintf('D$pGivenY[who] = D$pGiven%s[who]', y) ))
        ##
        #gwho = eval(parse( text=sprintf('gwho = !is.na(D$gGiven%s)', y) ))
        #eval(parse( text=sprintf('D$gGivenY[gwho] = D$gGiven%s[gwho]', y) ))
        ##
        eval(parse( text=sprintf('qwho = !is.na(D$qGiven%s)', y) ))
        eval(parse( text=sprintf('D$qGivenY[qwho] = D$qGiven%s[qwho]', y) ))
}
##
#D$qGivenS = rep(NA, length(D[[1]]))
#D$gGivenS = rep(NA, length(D[[1]]))
#D$pGivenS = rep(NA, length(D[[1]]))
#for(s in sppEff){
#        #
#        who = eval(parse( text=sprintf('who = !is.na(D$pGiven%s)', s) ))
#        eval(parse( text=sprintf('D$pGivenS[who] = D$pGiven%s[who]', s) ))
#       #
#        gwho = eval(parse( text=sprintf('gwho = !is.na(D$gGiven%s)', s) ))
#        eval(parse( text=sprintf('D$gGivenS[gwho] = D$gGiven%s[who]', s) ))
#       #
#        qwho = eval(parse( text=sprintf('qwho = !is.na(D$qGiven%s)', s) ))
#        eval(parse( text=sprintf('D$qGivenS[qwho] = D$qGiven%s[qwho]', s) ))
#}

#
#MAKE MODEL
#

#Linear predictor construction may be done programmatically, by building the appropriate string.
#Simple models may simply be specified by typing the desired linear predictor string.
#f(.) indicates a heirarchical prior structure amoung the the parameters implied by the enclosed data structure 

##construct the 2-way port interaction terms
#portCmd = sprintf("f(pGiven%s)", gearEff[1])
#for(i in seq(2, nGear)){ portCmd = sprintf("%s + f(pGiven%s)", portCmd, gearEff[i]) }
#for(i in seq(1, nYear)){ portCmd = sprintf("%s + f(pGiven%s)", portCmd, yearEff[i]) }
#for(i in seq(1, nQtr)){  portCmd = sprintf("%s + f(pGiven%s)", portCmd, qtrEff[i]) }
#for(i in seq(1, nSpp)){  portCmd = sprintf("%s + f(pGiven%s)", portCmd, sppEff[i]) }
##3-way term names
#gY = expand.grid(gearEff, yearEff);
#gQ = expand.grid(gearEff, qtrEff);
#gS = expand.grid(gearEff, sppEff);
##
#yQ = expand.grid(yearEff, qtrEff);
#yS = expand.grid(yearEff, sppEff);
##
#qS = expand.grid(qtrEff, sppEff);
##construct the 3-way port interaction terms
#if( !not3Way ){
#       #
#       for(i in seq(1, dim(gY)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, gY[i, 1], gY[i, 2]) }
#       for(i in seq(1, dim(gQ)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, gQ[i, 1], gQ[i, 2]) }
#       for(i in seq(1, dim(gS)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, gS[i, 1], gS[i, 2]) }
#       #
#       for(i in seq(1, dim(yQ)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, yQ[i, 1], yQ[i, 2]) }
#       for(i in seq(1, dim(yS)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, yS[i, 1], yS[i, 2]) }
#       #
#       for(i in seq(1, dim(qS)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, qS[i, 1], qS[i, 2]) }
#}
#
#model = sprintf('weight ~ spp + port + gear + f(year) + f(qtr) + %s', portCmd)
#model = 'weight ~ spp + port + gear + f(year) + f(qtr) + f(pGivenS) + f(pGivenG) + f(pGivenY) + f(pGivenQ) + f(gGivenS) + f(gGivenQ) + f(gGivenY)'
#NOTE: try model with ... + year + qtr (maybe + qGivenY)
#model = 'weight ~ spp + port + gear + f(year) + f(qtr) + f(qGivenY)'
model = 'weight ~ spp + port + gear + f(year) + f(qtr)'

#
#MAKE MODELING SCRIPT
#

#The temp version of this file is to remain in the workspace directory.
#preProcess.r builds a version of this file for each model which is to reside within the model directory.
#The conductor file is to run the model specific version of this file within the model directory.

#read in tempModel.r as a string; tempModel.r acts a modeling skeleton to be filled
fileStr = readChar('../../../tempModel.r', file.info('../../../tempModel.r')$size)
#38 \thetas is the limit for ccd integration
#intStrat = '\'eb\''
#below the double <> tags to be used in tempModel.r; they are replaced by the values specified below
fileStr = gsub('<<type1>>', sprintf('\'%s\'', type), fileStr)
#fileStr = gsub('<<name1>>', sprintf('\'%s\'', name), fileStr)
fileStr = gsub('<<model>>', model, fileStr)
#fiileStr = gsub('<<intStrat>>', intStrat, fileStr)
#
writeLines(fileStr, sprintf('./%sModel.r', type))

#
#SAVE NECISSARY NAMES
#

#
con = pipe(sprintf("pigz -p%d > %s.RData", 10, type), "wb")
save( D,
        list = lss,
        file = con #sprintf('./%s.RData', type)
)




































#
#JUNK YARD
#

##
##BUILD MODEL DATA STRUCTURE
##
#
##build the strata
#pG = expand.grid(yearEff, qtrEff, sppGold, portEff, gearEff)
##
#D = list()
#D$id     = dataClean$sampleNumber
#D$weight = dataClean$weights
#D$off    = dataClean$clustSize
#D$port   = matrix(NA, nrow=1, ncol=1) 
#D$gear   = matrix(NA, nrow=1, ncol=1) 
#D$year   = matrix(NA, nrow=1, ncol=1) 
#D$qtr    = matrix(NA, nrow=1, ncol=1) 
#D$spp    = matrix(NA, nrow=1, ncol=1) 
#
##
##MAKE FULL DATA
##
#
##length of observed data
#end = length(dataClean$sampleNumber)
##iterate over all of the strata
#for(i in seq(1, dim(pG)[1])){
#	#
#	year = sprintf('%s', pG[i,1])
#	qtr  = sprintf('%s', pG[i,2])
#	spp  = sprintf('%s', pG[i,3])
#	port = sprintf('%s', pG[i,4])
#	gear = sprintf('%s', pG[i,5])
#	#find data in strata	
#	wJoint = which(
#		dataClean$year==year		& 
#		dataClean$qtr==qtr		&
#		dataClean$species==spp		&
#		dataClean$portComplex==port	&
#		dataClean$gearGroup==gear
#	)
#	#
#	D$year[wJoint] = year
#	#D$spp[wJoint] = spp
#	#
#	#the case in which the an aggregated strata is fully lumped
#	if( length(portEff)>1 ){ D$port[wJoint]=port }else{ D$port[wJoint]=1 }
#	if( length(gearEff)>1 ){ D$gear[wJoint]=gear }else{ D$gear[wJoint]=1 }
#	if( length( qtrEff)>1 ){ D$qtr[ wJoint]=qtr  }else{ D$qtr[ wJoint]=1 }
#	if( length( sppEff)>1 ){ D$spp[ wJoint]=spp  }else{ D$spp[ wJoint]=1 }
#	#the case in which the strata was not observed
#	if( length(wJoint)==0 ){ 
#		#data grows by a single row with 0 weight
#		end = end + 1
#		#
#		D$id[end]     = NA
#		D$weight[end] = 0
#		D$year[end]   = year
#		D$off[end]    = NA #0 breaks optimizer
#		#D$qtr[end]    = qtr
#		#D$spp[end]    = spp
#		#D$gear[end]   = gear
#		#
#		if( length(portEff)>1 ){ D$port[end]=port }else{ D$port[end]=1 }
#		if( length(gearEff)>1 ){ D$gear[end]=gear }else{ D$gear[end]=1 }
#		if( length( qtrEff)>1 ){ D$qtr[ end]=qtr  }else{ D$qtr[ end]=1 }
#		if( length( sppEff)>1 ){ D$spp[ end]=spp  }else{ D$spp[ end]=1 }
#	}
#}
#
##
##INTERACTIONS
##
#
##I save disk space by not building the interactions which are not used in the final model.
##However, the following commented code will building many of the two and three way interactions that I played with.
##I start by building data structres for implimenting a fine-tooth heriarchical prior. 
##Later I aggregate across these fine-tooth data structures to build structures for more heirarchical sharing. 
#
###2-way interaction data structures
###port
##eval(parse( text=sprintf('D$pGiven%s=matrix(NA, nrow=end, ncol=1)', gsub("/", "_", gearEff)) ))
##eval(parse( text=sprintf('D$pGiven%s=matrix(NA, nrow=end, ncol=1)', gsub("/", "_", qtrEff)) ))
##eval(parse( text=sprintf('D$pGiven%s=matrix(NA, nrow=end, ncol=1)', yearEff) ))
##eval(parse( text=sprintf('D$pGiven%s=matrix(NA, nrow=end, ncol=1)', sppEff) ))
###gear
##eval(parse( text=sprintf('D$gGiven%s=matrix(NA, nrow=end, ncol=1)', gsub("/", "_", qtrEff)) ))
##eval(parse( text=sprintf('D$gGiven%s=matrix(NA, nrow=end, ncol=1)', yearEff) ))
##eval(parse( text=sprintf('D$gGiven%s=matrix(NA, nrow=end, ncol=1)', sppEff) ))
##qtr
#eval(parse( text=sprintf('D$qGiven%s=matrix(NA, nrow=end, ncol=1)', yearEff) ))
##eval(parse( text=sprintf('D$qGiven%s=matrix(NA, nrow=end, ncol=1)', sppEff) ))
###3-way interaction data structures
###if( !not3Way ){
##	#
##	for(i in seq(1, nGear)){
##		eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', gearEff[i], yearEff) ))
##	        eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', gearEff[i], qtrEff) ))
##		eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', gearEff[i], sppEff) ))
##	}
##	#
##	for(i in seq(1, nYear)){
##	        eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', yearEff[i], qtrEff) ))
##		eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', yearEff[i], sppEff) ))
##	}
##	#
##	for(i in seq(1, nQtr)){
##	        eval(parse( text=sprintf('D$pGiven%s_%s=matrix(NA, nrow=end, ncol=1)', qtrEff[i], sppEff) ))
##	}
###}
#
##fill structures
#for(w in seq(1, end)){
#	#
#	#TWO-WAY
#	#
#	
#	#
#	if( length(portEff)>1 ){ p=D$port[w] }else{ p=portEff[1] }
#	if( length(gearEff)>1 ){ g=D$gear[w] }else{ g=gearEff[1] }
#	if( length(qtrEff)>1 ){ q=D$qtr[w] }else{ q=qtrEff[1] }
#	if( length(sppEff)>1 ){ s=D$spp[w] }else{ s=sppEff[1] }
#	##
#	#pGg = sprintf('%s|%s', p, g) #D$gear[w])	
#	#pGq = sprintf('%s|%s', p, q) #FIX THE CASE WHERE THERE IS A SINGLE CATEGORY
#	#pGy = sprintf('%s|%s', p, D$year[w])
#	#pGs = sprintf('%s|%s', p, D$spp[w])
#	##
#	#gGq = sprintf('%s|%s', g, q)
#        #gGy = sprintf('%s|%s', g, D$year[w])
#        #gGs = sprintf('%s|%s', g, D$spp[w])
#	##
#	qGy = sprintf('%s|%s', q, D$year[w])
#	#qGs = sprintf('%s|%s', q, D$spp[w])
#	##
#	#eval(parse( text=sprintf('D$pGiven%s[w]=pGq', gsub("/", "_", q)) ))	
#	#eval(parse( text=sprintf('D$pGiven%s[w]=pGg', gsub("/", "_", g)) ))
#	#eval(parse( text=sprintf('D$pGiven%s[w]=pGy', D$year[w]) ))
#	#eval(parse( text=sprintf('D$pGiven%s[w]=pGs', D$spp[w]) ))
#	##
#	#eval(parse( text=sprintf('D$gGiven%s[w]=gGq', gsub("/", "_", q)) ))
#	#eval(parse( text=sprintf('D$gGiven%s[w]=gGy', D$year[w]) ))
#	#eval(parse( text=sprintf('D$gGiven%s[w]=gGs', D$spp[w]) ))
#	##
#	eval(parse( text=sprintf('D$qGiven%s[w]=qGy', D$year[w]) ))
#	#eval(parse( text=sprintf('D$qGiven%s[w]=qGs', D$spp[w]) ))
#	
#	##
#	##THREE-WAY
#	##
#	#
#	##
#	##if( !not3Way ){
#	#	#
#	#	pGgy = sprintf('%s|%s_%s', p, D$gear[w], D$year[w])
#	#	pGgq = sprintf('%s|%s_%s', p, D$gear[w], D$qtr[w])
#	#	pGgs = sprintf('%s|%s_%s', p, D$gear[w], D$spp[w])
#	#	#
#	#	pGyq = sprintf('%s|%s_%s', p, D$year[w], D$qtr[w])
#	#	pGys = sprintf('%s|%s_%s', p, D$year[w], D$spp[w])
#	#	#
#	#	pGqs = sprintf('%s|%s_%s', p, D$qtr[w], D$spp[w])
#
#	#	#
#	#	eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGgy', D$gear[w], D$year[w]) ))
#	#	eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGgq', D$gear[w], D$qtr[w]) ))
#	#	eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGgs', D$gear[w], D$spp[w]) ))
#	#	#
#	#	eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGyq', D$year[w], D$qtr[w]) ))
#	#	eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGys', D$year[w], D$spp[w]) ))
#	#	#
#	#	eval(parse( text=sprintf('D$pGiven%s_%s[w]=pGqs', D$qtr[w], D$spp[w]) ))
#	##}
#}
#
##
##TRIMMED STRUCTURES
##
#
###
##D$pGivenG = rep(NA, length(D[[1]]))
##for(g in gearEff){
##        #
##        who = eval(parse( text=sprintf('who = !is.na(D$pGiven%s)', gsub("/", "_", g)) ))
##        eval(parse( text=sprintf('D$pGivenG[who] = D$pGiven%s[who]', gsub("/", "_", g)) ))
##}
###
##D$gGivenQ = rep(NA, length(D[[1]]))
##D$pGivenQ = rep(NA, length(D[[1]]))
##for(q in qtrEff){
##        #
##        who = eval(parse( text=sprintf('who = !is.na(D$pGiven%s)', gsub("/", "_", q)) ))
##        eval(parse( text=sprintf('D$pGivenQ[who] = D$pGiven%s[who]', gsub("/", "_", q)) ))
##	#
##	gwho = eval(parse( text=sprintf('gwho = !is.na(D$gGiven%s)', gsub("/", "_", q)) ))
##        eval(parse( text=sprintf('D$gGivenQ[gwho] = D$gGiven%s[gwho]', gsub("/", "_", q)) ))
##}
###
#D$qGivenY = rep(NA, end)
##D$gGivenY = rep(NA, length(D[[1]]))
##D$pGivenY = rep(NA, length(D[[1]]))
#for(y in yearEff){
#        ##
#        #who = eval(parse( text=sprintf('who = !is.na(D$pGiven%s)', y) ))
#        #eval(parse( text=sprintf('D$pGivenY[who] = D$pGiven%s[who]', y) ))
#	##
#        #gwho = eval(parse( text=sprintf('gwho = !is.na(D$gGiven%s)', y) ))
#        #eval(parse( text=sprintf('D$gGivenY[gwho] = D$gGiven%s[gwho]', y) ))
#	##
#        eval(parse( text=sprintf('qwho = !is.na(D$qGiven%s)', y) ))
#        eval(parse( text=sprintf('D$qGivenY[qwho] = D$qGiven%s[qwho]', y) ))
#}
###
##D$qGivenS = rep(NA, length(D[[1]]))
##D$gGivenS = rep(NA, length(D[[1]]))
##D$pGivenS = rep(NA, length(D[[1]]))
##for(s in sppEff){
##        #
##        who = eval(parse( text=sprintf('who = !is.na(D$pGiven%s)', s) ))
##        eval(parse( text=sprintf('D$pGivenS[who] = D$pGiven%s[who]', s) ))
##	#
##        gwho = eval(parse( text=sprintf('gwho = !is.na(D$gGiven%s)', s) ))
##        eval(parse( text=sprintf('D$gGivenS[gwho] = D$gGiven%s[who]', s) ))
##	#
##        qwho = eval(parse( text=sprintf('qwho = !is.na(D$qGiven%s)', s) ))
##        eval(parse( text=sprintf('D$qGivenS[qwho] = D$qGiven%s[qwho]', s) ))
##}
#
##
##MAKE MODEL
##
#
##Linear predictor construction may be done programmatically, by building the appropriate string.
##Simple models may simply be specified by typing the desired linear predictor string.
##f(.) indicates a heirarchical prior structure amoung the the parameters implied by the enclosed data structure 
#
###construct the 2-way port interaction terms
##portCmd = sprintf("f(pGiven%s)", gearEff[1])
##for(i in seq(2, nGear)){ portCmd = sprintf("%s + f(pGiven%s)", portCmd, gearEff[i]) }
##for(i in seq(1, nYear)){ portCmd = sprintf("%s + f(pGiven%s)", portCmd, yearEff[i]) }
##for(i in seq(1, nQtr)){  portCmd = sprintf("%s + f(pGiven%s)", portCmd, qtrEff[i]) }
##for(i in seq(1, nSpp)){  portCmd = sprintf("%s + f(pGiven%s)", portCmd, sppEff[i]) }
###3-way term names
##gY = expand.grid(gearEff, yearEff);
##gQ = expand.grid(gearEff, qtrEff);
##gS = expand.grid(gearEff, sppEff);
###
##yQ = expand.grid(yearEff, qtrEff);
##yS = expand.grid(yearEff, sppEff);
###
##qS = expand.grid(qtrEff, sppEff);
###construct the 3-way port interaction terms
##if( !not3Way ){
##	#
##	for(i in seq(1, dim(gY)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, gY[i, 1], gY[i, 2]) }
##	for(i in seq(1, dim(gQ)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, gQ[i, 1], gQ[i, 2]) }
##	for(i in seq(1, dim(gS)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, gS[i, 1], gS[i, 2]) }
##	#
##	for(i in seq(1, dim(yQ)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, yQ[i, 1], yQ[i, 2]) }
##	for(i in seq(1, dim(yS)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, yS[i, 1], yS[i, 2]) }
##	#
##	for(i in seq(1, dim(qS)[1])){ portCmd = sprintf("%s + f(pGiven%s_%s)", portCmd, qS[i, 1], qS[i, 2]) }
##}
##
##model = sprintf('weight ~ spp + port + gear + f(year) + f(qtr) + %s', portCmd)
##model = 'weight ~ spp + port + gear + f(year) + f(qtr) + f(pGivenS) + f(pGivenG) + f(pGivenY) + f(pGivenQ) + f(gGivenS) + f(gGivenQ) + f(gGivenY)'
##
#model = 'weight ~ spp + port + gear + f(year) + f(qtr) + f(qGivenY)' # + offset(log(off))'
##model = 'weight ~ spp + port + gear + f(year) + f(qtr)'
#
##
##MAKE MODELING SCRIPT
##
#
##The temp version of this file is to remain in the workspace directory.
##preProcess.r builds a version of this file for each model which is to reside within the model directory.
##The conductor file is to run the model specific version of this file within the model directory.
#
##read in tempModel.r as a string; tempModel.r acts a modeling skeleton to be filled
#fileStr = readChar('../../../tempModel.r', file.info('../../../tempModel.r')$size)
##38 \thetas is the limit for ccd integration
#intStrat = '\'eb\''
##below the double <> tags to be used in tempModel.r; they are replaced by the values specified below
#fileStr = gsub('<<type1>>', sprintf('\'%s\'', type), fileStr)
##fileStr = gsub('<<name1>>', sprintf('\'%s\'', name), fileStr)
#fileStr = gsub('<<model>>', model, fileStr)
##fiileStr = gsub('<<intStrat>>', intStrat, fileStr)
##
#writeLines(fileStr, sprintf('./%sModel.r', type))
#
##
##SAVE NECISSARY NAMES
##
#
##
#save( D,
#        list = lss,
#        file = sprintf('./%s.RData', type)
#)

