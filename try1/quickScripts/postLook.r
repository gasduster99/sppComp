rm(list=ls())

#
library(RJDBC)
library(foreach)
library(parallel)
library(doParallel)
library(KernSmooth)
library(HDInterval)
#
source('predictFunc.r')

#
#DON COMPS
#

#
mcats = c(195, 250, 253, 262, 265, 269, 270, 956, 959, 961) # c(245, 250, 253, 259, 262, 269, 270, 663, 667, 956, 959, 960, 961) #
#
minYear = 1978 # 1983 #
maxYear = 1982 # 1990 #

#driver
drv = JDBC('com.microsoft.sqlserver.jdbc.SQLServerDriver', './sqljdbc4.jar', identifier.quote="'");
#connection
ch = dbConnect(drv, 'jdbc:sqlserver://128.114.3.187;databaseName=calcom', 'nick.grunloh', 'Nmfsswfsc2017')
#call
lands = dbGetQuery(ch,
        sprintf("
        select
		source, 
		mark_cat, 
		year, 
		quarter,
		gear_grp, 
		port_complex, 
		species, 
		pounds 

        FROM [calcom].[dbo].[COM_LANDS]

        where year >= %d and year <= %d and live='N'
        ", minYear, maxYear)
)
lands = lands[lands$mark_cat%in%mcats,]
#aggregate gear to OTH=UNK+FPT; TWL=TWL+MDT; HKL; NET
lands$gear_grp[lands$gear_grp=='MDT'] = 'TWL'
lands$gear_grp[lands$gear_grp=='FPT'] = 'OTH'
lands$gear_grp[lands$gear_grp=='UNK'] = 'OTH'
#
yearEff = unique(lands$year)
qtrEff  = unique(lands$quarter)
portEff = unique(lands$port_complex)
gearEff = unique(lands$gear_grp)
#
landsTotals = aggregate(lands$pounds, by=list(lands$mark_cat, lands$year, lands$quarter, lands$gear_grp, lands$port_complex), FUN=sum)
colnames(landsTotals) = c('mcat', 'year', 'qtr', 'gear', 'port', 'weight')
#
nowComps = numeric(0) #matrix(NA, ncol=3)
#nowComps = mclapply( mcats, FUN = function(m){
for(m in mcats){
	for(y in yearEff){ #print(y)
	for(q in qtrEff ){ #print(q)
	for(g in gearEff){ #print(g)
	for(p in portEff){ #print(p)
		landsWhere = which(
			lands$mark_cat==m 	&
			lands$year==y		&
			lands$quarter==q	&
			lands$port_complex==p	&
			lands$gear_grp==g		
		)
		landsTotalsWhere = which(
			landsTotals$mcat==m 	&
			landsTotals$year==y	&
			landsTotals$qtr==q	&
			landsTotals$port==p	&
			landsTotals$gear==g
		)
		#
		#print(landsWhere)
		#print(landsTotalsWhere)
		#
		if( length(landsWhere)>0 & length(landsTotalsWhere)>0 ){
			comp = lands[landsWhere,'pounds']/landsTotals[landsTotalsWhere,'weight']	
			#gear = lands[lands$mark_cat==m,'gear_group']
			#year = lands[lands$mark_cat==m,'year']
			#port = lands[lands$mark_cat==m,'port_complex']
			#mcat = rep(m, length(comp))
			#qtr = lands[lands$mark_cat==m,'quarter']
			src = lands[landsWhere,'source']	
			spp = lands[landsWhere,'species']
			#	
			nowComps = rbind(nowComps, cbind(src, m, y, q, g, p, spp, comp))
		}
	}}}}
}
#}, mc.cores=length(mcats))
#nowComps = do.call(rbind, nowComps)
colnames(nowComps) = c('source', 'mcat', 'year', 'qtr', 'gear', 'port', 'species', 'comp')
nowComps = data.frame(nowComps, stringsAsFactors=F)
save(yearEff, qtrEff, gearEff, portEff, mcats, minYear, maxYear, nowComps, file=sprintf('%sto%sDonComps.RData', minYear, maxYear))

load('1978to1982DonComps.RData')
#load('1983to1990DonComps.RData')

#
#HAND TUNE STRATA
#

#78-82:
#unlumped gears
##195:                                                                                                       
#1: c(0.90702479338843, 0.917355371900826, 1)                                                               
#                                                                                                           
#250:                                                                                                                 
#1: c(0.123626373626374, 0.217948717948718, 0.259157509157509)                                                        
#                                                                                                                                 
#253:                                                                                                                             
#1: c(0.159332321699545, 0.2701062215478, 0.355083459787557)                                                                      
#                                                                                                                                 
#262:                                                                                                                             
#1: c(0.106299212598425, 0.248031496062992, 0.354330708661417)                                                                    
#                                                                                                                                 
#265:                                                                                                                             
#1: c(0.647577092511013, 0.823788546255507, 0.86784140969163)
#
#269:
#1: c(0.228571428571429, 0.357142857142857, 0.485714285714286)
#
#270:
#1: c(0.301204819277108, 0.487951807228916, 0.512048192771084)
#
#956:
#1: c(0.224669603524229, 0.405286343612335, 0.555066079295154)
#
#959:
#1: c(0.389473684210526, 0.747368421052632, 0.842105263157895)
#
#961:
#1: c(0.304347826086957, 0.543478260869565, 0.652173913043478)
#

#lumped gears
#195:
#1: c(0.762962962962963, 0.823703703703704, 0.908148148148148)
#
#250:
#1: c(0.133391455972101, 0.233362394652717, 0.275210694565533)
#
#253:
#1: c(0.165680473372781, 0.272189349112426, 0.353550295857988)
#
#262:
#1: c(0.10546875, 0.24609375, 0.3515625)
#
#265:
#1: c(0.647577092511013, 0.823788546255507, 0.86784140969163)
#
#269:
#1: c(0.269230769230769, 0.375, 0.461538461538462)
#
#270:
#1: c(0.303571428571429, 0.488095238095238, 0.511904761904762)
#
#956:
#1: c(0.224669603524229, 0.405286343612335, 0.555066079295154)
#
#959:
#1: c(0.389473684210526, 0.747368421052632, 0.842105263157895)
#
#961:
#1: c(0.304347826086957, 0.543478260869565, 0.652173913043478)


#83-90:
#unlumped gears
#245:
#1: c(0.339152119700748, 0.456359102244389, 0.680798004987531)
#
#250:
#1: c(0.0566455696202532, 0.140084388185654, 0.171413502109705)
#
#253:
#1: c(0.239495798319328, 0.390756302521008, 0.525210084033613)
#
#259:
#1: c(0.579505300353357, 0.706713780918728, 0.724381625441696)
#
#262:
#1: c(0.0731707317073171, 0.181818181818182, 0.266075388026608)
#
#269:
#1: c(0.270018621973929, 0.342644320297952, 0.409683426443203)
#
#270:
#1: c(0.5, 0.5, 0.5)
#
#663:
#1: c(0.407766990291262, 0.601941747572815, 0.640776699029126)
#
#667:
#1: c(0.195710455764075, 0.364611260053619, 0.463806970509383)
#
#956:
#1: c(0.0929577464788732, 0.185482123510293, 0.226435536294691)
#
#959:
#1: c(0.17631143844882, 0.324515256275364, 0.381482038178265)
#
#960:
#1: c(0.341666666666667, 0.451282051282051, 0.538461538461538)
#
#961:
#1: c(0.14412955465587, 0.240485829959514, 0.302834008097166)

##lumped gears
#245:
#1: c(0.342364532019704, 0.458128078817734, 0.679802955665025)
#
#250:
#1: c(0.0563874832180109, 0.139729422699577, 0.170814830114634)
#
#253:
#1: c(0.239495798319328, 0.390756302521008, 0.525210084033613)
#
#259:
#1: c(0.579861111111111, 0.704861111111111, 0.722222222222222)
#
#262:
#1: c(0.0764331210191083, 0.18895966029724, 0.271762208067941)
#
#269:
#1: c(0.259896729776248, 0.323580034423408, 0.387263339070568)
#
#270:
#1: c(0.5, 0.5, 0.5)
#
#663:
#1: c(0.407766990291262, 0.601941747572815, 0.640776699029126)
#
#667:
#1: c(0.2, 0.368, 0.466666666666667)
#
#956:
#1: c(0.0926517571884984, 0.18381256656017, 0.224068157614483)
#
#959:
#1: c(0.178291129868207, 0.326521545979565, 0.382644750481268)
#
#960:
#1: c(0.340575079872204, 0.449840255591054, 0.538019169329073)
#
#961:
#1: c(0.141260973663208, 0.237031125299282, 0.298483639265762)

#
adj = 1#552
#mcat = 253 
probs = c(0.68, 0.95, 0.99)
#
threads = 10 #8
registerDoParallel(cores=threads)
#
yMin = substr(minYear, 3, 4)
yMax = substr(maxYear, 3, 4)
#	
predMCAT = list()
for(mcat in mcats){
	writeLines(sprintf('%s:', mcat))
	#
	avgPath = sprintf("/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/%sto%s/MCAT%d/Top/avgModel/", yMin, yMax, mcat)
	dynPath = avgPath
	#
	ports = list.dirs(dynPath, recursive=F)
	ports = unlist(lapply(strsplit(ports, '//'), function(x){x[2]}))
	dynPath = sprintf("%s%s/", dynPath, ports[1])
	# 
	gears = list.dirs(dynPath, recursive=F);
	gears = unlist(lapply(strsplit(gears, '//'), function(x){x[2]}))
	dynPath = sprintf("%s%s/", dynPath, gears[1])
	#
	qtrs = list.dirs(dynPath, recursive=F);
	qtrs = unlist(lapply(strsplit(qtrs, '//'), function(x){x[2]}))
	dynPath = sprintf("%s%s/", dynPath, qtrs[1])
	#
	years = list.dirs(dynPath, recursive=F);
	years = unlist(lapply(strsplit(years, '//'), function(x){x[2]}))
	
	#
	sqErrs = c()
	rates = c()
	for(prob in probs){
	        preds = foreach( p=ports )%dopar%{
	        #for(p in ports){
	                #
	                end  = 1
	                pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
	                #
	                for(g in gears){
	                for(q in qtrs ){
	                for(y in years){
	                        #
	                        dp = nowComps[nowComps[,'mcat']==mcat & nowComps[,'year']==y & nowComps[,'qtr']==q & nowComps[,'gear']==g & nowComps[,'port']==p,]
	                        donSpp = dp[,'species'] #<<<< I WAS HERE
	                        dp = as.numeric(dp[,'comp'])
	                        names(dp) = donSpp
	                        #
	                        lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y))
	                        lp = lp[!is.na(lp[,1]),]
	                        #boxplot(lp, ylim=c(0, 0.3))
	                        for(s in donSpp[donSpp%in%colnames(lp)]){ #NOTE: donSpp){ some species that are not in my numbers
	                                #
	                                spIntHDI = hdi(density(lp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
	                                spIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
	                                #
	                                #spHDI
	                                inOut = 0
	                                for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(dp[s], spIntHDI[i,], rightmost.closed=T)==1 | inOut }
	                                #inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other interva
	                                #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
	                                cpHdiMean = mean(inOut)
	                                #
	                                #spCI
	                                inOut = 0
	                                for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(dp[s], spIntCI[i,], rightmost.closed=T)==1 | inOut }
	                                #inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other interval
	                                #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
	                                cpCiMean = mean(inOut)
	                                #
	                                pred[end,] = c(as.character(p), as.character(g), q, y, as.character(s), cpHdiMean, cpCiMean)
	                                end = end+1
	                        }
	                }}}
	                return(pred)
	                #preds = rbind(preds, pred)
	        }
	        #reduce preds
	        preds = do.call(rbind, preds)
	        preds$propPostHDI = as.numeric(preds$propPostHDI)
	        preds$propPostCI = as.numeric(preds$propPostCI)
	        #
	        sqErrs = c(sqErrs, (prob-mean(preds$propPostHDI))^2)
	        rates = c(rates, mean(preds$propPostHDI))
		#
		strMcat = as.character(mcat)
		if( prob==0.68 ){ 
			predMCAT[[strMcat]] = preds
			names(predMCAT[[strMcat]])[6:7] = c(sprintf('propPostHDI%s', prob*100), sprintf('propPostCI%s', prob*100))
		} else{
			predMCAT[[strMcat]] = cbind(predMCAT[[strMcat]], cbind(preds$propPostHDI, preds$propPostCI))
			names(predMCAT[[strMcat]])[(ncol(predMCAT[[strMcat]])-1):ncol(predMCAT[[strMcat]])] = c(sprintf('propPostHDI%s', prob*100), sprintf('propPostCI%s', prob*100))
		}
		print( head(predMCAT[[strMcat]]) )
	
	}
	# 
	writeLines(sprintf('%s: c(%s, %s, %s)', adj, rates[1], rates[2], rates[3]))
	writeLines('')
	##
	#g = gears[length(gears)]
	#p = ports[length(ports)]
	#q = qtrs[length(qtrs)]
	#y = years[length(years)]
	##
	#dp = nowComps[
	#	nowComps[,'mcat']==mcat & 
	#	nowComps[,'year']==y    & 
	#	nowComps[,'qtr']==q     & 
	#	nowComps[,'gear']==g    & 
	#	nowComps[,'port']==p,]
	#donSpp = dp[,'species']
	#dp = as.numeric(dp[,'comp'])
	#names(dp) = donSpp
	##
	#dp = nowComps[nowComps[,'mcat']==mcat & nowComps[,'year']==y & nowComps[,'qtr']==q & nowComps[,'gear']==g & nowComps[,'port']==p,]
	#donSpp = dp[,'species'] #<<<< I WAS HERE
	#dp = as.numeric(dp[,'comp'])
	#names(dp) = donSpp
	##
	#lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y))
	#lp = lp[!is.na(lp[,1]),]
	##
	#m = sum(donSpp%in%colnames(lp))
	#if( m>0 ){
	#	dev.new()
	#	layout(matrix(seq(1, min(4, m)), nrow=m, ncol=1))
	#	for(s in head(donSpp[donSpp%in%colnames(lp)], 4)){
	#		hist(lp[,s], freq=F, xlim=c(0, 1), main=s, xlab=mcat)
	#		lines(density(lp[,s], from=0, to=1, adjust=adj))
	#		abline(v=dp[s], col='red')
	#	}
	#}
}

#
save(predMCAT, file=sprintf('%sto%sPostLooks.RData', minYear, maxYear))

##
##AUTO TUNE STRATA
##
#
##78-82:
##	195:	25.7354213751997:
##c(1, 1, 1)
##	250:	90.4605741740214:
##c(0.763736263736264, 0.888888888888889, 0.913003663003663)
##	253:	552.446757838051:
##NOTE:c(0.842185128983308, 0.893778452200303, 0.893778452200303)
##	262:	474.723559474155:
##NOTE:c(0.925196850393701, 0.94488188976378, 0.968503937007874)
##	265:	494.084135677841:
##c(0.6784140969163, 0.955947136563877, 1)
##	269:	26.7392001252358:
##c(0.742857142857143, 0.914285714285714, 0.942857142857143)
##	270:	143.41214333129:
##c(0.897590361445783, 0.933734939759036, 0.987951807228916)
##	956:	550.400375834313:
##NOTE:c(0.806167400881057, 0.863436123348018, 0.876651982378855)
##	959:	645.950609349151:
##c(0.494736842105263, 0.757894736842105, 0.894736842105263)
##	961:	211.93300620584:
##c(0.630434782608696, 0.891304347826087, 1)
#
##
##83-90:
##	245:	480.645936256886:
##c(0.610972568578554, 0.952618453865337, 0.99002493765586) 
##	250: 	149.173461830064:
##c(0.755274261603376, 0.858649789029536, 0.886286919831224)
##	253:	111.411302674679:
##c(0.852941176470588, 0.903361344537815, 0.911764705882353)
##	259:	19.3071796403047:
##c(0.696113074204947, 0.932862190812721, 0.954063604240283)
##	262:	114.278051835428:
##c(0.771618625277162, 0.911308203991131, 0.953436807095344)
##	269:	87.2936357296876:
##c(0.787709497206704, 0.878957169459963, 0.905027932960894)
##	270:	25.7354213751997:
##c(0.5, 1, 1)
##	663:	25.3657336174068:
##c(0.679611650485437, 0.961165048543689, 0.980582524271845)
##	667:	41.3561643141262:
##c(0.667560321715818, 0.900804289544236, 0.906166219839142)
##	956:	102.434264346351:
##c(0.741711809317443, 0.881906825568797, 0.909425785482123)
##	959:	1089.76024908955:
##NOTE:c(0.734405531339245, 0.815271306177664, 0.874643018187284)
##	960:	760.431652502663:
##NOTE:c(0.68525641025641, 0.783333333333333, 0.847435897435897)
##	961:	701.25972263191:
##NOTE:c(0.784615384615385, 0.887449392712551, 0.937651821862348)
#
#
##
#threads = 8
#search = c(-700, 1200) #c(-1225, 2000) #c(0, 1000)
##
#yMin = substr(minYear, 3, 4)
#yMax = substr(maxYear, 3, 4)
##mcat = mcats[1]
#outs = list()
#for(mcat in mcats){
#	#
#	avgPath = sprintf("/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/%sto%s/MCAT%d/Top/avgModel/", yMin, yMax, mcat)
#	dynPath = avgPath
#	#
#	ports = list.dirs(dynPath, recursive=F)
#	ports = unlist(lapply(strsplit(ports, '//'), function(x){x[2]}))
#	dynPath = sprintf("%s%s/", dynPath, ports[1])
#	# 
#	gears = list.dirs(dynPath, recursive=F);
#	gears = unlist(lapply(strsplit(gears, '//'), function(x){x[2]}))
#	dynPath = sprintf("%s%s/", dynPath, gears[1])
#	#
#	qtrs = list.dirs(dynPath, recursive=F);
#	qtrs = unlist(lapply(strsplit(qtrs, '//'), function(x){x[2]}))
#	dynPath = sprintf("%s%s/", dynPath, qtrs[1])
#	#
#	years = list.dirs(dynPath, recursive=F);
#	years = unlist(lapply(strsplit(years, '//'), function(x){x[2]}))
#	
#	#
#	#EVALUATE
#	#
#	
#	#
#	#out = optim(100, opt)
#	writeLines( sprintf('%s:', mcat) )
#	outs[[mcat]] = optimize(postOpt, search, tol=1, mcat=mcat)
#	#if( out$converge==0 ){ outs[[mcat]]=out }
#}
#
##
#save.image(sprintf('%sto%sOut.RData', yMin, yMax))





































##
#adj = 166 #4.1/12
#prob = 0.99
#threads = 8
##
#preds = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
#registerDoParallel(cores=threads)
#preds = foreach( p=ports )%dopar%{
##for(p in ports){
#	#
#        end  = 1
#        pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
#        #
#	for(g in gears){
#	for(q in qtrs ){
#	for(y in years){
#		#
#		dp = nowComps[nowComps[,'mcat']==mcat & nowComps[,'year']==y & nowComps[,'qtr']==q & nowComps[,'gear']==g & nowComps[,'port']==p,]
#		donSpp = dp[,'species'] #<<<< I WAS HERE
#		dp = as.numeric(dp[,'comp'])
#		names(dp) = donSpp
#		#
#		lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y))
#		lp = lp[!is.na(lp[,1]),]
#		#boxplot(lp, ylim=c(0, 0.3))
#		for(s in donSpp[donSpp%in%colnames(lp)]){ #NOTE: donSpp){ some species that are not in my numbers
#			#
#			spIntHDI = hdi(density(lp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
#                        spIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#			#
#			#spHDI
#	                inOut = 0
#			for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(dp[s], spIntHDI[i,], rightmost.closed=T)==1 | inOut }         
#			#inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other interva
#                        #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
#                        cpHdiMean = mean(inOut)
#                        #
#                        #spCI
#                        inOut = 0
#			for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(dp[s], spIntCI[i,], rightmost.closed=T)==1 | inOut }
#			#inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other interval
#                        #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
#                        cpCiMean = mean(inOut)
#			#
#			pred[end,] = c(as.character(p), as.character(g), q, y, as.character(s), cpHdiMean, cpCiMean)
#			end = end+1
#		}
#	}}}
#	return(pred)
#	#preds = rbind(preds, pred)
#}
##reduce preds
#preds = do.call(rbind, preds)
#preds$propPostHDI = as.numeric(preds$propPostHDI)
#preds$propPostCI = as.numeric(preds$propPostCI)
##
#print(mean(preds$propPostHDI))

##
#opt = function(adj, mcat){
#	#
#	probs = c(0.68, 0.95, 0.99)
#	registerDoParallel(cores=threads)
#	#
#	sqErrs = c()
#	rates = c()
#	for(prob in probs){
#		preds = foreach( p=ports )%dopar%{
#		#for(p in ports){
#			#
#		        end  = 1
#		        pred = data.frame(port=character(), gear=character(), qtr=integer(), year=integer(), spp=character(), propPostHDI=numeric(), propPostCI=numeric(), stringsAsFactors=F)
#		        #
#			for(g in gears){
#			for(q in qtrs ){
#			for(y in years){
#				#
#				dp = nowComps[nowComps[,'mcat']==mcat & nowComps[,'year']==y & nowComps[,'qtr']==q & nowComps[,'gear']==g & nowComps[,'port']==p,]
#				donSpp = dp[,'species'] #<<<< I WAS HERE
#				dp = as.numeric(dp[,'comp'])
#				names(dp) = donSpp
#				#
#				lp = read.csv(sprintf('%s/%s/%s/%s/%s/lpPost.csv', avgPath, p, g, q, y))
#				lp = lp[!is.na(lp[,1]),]
#				#boxplot(lp, ylim=c(0, 0.3))
#				for(s in donSpp[donSpp%in%colnames(lp)]){ #NOTE: donSpp){ some species that are not in my numbers
#					#
#					spIntHDI = hdi(density(lp[,s], from=0, to=1, adjust=adj), credMass=prob, allowSplit=T)
#		                        spIntCI = t(quantile(lp[,s], c((1-prob)/2, prob+(1-prob)/2)))
#					#
#					#spHDI
#			                inOut = 0
#					for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(dp[s], spIntHDI[i,], rightmost.closed=T)==1 | inOut }         
#					#inOut = rep(0, length(cp))    #check if in interval, convert to proper bool, and unite with other interva
#		                        #for(i in 1:dim(spIntHDI)[1]){ inOut=findInterval(cp, spIntHDI[i,])==1 | inOut }
#		                        cpHdiMean = mean(inOut)
#		                        #
#		                        #spCI
#		                        inOut = 0
#					for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(dp[s], spIntCI[i,], rightmost.closed=T)==1 | inOut }
#					#inOut = rep(0, length(cp))   #check if in interval, convert to proper bool, and unite with other interval
#		                        #for(i in 1:dim(spIntCI)[1]){ inOut=findInterval(cp, spIntCI[i,])==1 | inOut }
#		                        cpCiMean = mean(inOut)
#					#
#					pred[end,] = c(as.character(p), as.character(g), q, y, as.character(s), cpHdiMean, cpCiMean)
#					end = end+1
#				}
#			}}}
#			return(pred)
#			#preds = rbind(preds, pred)
#		}
#		#reduce preds
#		preds = do.call(rbind, preds)
#		preds$propPostHDI = as.numeric(preds$propPostHDI)
#		preds$propPostCI = as.numeric(preds$propPostCI)
#		#
#		sqErrs = c(sqErrs, (prob-mean(preds$propPostHDI))^2)
#		rates = c(rates, mean(preds$propPostHDI))
#	}
#	#
#	writeLines(sprintf('%s:', adj))
#	print(rates)
#	writeLines('')
#	#
#	return( mean(sqErrs) )
#}













