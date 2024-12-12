rm(list=ls())

#NOTE: Create an R package that depends on another R package located on GitHub
#https://stackoverflow.com/questions/30493388/create-an-r-package-that-depends-on-another-r-package-located-on-github
#https://github.com/r-lib/devtools/pull/902
library(INLA)
library(brms)
library(rstan)
#
source('dataFunk.r')
source('predFunk.r')

#
#KNOBS
#

#
mcat = 959 #250 
minYear = 1983 #1978 #1991 #
maxYear = 1990 #1982 #2001 #

##gold standards for defining strata
#portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
portGold = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO'); regionID="N"
##NOTE: no OLA observations in early time period
#split ports and remove OLA from south
#portGold = c('OSB', 'OLA', 'OSD'); regionID="S"
yearGold = minYear:maxYear
qtrGold  = 1:4
gearGold = c('HKL', 'TWL', 'NET') #c('HKL', 'TWL', 'FPT', 'NET', 'MDT')
#
regionID = sprintf("%s%s", mcat, regionID)

#
#DATA
#

#
#dataFile = sprintf('%sdata%sTo%s_2018-06-08.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4))
raw = getRawData(mcat, minYear, maxYear, save=T, fromFile=T) #, fromFile="250CALCOMdata78To82_2024-10-28.csv") #"250data78To83_2024-11-13.csv") #T)
raw = raw[raw$portComplex%in%portGold,]

#define sppGold from the data
spp  = unique(raw$species[raw$portComplex%in%portGold])
#sppGold = names(sort(table(raw$species[raw$weight>0]))[floor(length(spp)*3/4):length(spp)])
aSpp = read.csv('assSppCurrent.csv') #read.csv('assSppMore.csv') #read.csv('assSpp.csv')
flatSpp = c()
sppKey = list()
for(i in 1:nrow(aSpp)){
	#
	extraSpp = unlist(aSpp[i,6:ncol(aSpp)])
	extraSpp = extraSpp[extraSpp!=""]
	#
	sppKey[[aSpp[i,1]]] = c(aSpp[i,1], extraSpp)
	flatSpp = c(flatSpp, aSpp[i,1], extraSpp)
}
sppKey[["OTHR"]] = spp[!spp%in%flatSpp]
#invert key to rename species based on sppKey definition
keySpp = with(stack(sppKey), split(as.character(ind), values))
raw$species = unlist(sapply(raw$species, function(x)keySpp[[x]]))
#
sppGold = unique(raw$species) 

#
D = makeD(sppGold, portGold, gearGold, yearGold, qtrGold, raw)
#interactions
D$YQ = as.character(interaction(D$year, D$qtr))

##add predictive structure 
#DPred = addPredStrat(sppGold, portGold, gearGold, yearGold, qtrGold, D)
##interactions
#DPred$YQ = as.character(interaction(DPred$year, DPred$qtr))
#DPred$SP = as.character(interaction(DPred$species, DPred$port))
#DPred$SG = as.character(interaction(DPred$species, DPred$gear))

##
#D$weight = rbeta_binomial(nrow(D), D$nBB, 0.25, 1)

#
#MODEL
#

#
refit="on_change" # "always"
cores = parallel::detectCores()-1
MM = 10000 
thin = 10
warmFrac = seq(0, 1, 0.05)[1+2] #1/3
modID = sprintf("%s%sto%sSPGY:Q_RDBD_SPKL", regionID, minYear, maxYear)
#
source('model.r')

##
##PREDICT
##
#
###
###https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/#posterior-predictions
###MM x #(pred strata)
##predSam = posterior_predict(brmsOut, newdata=predD, allow_new_levels=T, sample_new_levels="uncertainty")# "gaussian")
###
##postSam = as_draws_df(brmsOut)
###
#sppComps = sppCompPred(MM, brmsOut, sppGold, portGold, gearGold, yearGold, qtrGold, tree=T, place=modPath, cores=cores)
#saveRDS(sppComps, file=sprintf("%s/sppComp%s.rds", modPath, modID))


#
#EXPORT
#

#

#
#
#

#shinystan::launch_shinystan(fit_MC)





###model
##modelDef = weight~species+gear+port+f(YQ)
#
##
#inlaOut = inla(weight~-1+species+gear+port+f(YQ), 
#		family = "betabinomial", 
#		data   = D, #Pred, 
#		num.threads = cores, 
#		Ntrials = D$nBB, #DPred$nBB,
#		#control.inla = list( tolerance=1e-8 ),
#		#control.predictor=list( link=inla.link.logit ),
#                #control.inla=list(
#                #        int.strategy='ccd'#, #eb',
#                #        #tolerance=1e-10,
#                #        #h=1e-10 #,
#                #        #lincomb.derived.only=F lincomb.derived.correlation.matrix
#                #),
#                control.compute=list(
#                        config=T,
#                        mlik=T,
#                        dic=T,
#                        waic=T,
#			return.marginals.predictor=T
#                ),
#                verbose = F
#                #control.fixed = list(
#                #        expand.factor.strategy='inla'
#                #),
#                #control.mode = list(
#                #        result=starter,
#                #        restart=T
#                #)
#)
#
##
##inla.mmarginal(inlaOut$marginals.fixed$portMRO)
#
###
###fixed = summary(inlaOut)$fixed[,'mean']
##
##hyper = summary(inlaOut)$hyperpar[,'mean']
##names(hyper) = rownames(summary(inlaOut)$hyperpar)
##
#
###
##fixed = inlaOut$summary.fixed[,'mode']
##names(fixed) = rownames(inlaOut$summary.fixed)
###
##random = inlaOut$summary.random$YQ[,'mode']
##names(random) = inlaOut$summary.random$YQ$ID
###
##hyper = inlaOut$summary.hyperpar[,'mean']
##names(hyper) = rownames(summary(inlaOut)$hyperpar)
##
#
##
#modes = function(inlaFit){
#	#
#	out = c()
#	
#	#
#	fixed = inlaOut$summary.fixed[,'mode']
#	names(fixed) = rownames(inlaOut$summary.fixed)
#	#
#	randoNames = names(inlaOut$marginals.random)
#	rOut = c()
#	for(rn in randoNames){
#		#
#		o = inlaOut$summary.random[[rn]][,'mode']
#		names(o) = inlaOut$summary.random$YQ$ID
#		rOut = c(rOut, o)
#	}
#	#random = inlaOut$summary.random$YQ[,'mode']
#	#names(random) = inlaOut$summary.random$YQ$ID
#	#
#	hyper = inlaOut$summary.hyperpar[,'mean']
#	names(hyper) = rownames(summary(inlaOut)$hyperpar)
#
#	#
#	out = c(fixed, rOut, hyper)
#	return(out)
#}
#
###https://discourse.mc-stan.org/t/using-rstan-optimizing-with-brmsfit-object/36180
##code = stancode(weight|trials(nBB)~species+gear+port+1|YQ, data=DPred, family=beta_binomial())
##model = stan_model(model_code=code)
##data = make_standata(weight|trials(nBB)~species+gear+port+1|YQ, data=DPred, family=beta_binomial())
##opt = optimizing(model, data=data)
#
##
#ms = modes(inlaOut)
##names(ms)[1] = 'Intercept'
##
#names(ms)[grepl("gear*", names(ms))] = gsub("gear", "b_gear", names(ms)[grepl("gear*", names(ms))])
#names(ms)[grepl("port*", names(ms))] = gsub("port", "b_port", names(ms)[grepl("port*", names(ms))])
#names(ms)[grepl("species*", names(ms))] = gsub("species", "b_species", names(ms)[grepl("species*", names(ms))])
##gsub("\\.", "_", names(ms[grepl(glob2rx("*.*"), names(ms))]))
#names(ms)[grepl(glob2rx("*.*"), names(ms))] = sprintf("r_YQ[%s,Intercept]", names(ms[grepl(glob2rx("*.*"), names(ms))]))
#names(ms[(length(ms)-1):length(ms)]) = c("phi", "sd_YQ__Intercept")
##
#ms["phi"] = ms["overdispersion for the betabinomial observations"]
#ms["sd_YQ__Intercept"] = sqrt(1/ms["Precision for YQ"])
#ms = ms[-c(length(ms)-2, length(ms)-3)] 
##
#ms['zi'] = 0
#
##
#brmsOut = brm(
#        bf(
#		weight|trials(nBB)~ -1 + species + gear + port + (1|YQ),
#        	zi = 0 
#	),
#        data   = D, #Pred,
#        family = zero_inflated_beta_binomial(), 
#	#family = beta_binomial(),
#	#family = negbinomial(),
#        #family = zero_inflated_negbinomial(),
#        #family = zero_inflated_poisson(),
#        chains = cores,
#        cores  = cores,
#	#init = 0,
#	init = ms, 
#        #silent = 0,
#        file   = "test", #id,
#        iter   = cores * 100 #600
#)




#
#for later
#

##
#sam=as_draws_df(brmsOut)

##
#prop_zero <- function(y) mean(y == 0)
#prop_zero_test1 = pp_check(out, type="stat", stat="prop_zero")
#prop_zero_test2 = pp_check(out, type="stat_grouped", stat="prop_zero", group="year")
##
#png(sprintf("p0All%s.png", id)) #, width=960/18*(yHi-yLo), height=400*4)
#plot(prop_zero_test1)
#dev.off()
##
#png(sprintf("p0Year%s.png", id)) #, width=960/18*(yHi-yLo), height=400*4)
#plot( prop_zero_test2+coord_cartesian(xlim=c(0.6, 1)) ) #xlim(0.25, 0.75) #+ coord_cartesian(xlim = c(0, 1))
#dev.off()
#
##
#mTest1 = pp_check(out, type="stat", stat="mean")
#mTest2 = pp_check(out, type="stat_grouped", stat="mean", group="year")
##
#png(sprintf("mmAll%s.png", id)) #, width=960/18*(yHi-yLo), height=400*4)
#plot(mTest1+coord_cartesian(xlim=c(0, 2)))
#dev.off()
##
#png(sprintf("mmYear%s.png", id)) #, width=960/18*(yHi-yLo), height=400*4)
#plot( mTest2+coord_cartesian(xlim=c(0, 2)) ) #+ xlim(2, 6) #+ coord_cartesian(xlim = c(2, 6))
#dev.off()
#
##
#sTest1 = pp_check(out, type="stat", stat="sd")
#sTest2 = pp_check(out, type="stat_grouped", stat="sd", group="year")
##
#png(sprintf("sdAll%s.png", id)) #, width=960/18*(yHi-yLo), height=400*4)
#plot(sTest1+coord_cartesian(xlim=c(0, 10)))
#dev.off()
##
#png(sprintf("sdYear%s.png", id)) #, width=960/18*(yHi-yLo), height=400*4)
#plot( sTest2+coord_cartesian(xlim=c(0, 6)) ) #xlim(6, 18) #+ coord_cartesian(xlim = c(6, 15))
#dev.off()


#shinystan::launch_shinystan(fit_MC)




###
##sc = stancode( weight|trials(nBB)~species+gear+port+1|YQ, family=beta_binomial(), data=DPred )
#
##
##EXPORT
##
#
##sam = sampler(out, portGold, gearGold, qtrGold, yearGold, D, M=10^4, cores=8, samplePath='./')
#
##dataFile = sprintf('%sdata%sTo%s_2018-06-08.csv', mcat, substring(minYear, 3, 4), substring(maxYear, 3, 4))
##samplePath = sprintf('./%s%s%s/', mcat, minYear, maxYear)




#
#JUNK
#



##
#sample = function(M, brmsOut, sppG, portG, gearG, yearG, qtrG, tree=F, place="./", cores=1){
#	#value: nameD, predSppComp
#	#Nums Scale: posterior_predict( brmsOut, newdata=predD, allow_new_levels=T, sample_new_levels=c("uncertainty","gaussian")[1] )
#	#Post Scale: as_draws_df(brmsOut)
#
#	#
#	stopifnot(M<summary(brmsOut)$total_ndraws)
#		
#	#
#	nameDF = expand.grid(species=sppG, port=portG, gear=gearG, year=yearG, qtr=qtrG)
#	predStrat = expand.grid(port=portG, gear=gearG, year=yearG, qtr=qtrG)
#	
#	#predSppComp = c()
#	#for(i in 1:nrow(predStrat)){
#	cl = makePSOCKcluster(cores)
#	registerDoParallel( cl )
#	predSppComp = foreach(i=1:nrow(predStrat), .combine='cbind', .inorder=T)%dopar%{ #.export=c("brmsOut")
#		#
#		p = predStrat[i,"port"]
#		g = predStrat[i,"gear"]
#		y = predStrat[i,"year"]
#		q = predStrat[i,"qtr"]
#		#
#		predD = data.frame(species=sppG, port=p, gear=g, year=y, qtr=q)
#		predD$YQ = as.character(interaction(predD[,'year'], predD[,'qtr']))
#		predD$nBB = 100
#		
#		#
#		samBB = brms::posterior_predict(brmsOut, newdata=predD, allow_new_levels=T, sample_new_levels="uncertainty")# "gaussian")
#		#
#		sums = rowSums(samBB)
#		ss   = sums==0
#		samP = samBB[!ss,]/sums[!ss]
#		
#		#make sure we have exactly MM samples for the database
#		samP = head(rbind(samP, samP[sample.int(nrow(samP), replace=T),]), M)
#	
#		#NOTE:maybe reform the predSam shape or write the stratum tree with sppComp.csv leaves.
#				
#		#write the stratum tree with sppComp.csv leaves.
#		if( tree ){
#			#
#			suppressWarnings({
#				#NOTE: figure out conventions around working directory
#				path = sprintf('%s/', place); dir.create(path);
#				path = sprintf('%s/%s/', place, p); dir.create(path);
#				path = sprintf('%s/%s/%s/', place, p, g); dir.create(path);
#	                	path = sprintf('%s/%s/%s/%s/', place, p, g, q); dir.create(path);
#	                	path = sprintf('%s/%s/%s/%s/%s/', place, p, g, q, y); dir.create(path);
#			})
#			#
#			colnames(samP) = sppG
#			write.table(format(samP, scientific=T, digits=6), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=T, quote=F, sep=',')
#		}
#		
#		#
#		#reform the predSam shape (in parrallel the order of predSppComp will not match predSam)
#		#predSppComp = cbind(predSppComp, samP)
#		return(samP)
#	}
#	stopCluster(cl)
#	
#	#
#	return(list(nameDF=nameDF, predSppComp=predSppComp))
#}






		#samBB = predSam[,
		#	predD$port==p &
		#	predD$gear==g &
		#	predD$year==y &
		#	predD$qtr==q 
		#]

##nPred = length(sppGold)*length(portGold)*length(gearGold)*length(yearGold)*length(qtrGold)
	#nameDF = expand.grid(species=sppG, port=portG, gear=gearG, year=yearG, qtr=qtrG)
	#predD$YQ = as.character(interaction(predD$year, predD$qtr))
	#predD$nBB = 100
	#
	##https://www.andrewheiss.com/blog/2021/11/10/ame-bayes-re-guide/#posterior-predictions
	#predSam = posterior_predict(brmsOut, newdata=predD, allow_new_levels=T, sample_new_levels="uncertainty")# "gaussian")
	##MM x #(pred strata)
	##postSam = as_draws_df(brmsOut)
	



#NOTE: define MM better
	#idFill = sample(which(!ss), MM-sum(!ss), replace=T)
	#samP = rbind(samP, samBB[idFill,]/rowSums(samBB[idFill,]))

	##sppT is BB counts
	##transform sppComps
	#sums = rowSums(sppT)
	#ss   = sums==0
	#sppP = sppT[!ss,]/sums[!ss]


##
#predD = cbind.data.frame(
#	species = as.character( rep(sppGold, nPred/length(sppGold))   ),
#	port	= as.character( rep(portGold, nPred/length(portGold)) ),
#	gear	= as.character( rep(gearGold, nPred/length(gearGold)) ),
#	year 	= as.character( rep(yearGold, nPred/length(yearGold)) ),
#	qtr	= as.character( rep(qtrGold, nPred/length(qtrGold))   ),
#	nBB	= 100
#	#
#	#species = factor(rep(sppGold, nPred/length(sppGold)),	levels=levels(sppGold)),
#	#port	= factor(rep(portGold, nPred/length(portGold)),	levels=levels(portGold)),
#	#gear	= factor(rep(gearGold, nPred/length(gearGold)),	levels=levels(gearGold)),
#	#year 	= factor(rep(yearGold, nPred/length(yearGold)),	levels=levels(yearGold)),
#	#qtr	= factor(rep(qtrGold, nPred/length(qtrGold)),	levels=levels(qtrGold))
#        #
#	#year            = factor(rep(yearsN, nDistN),levels=levels(as.factor(DN$year))),
#        #district        = factor(rep(districtsN, rep(nYearsN,nDistN)), levels=levels(DN$district)),
#        ##NOTE: due to the district:wave interaction each wave will have a slightly different shift for each district (May_J
#        #month           = factor(rep("7", nYearsN*nDistN),levels=levels(as.factor(D$month))), #factor(rep("Sep_Oct",52),levels=level
#        ##prim1Common     = factor(rep("rockfish genus",52), levels=levels(D$prim1Common)),
#        #depth_bin       = factor(rep(depth_bins[nDepth], nYearsN*nDistN), levels=levels(D$depth_bin)),
#        #logEffort       = rep(0, nYearsN*nDistN) #median(dat$logEffort[dat$prim1Common=="rockfish genus" & dat$wave=="July_Aug"]),52
#)
#DPred$YQ = as.character(interaction(DPred$year, DPred$qtr))
#predD$YQ = as.character(interaction(predD$year, predD$qtr))



##
#brmsOut = brm(
#        bf(
#		weight|trials(nBB)~ -1 + species + gear + port + (1|YQ), #-1 + gear + (1|YQ)  #
#        	zi = 0 
#	),
#        #prior = c(
#        #        #prior(normal(0, 2), class="b"),
#        #        #prior(normal(0, 4), class="Intercept"),
#        #        ##prior(normal(0, 2), dpar="shape"),
#        #        #prior(normal(0, 2), dpar="zi"), #class="zi")
#        #        #prior(gamma(0.01, 0.01), class="shape"),
#        #        #prior(cauchy(0, 1), class="sd")
#        #        ##prior(beta(2, 2), class="zi")
#        #),
#        data   = D, #Pred,
#        family = zero_inflated_beta_binomial(), #beta_binomial(),
#	#family = negbinomial(),
#        #family = zero_inflated_negbinomial(),
#        #family = zero_inflated_poisson(),
#        chains = cores,
#        cores  = cores,
#	#init = 0,
#	init = ms, #0, 
#        #silent = 0,
#        #file   = id,
#        iter   = cores*100
#)




#ms["Precision for YQ"]
#names(ms[(length(ms)-2):length(ms)]) = c("shape", "sd_YQ__Intercept")
#names(ms[c("overdispersion for the betabinomial observations", "Precision for YQ")]) = c("shape", "sd_YQ__Intercept")
#names(ms["Precision for YQ"]) = "sd_YQ__Intercept"
#ms[grepl(glob2rx("*.*"), names(ms))]
#ms = ms[1:(length(ms)-2)] 

#D = head(D, 1000)


	

	##
	#nameFixed = names(inlaFit$marginals.fixed)
	#for(n in nameFixed){
	#	out = c(out, inla.mmarginal(inlaFit$marginals.fixed[[n]]))
	#}
	#names(out) = nameFixed
	##nameRando = names(inlaOut$marginals.random$YQ)
	##nameHyper = names(inlaOut$marginals.hyperpar)	
	
	##
	#return(out)
