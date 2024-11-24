#
#DEPENDENCIES
#

#
suppressMessages(library(INLA, quietly=FALSE))
suppressMessages(library(brms, quietly=FALSE))
suppressMessages(library(rstan, quietly=FALSE))

#
#VARIABLES
#

#
if( !exists("refit") ){ refit="on_change" }
if( !exists("cores") ){ cores=parallel::detectCores()-1 }
if( !exists("modID") ){ modID=sprintf("test%s", format(Sys.time(), "%Y%m%d")) }
if( !exists("warmFrac") ){ warmFrac=0.5 }
if( !exists("thin") ){ thin=1 }
if( !exists("MM") ){ MM=10000 }

#make working directory. copy code for documentation. write paths as absolute with getwd().
#In dir: data, code, tree, *.rds files
modPath = file.path(getwd(), modID)
if( !dir.exists(modPath) ){ dir.create(modPath); }
file.copy(c("./dataFunk.r", "./predFunk.r", "./model.r", "./main.r"), modPath)
save.image(sprintf("%s/data%s.RData", modPath, modID))

#
#INLA STAGE
#

#if( !file.exists(modelID) ){

#
writeLines("INLA stage...\n")
inlaOut = inla(weight ~ -1 + species + port + gear + f(YQ), 
		family = "betabinomial", 
		data   = D, #Pred, 
		num.threads = cores, 
		Ntrials = D$nBB, #DPred$nBB,
		#control.inla = list( tolerance=1e-8 ),
		#control.predictor=list( link=inla.link.logit ),
                #control.inla=list(
                #        int.strategy='ccd'#, #eb',
                #        #tolerance=1e-10,
                #        #h=1e-10 #,
                #        #lincomb.derived.only=F lincomb.derived.correlation.matrix
                #),
                control.compute=list(
                        config=T,
                        mlik=T,
                        dic=T,
                        waic=T,
			return.marginals.predictor=T
                ),
                verbose = F
                #control.fixed = list(
                #        expand.factor.strategy='inla'
                #),
                #control.mode = list(
                #        result=starter,
                #        restart=T
                #)
)
saveRDS(inlaOut, file=sprintf("%s/inla%s.rds", modPath, modID))

#
#EXTRACT INLA MODES
#

#
modes = function(inlaFit){
	#
	out = c()
	
	#
	fixed = inlaOut$summary.fixed[,'mode']
	names(fixed) = rownames(inlaOut$summary.fixed)
	#
	randoNames = names(inlaOut$marginals.random)
	rOut = c()
	for(rn in randoNames){
		#
		o = inlaOut$summary.random[[rn]][,'mode']
		names(o) = inlaOut$summary.random$YQ$ID
		rOut = c(rOut, o)
	}
	#random = inlaOut$summary.random$YQ[,'mode']
	#names(random) = inlaOut$summary.random$YQ$ID
	#
	hyper = inlaOut$summary.hyperpar[,'mean']
	names(hyper) = rownames(summary(inlaOut)$hyperpar)

	#
	out = c(fixed, rOut, hyper)
	return(out)
}
writeLines('')

#
ms = modes(inlaOut)
#names(ms)[1] = 'Intercept'
#
names(ms)[grepl("gear*", names(ms))] = gsub("gear", "b_gear", names(ms)[grepl("gear*", names(ms))])
names(ms)[grepl("port*", names(ms))] = gsub("port", "b_port", names(ms)[grepl("port*", names(ms))])
names(ms)[grepl("species*", names(ms))] = gsub("species", "b_species", names(ms)[grepl("species*", names(ms))])
#gsub("\\.", "_", names(ms[grepl(glob2rx("*.*"), names(ms))]))
names(ms)[grepl(glob2rx("*.*"), names(ms))] = sprintf("r_YQ[%s,Intercept]", names(ms[grepl(glob2rx("*.*"), names(ms))]))
names(ms[(length(ms)-1):length(ms)]) = c("phi", "sd_YQ__Intercept")
#
ms["phi"] = ms["overdispersion for the betabinomial observations"]
ms["sd_YQ__Intercept"] = sqrt(1/ms["Precision for YQ"])
ms = ms[-c(length(ms)-2, length(ms)-3)] 
#
ms['zi'] = 0

#
#BRMS STAGE
#

#
writeLines("BRMS:")

#NOTE: maybe move this above the inla model somehow
brmsMod = sprintf("%s/brms%s.rds", modPath, modID)
if( file.exists(brmsMod) ){
	#| brmsOut$init!=ms 
	s = summary(readRDS( brmsMod ))
	if( s$thin!=thin | s$warmup!=floor(warmFrac*thin*MM/cores) | s$iter!=ceiling( thin*MM/cores + floor(warmFrac*thin*MM/cores) ) ){ refit="always" }
}

#
brmsOut = brm(
        bf(
		weight|trials(nBB)~ -1 + species + port + gear + (1|YQ),
        	zi = 0 
	),
        data   = D, #Pred,
        family = zero_inflated_beta_binomial(),
        #family = zero_inflated_negbinomial(),
        #family = zero_inflated_poisson(),
        chains = cores,
        cores  = cores,
	file_refit = refit, #"always", #"on_change", #getOption("brms.file_refit", "never"),
	#init = 0,
	init = ms, 
        #silent = 0,
        file = brmsMod, #sprintf("%s/brms%s.rds", modPath, modID),
	thin = thin,
	warmup = floor(warmFrac*thin*MM/cores), #floor(iter*warmFrac),
        iter = ceiling( thin*MM/cores + floor(warmFrac*thin*MM/cores) ) #(thin+1)*MM/cores #ceiling((thin+1)*MM/cores) #600
)

writeLines('')









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


##https://discourse.mc-stan.org/t/using-rstan-optimizing-with-brmsfit-object/36180
#code = stancode(weight|trials(nBB)~species+gear+port+1|YQ, data=DPred, family=beta_binomial())
#model = stan_model(model_code=code)
#data = make_standata(weight|trials(nBB)~species+gear+port+1|YQ, data=DPred, family=beta_binomial())
#opt = optimizing(model, data=data)



#
#inla.mmarginal(inlaOut$marginals.fixed$portMRO)

##
##fixed = summary(inlaOut)$fixed[,'mean']
#
#hyper = summary(inlaOut)$hyperpar[,'mean']
#names(hyper) = rownames(summary(inlaOut)$hyperpar)
#

##
#fixed = inlaOut$summary.fixed[,'mode']
#names(fixed) = rownames(inlaOut$summary.fixed)
##
#random = inlaOut$summary.random$YQ[,'mode']
#names(random) = inlaOut$summary.random$YQ$ID
##
#hyper = inlaOut$summary.hyperpar[,'mean']
#names(hyper) = rownames(summary(inlaOut)$hyperpar)
#



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
