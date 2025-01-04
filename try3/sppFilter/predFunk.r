#
#DEPENDENCIES
#

#
suppressMessages(library(brms, quietly=FALSE))
suppressMessages(library(foreach, quietly=FALSE))
suppressMessages(library(doParallel, quietly=FALSE))

#
sppCompPred = function(M, brmsOut, sppG, portG, gearG, yearG, qtrG, tree=F, place="./", cores=1){
        #value: nameD, predSppComp
        #Nums Scale: posterior_predict( brmsOut, newdata=predD, allow_new_levels=T, sample_new_levels=c("uncertainty","gaussian")[1] )
        #Post Scale: as_draws_df(brmsOut)
	writeLines('sppCompPred...\n')

        #
        stopifnot(M<summary(brmsOut)$total_ndraws)

        #
        nameDF = expand.grid(species=sppG, port=portG, gear=gearG, year=yearG, qtr=qtrG)
        predStrat = expand.grid(port=portG, gear=gearG, year=yearG, qtr=qtrG)

        #predSppComp = c()
        #for(i in 1:nrow(predStrat)){
        cl = makePSOCKcluster(cores)
        registerDoParallel( cl )
        predSppComp = foreach(i=1:nrow(predStrat), .combine='cbind', .inorder=T)%dopar%{ #.export=c("brmsOut")
                #
                p = predStrat[i,"port"]
                g = predStrat[i,"gear"]
                y = predStrat[i,"year"]
                q = predStrat[i,"qtr"]
                #
                predD = data.frame(species=sppG, port=p, gear=g, year=y, qtr=q)
                predD$YQ = as.character(interaction(predD[,'year'], predD[,'qtr']))
                predD$nBB = 100

                #
                samBB = brms::posterior_predict(brmsOut, newdata=predD, allow_new_levels=T, sample_new_levels="uncertainty")# "gaussian")
                #
                sums = rowSums(samBB)
                ss   = sums==0
                samP = samBB[!ss,]/sums[!ss]

                #make sure we have exactly MM samples for the database
                samP = head(rbind(samP, samP[sample.int(nrow(samP), replace=T),]), M)

                #write the stratum tree with sppComp.csv leaves.
                if( tree ){
                        #
                        suppressWarnings({
                                #NOTE: figure out conventions around working directory
                                path = sprintf('%s/', place); dir.create(path);
                                path = sprintf('%s/%s/', place, p); dir.create(path);
                                path = sprintf('%s/%s/%s/', place, p, g); dir.create(path);
                                path = sprintf('%s/%s/%s/%s/', place, p, g, q); dir.create(path);
                                path = sprintf('%s/%s/%s/%s/%s/', place, p, g, q, y); dir.create(path);
                        })
                        #
                        colnames(samP) = sppG
                        write.table(format(samP, scientific=T, digits=6), file=sprintf("%ssppComp.csv", path), row.names=F, col.names=T, quote=F, sep=',')
                }

                #
                #reform the predSam shape (in parrallel the order of predSppComp will not match predSam)
                #predSppComp = cbind(predSppComp, samP)
                return(samP)
        }
        stopCluster(cl)

        #
        return(list(nameDF=nameDF, predSppComp=predSppComp))
}

#NOTE: remove gold strata and only predict for observed strata.
#D will need to be an argument
sppNumPredAgg = function(M, brmsOut, sppG, portG, gearG, yearG, qtrG, tree=F, place="./", cores=1){
	#
	
	#
        stopifnot(M<summary(brmsOut)$total_ndraws)
	
        #
        nameDF = expand.grid(species=sppG, port=portG, gear=gearG, year=yearG, qtr=qtrG)
        predStrat = expand.grid(port=portG, gear=gearG, year=yearG, qtr=qtrG)
	
        #predSppComp = c()
        #for(i in 1:nrow(predStrat)){
        cl = makePSOCKcluster(cores)
        registerDoParallel( cl )
        predSppNum = foreach(i=1:nrow(predStrat), .combine='cbind', .inorder=T)%dopar%{ #.export=c("brmsOut")
                #
                p = predStrat[i,"port"]
                g = predStrat[i,"gear"]
                y = predStrat[i,"year"]
                q = predStrat[i,"qtr"]
                #
                predD = data.frame(species=sppG, port=p, gear=g, year=y, qtr=q)
                predD$YQ = as.character(interaction(predD[,'year'], predD[,'qtr']))
                predD$nBB = 100

                #
                samBB = brms::posterior_predict(brmsOut, newdata=predD, allow_new_levels=T, sample_new_levels="uncertainty")
	
		#
		
		
		#make sure we have exactly M samples for the database
                samBB = head(rbind(samBB, samBB[sample.int(nrow(samBB), replace=T),]), M)
		
		#
                #reform the predSam shape (in parrallel the order of predSppComp will not match predSam)
                #predSppComp = cbind(predSppComp, samP)
                return(samBB)
        }
        stopCluster(cl)

	#return(predSppComp)
	#
        return(list(nameDF=nameDF, predSppComp=predSppNum))
	
	#
	#samBB = brms::posterior_predict(brmsOut, newdata=predD, allow_new_levels=T, sample_new_levels="uncertainty")
}




