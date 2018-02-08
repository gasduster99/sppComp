rm(list=ls())

#
library(maps)
#library(foreach)
#library(doParallel)
library(HDInterval)
library(KernSmooth)
library(RColorBrewer)

#
#FUNCTIONS
#

#
goodRatios = function( loggedStuff ){
        #loggedStuff: the log(x) of the values (x) to compute the following ratio
        #       r_i = x_i/sum(x)

        #
        if(any(is.na(loggedStuff))){
                #
                warning("'NA' present in loggedStuff")
                naWhere = which(is.na(loggedStuff))
                loggedStuff = loggedStuff[!is.na(loggedStuff)]
                #
                c = max(loggedStuff)
                stand = exp(loggedStuff-c)
                sStand = sum(stand)
                out = stand/sStand
                #
                for(na in naWhere){ out=append(out, 0, na-1) }
                return( matrix(out) )

        }
        #
        c = max(loggedStuff)
        stand = exp(loggedStuff-c)
        sStand = sum(stand)
        out = stand/sStand
        #
        return( out )
}


#
#PSUDO CODE
#

#figure out mcat # model order (w/ model weights)
	#dynamic head size of model list
#translate R colors to latex colors (need at most 10 colors)
	#https://tex.stackexchange.com/questions/265722/colouring-lines-in-table-when-using-multicolumn
#cols = c(brewer.pal(9,"Set1"), 'black')
cols = c(gsub('#', '', brewer.pal(9,"Set1")), '000000')
#figure out how to make latex MCAT table header
#plot a single black text cali map

#
gos = c(
        ##78-82
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT250/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT253/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT269/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT956/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT265/",
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT262/",
        "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT270/",
        "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT959/",
        "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/78to82/MCAT961/",
        ##83-90
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT956/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT269/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT959/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT961/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT960/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT667/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT253/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT259/",
        #"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT663/"
	"/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT245/",
        "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT262/",
        "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT270/",
        "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT663/"
)
#basic knobs
h = 5
path = "/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/"
cols = c(gsub('#', '', brewer.pal(9,"Set1")), '000000')
ports = c('CRS', 'ERK', 'BRG', 'BDG', 'OSF', 'MNT', 'MRO', 'OSB', 'OLA', 'OSD')
#a few fillers
mcat = '250'
weights = rep(1/h, h)

#start figurin
timePeriods = Sys.glob(sprintf('%s*to*/', path))
for( tp in timePeriods ){
	#
        yearsStr = strsplit(tp, '/')[[1]][6]
        mcats = Sys.glob(sprintf('%sMCAT*/', tp))
	#
	for( mcat in mcats ){
                ##skippers
                #if( mcat=="/media/nick/67cffa21-c600-49a3-91a1-3f7567c21507/fullTimeComplete/83to90/MCAT250/" ){ next }        
                if( !(mcat%in%gos) ){ next }
		#
		#MAKE WEIGHTS
		load(sprintf('%s/Top/Space274/Space274.RData', mcat))
		#
		mets = Sys.glob(sprintf('%sTop/Space*/metrics.csv', mcat))
		metDF = matrix(NA, nrow=length(mets), ncol=4)
		#
		i = 1;
                rNames = matrix(NA, nrow=length(mets), ncol=1)
                for( met in mets ){
                       #
                       metDF[i,c(1,2,3)] = unlist(read.csv(met)[1:3])
                       rNames[i] = strsplit(met, '/')[[1]][9]
                       #
                       i = i+1
                }
                rownames(metDF) = rNames
                colnames(metDF) = c('mlik', 'waic', 'dic', 'weight')
                #
                metDF[,4] = goodRatios(metDF[,'mlik'])
                metDF = metDF[order(metDF[,'mlik'], decreasing=T),] #, decreasing=T),]
		#
		weights = head(metDF[,4], h)
		
		#GET MODEL GROUPS
		spaceNames = names(weights)
		cMat = c()
		for( sn in spaceNames){ 
                       #       
                       load( sprintf('%s/Top/%s/%s.RData', mcat, sn, sn) )     
                       col = rep(NA, length(portGold))
		       portEff2 = c()
		       for(pg in portGold){ portEff2 = c(portEff2, portEff[grep(pg, portEff)]) }
		       portEff2 = unique(portEff2)
                       for( j in 1:length(portGold) ){ col[j]=cols[grep(portGold[j], portEff2)] }
                       cMat = cbind(cMat, col)
		       ##
                       #mapName = sprintf('%s%sMap%d.pdf', nameSplit[6], nameSplit[7], i)
                       #pdf(mapName, width=7, height=15)
                       #makeMap(col)
                       #dev.off()
                       ##
                       #i = i+1
                }
		colnames(cMat) = spaceNames
		rownames(cMat) = ports
		#
		mcatNum = tail(strsplit(tail(strsplit(mcat, '/')[[1]], 1), 'T')[[1]], 1)		

		#LATEX		
		writeLines(sprintf('\\begin{tabular}{|%s|}', paste(rep('c', h+1), collapse='|')))
		#\cline{2-6}
		writeLines(sprintf('\t \\hline \\multicolumn{%d}{|c|}{MCAT %s} \\\\ \\hline', h+1, mcatNum))
		writeLines(sprintf('\t $\\omega$&%s \\\\ \\hline', paste(sprintf('%1.2f', round(weights, 2)), collapse='&')))
		for(p in ports){
			writeLines(sprintf('\t%s&%s \\\\ \\hline', p, paste(sprintf('\\cellcolor[HTML]{%s}', cMat[p,]), collapse='&')))
		}
		writeLines('\\end{tabular}')	
	}
}




#\multicolumn{6}{l|}{\cellcolor[HTML]{6195C9}Something}
#print weights
#for each port print all model colors

#writeLines('\\begin{tabular}{l|c|r}')	
#  \hline
#  Some & \cellcolor[HTML]{000000} \cellcolor{blue!25}coloured & contents \\
#  \hline
#writeLines('\\end{tabular}')

