rm(list=ls())

#
library(maps)
library(RColorBrewer)

#
#FUNCTIONS
#

#
makeMap = function(cols){
        #
	texts = c(expression(bold('CRS')), expression(bold('ERK')), expression(bold('BRG')), expression(bold('BDG')), expression(bold('OSF')), expression(bold('MNT')), expression(bold('MRO')), expression(bold('OSB')), expression(bold('OLA')), expression(bold('OSD'))) 
        #
        top = 0.98
        bot = 0.12
        #
        map(database="state", regions="california",
               projection="gilbert",
               orientation=c(120, 0, 240),
               fill=T,
               col='grey'
        )
        #
        mtext(texts,
               side=2,
               adj=seq(top, bot, -(top-bot)/(length(texts)-1)),
               cex=2.5,
               col=cols,
               line=0
        )
}


#
#CODE
#

#
mapWidth = 7
mapHeight = 15
#
nPort = 10
cols = brewer.pal(9,"Set1")
#
pdf('../pictures/mapLeapFrog.pdf', width=mapWidth, height=mapHeight)
colors=matrix(NaN, nrow=nPort, ncol=1)
colors[seq(1, nPort/2, 2)] = cols[1]
colors[seq(2, nPort/2, 2)] = cols[2]
colors[seq(nPort/2+1, nPort, 2)] = cols[3]
colors[seq(nPort/2+2, nPort, 2)] = cols[4]
makeMap(colors)
dev.off()
##
#pdf('mapFullBlank.pdf', width=mapWidth, height=mapHeight)
#makeMap(rep('black', nPort))
#dev.off()
##
#pdf('mapFullHalfHalf.pdf', width=mapWidth, height=mapHeight)
#makeMap(c(rep(cols[3], nPort/2), rep(cols[2], nPort/2)))
#dev.off()
##
#pdf('mapFullConcMend.pdf', width=mapWidth, height=mapHeight)
#makeMap(c(rep(cols[1], 2), rep(cols[2], 3), rep(cols[3], 2), rep(cols[4], 3)))
#dev.off()
##
#pdf('mapFullEveryOther.pdf', width=mapWidth, height=mapHeight)
#colors=matrix(NaN, nrow=nPort, ncol=1)
#colors[seq(1, nPort, 2)] = cols[3]
#colors[seq(2, nPort, 2)] = cols[2]
#makeMap(colors)
#dev.off()
##
#pdf('mapFullSparse.pdf', width=mapWidth, height=mapHeight)
#makeMap(c(cols, 'black'))
#dev.off()



