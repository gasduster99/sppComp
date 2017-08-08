rm(list=ls())

#
library(plotrix)
library(parallel)
library(partitions)
library(RColorBrewer)


#
#FUNCTIONS
#

#
stir = function(n, k){
	#
	js = seq(0,k)
	out = factorial(k)^(-1) * sum( (-1)^(k-js) * choose(k,js) * js^n )
	#
	return( out )
}
stir = Vectorize(stir, c('k'))

#
Bk = function(N){
       	#
	return( sum(stir(N,seq(1,N))) ) 
}
Bk = Vectorize(Bk, c('N'))


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
#MAIN
#

#
kMax = 10
threads = 4
#
mcOut = mclapply(
	rev(1:kMax),
	FUN = function(k){
		#
		ll = listParts( k )
		nll = length( ll )
		#
		out = list(adj=0, sml=0, adjSml=0, ssm=0, adjBig=0, big=0, Bk=Bk(k))
		for(i in seq(1, nll)){
			#
			#isAdj(xx, 1) : no leapfrogging
        		#isAdj(xx, 2) : single port leapfrogging
        		#isAdj(xx, 3) : double port leapfrogging
        		#...
        		#isSml(xx, 1): super groups 1 big (all separate)
        		#isSml(xx, 2): super groups 2 big (1 port sharing)
        		#isSml(xx, 3): super groups 3 big (2 port sharing)
        		#...
        		#the constraint    v                   v   
        		if( isAdj(ll[[i]], 1) & isSml(ll[[i]], 3) ){ out$adjSml = out$adjSml+1 }
			if( isAdj(ll[[i]], 1) & isSml(ll[[i]], 4) ){ out$adjBig = out$adjBig+1 }
			if( isAdj(ll[[i]], 1) ){ out$adj = out$adj+1 }
			if( isSml(ll[[i]], 2) ){ out$ssm = out$ssm+1 }
			if( isSml(ll[[i]], 3) ){ out$sml = out$sml+1 }	
			if( isSml(ll[[i]], 5) ){ out$big = out$big+1 }	
		}
		#
		return(unlist(out))
	},
	mc.cores=threads,
	mc.preschedule=F
)
mcOut = do.call(rbind, mcOut)

#
#PLOT
#

#
#kMin = 7
#xFill = c(1:kMin)
#yFill = c(Bk(seq(1, kMin-1)))
#fill = c(900, 1200, 1500, 1800) #1070,1400,1800)
#yFill=c(yFill, fill)
#for(i in (kMin+1):kMax){ xFill=c(xFill, i) }
#
plot( 1:kMax, rev(mcOut[,'Bk']), #xFill, yFill, 
	type = 'b',
	yaxt = "n", 
	xaxt = "n",
	xlab = "K",
	ylab = expression("Models"),
	main = 'Constraint Scaling',
	ylim = c(0, 10000),
	lwd=2
)
#
pal=brewer.pal(5,"Set1")[2:5]
#adj = c(-150, -200, -250)
#adj2 = c(-400, -600)
for(i in 1:5){
	#if(i==2){ lines(xFill, c(rev(mcOut[,i])[1:7], fill[2:4]+adj), col=pal[i], type='b', lwd=2)
	#}else if(i==4){ lines(xFill, c(rev(mcOut[,i])[1:8], fill[3:4]+adj[2:3]+adj2), col=pal[i], type='b', lwd=2) 
	#}else{ lines(xFill, rev(mcOut[,i]), col=pal[i], type='b', lwd=2) }
	lines(1:kMax, rev(mcOut[,i]), col=pal[i], type='b', lwd=2)
}
##
#axis(1, at=seq(1, 10), labels=seq(1, 10))
#axis(2, at=c(seq(0, 704, 64), 1000, 1300, 1700), labels=c(seq(0, 704, 64), 10^3, 10^4, 10^5))
#br = 825#950
#axis.break(axis=2, breakpos=br, pos=NA, bgcol="white", breakcol="red", style="slash", brw=0.03)
#abline(h=br, col='red', lty=2)
#abline()

#legend('topleft', legend=c('Bk', ''))


##
#kMax = 7
##
#pdf('compTime.pdf')
#plot( c(seq(1, kMax), 8, 9, 10), c(30*Tk(seq(1, kMax))/60/24, 25, 30, 35), 
#	yaxt = "n", 
#	xaxt = "n",
#	xlab = "K",
#	ylab = expression("Days of Computaton ("%prop%T[K]*")"),
#	main = 'Approximate Computation Time'
#)
#abline(h=c(22.5, 27.5, 32.5), lty=5, col='red')
#axis(1, at=seq(1, 10), labels=seq(1, 10))
#axis(2, at=seq(0, 35, 5), labels=c(seq(0, 20, 5), round(30*Tk(8:10)/60/24)))
#axis.break(axis=2, breakpos=22.5, pos=NA, bgcol="white", breakcol="red", style="slash", brw=0.05)
#axis.break(axis=2, breakpos=27.5, pos=NA, bgcol="white", breakcol="red", style="slash", brw=0.05)
#axis.break(axis=2, breakpos=32.5, pos=NA, bgcol="white", breakcol="red", style="slash", brw=0.05)
#dev.off()
