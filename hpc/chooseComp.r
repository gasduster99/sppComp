##rm(list=ls())
#
##
##
##
#
##
#raw = read.table('chooseSheet.csv', sep=',', header=T, colClasses=c(NA, NA, NA, NA, NA, NA, NA, NA, "NULL", "NULL", "NULL", "NULL"))
#dat = raw[9:17,1:4] 
#dat = rbind(dat, raw[20:27,1:4])
#dat = rbind(dat, raw[30:46,1:4])
#dat = rbind(dat, raw[c(48, 50, 52),1:4])
#colnames(dat) = c('cluster', 'clock', 'threads', 'cost')
##no 10-Gigabit
#dat$cost = dat$cost-300
##
#budget = 23500
#overhead = 0#657.48 #4*657.48
#buddy = budget-overhead
##
#D = list()
#R = dim(dat)[1]
#for(r1 in 1:R){ #5){#1:R){
#	#
#	d1 = dat[r1,c(1, 2, 3, 4)]
#	D = c(D, list(d1))
#	#
#	for(r2 in 1:R){
#		#
#		d2 = rbind(d1, dat[r2,c(1, 2, 3, 4)])
#		if( sum(d2$cost)>buddy ){ next
#		} else{ D=c(D, list(d2)) }
#		#
#		for(r3 in 1:R){
#			#
#			d3 = rbind(d2, dat[r3,c(1, 2, 3, 4)])
#                	if( sum(d3$cost)>buddy ){ next
#                	} else{ D=c(D, list(d3)) }
#			#
#			for(r4 in 1:R){
#				#
#				d4 = rbind(d3, dat[r4,c(1, 2, 3, 4)])
#                		if( sum(d4$cost)>buddy ){ next
#                		} else{ D=c(D, list(d4)) }		
#			}
#		}
#	}
#}
##hash D
#nome = sapply(D, function(x){ paste(sort(x$cluster), collapse='|') })
#names(D) = nome
##remove redundant configurations
UD = lapply( unique(nome), function(n){ D[[n]] } )
summ = sapply(UD, function(x){ sum(x$clock*x$threads) } ) #sum(x$threads) } )#
wMax = which( summ==max(summ) )
print( UD[[wMax]] )
