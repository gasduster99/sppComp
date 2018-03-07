rm(list=ls())

#
#DATA
#

#
dat = read.csv('total_rf_lbs_by_year_grp78.csv')
colnames(dat) = c('year', 'mcat', 'weight')
modMcat = c(250, 253, 262, 265, 269, 270, 956, 959, 961)
##
#dat = read.csv('total_rf_lbs_by_year_grp83.csv')
#colnames(dat) = c('year', 'mcat', 'weight')
#modMcat = c(245, 250, 253, 259, 262, 269, 270, 663, 667, 956, 959, 960, 961)
##
mcatLands = aggregate(dat$weight, by=list(dat$mcat), FUN=sum)
colnames(mcatLands) = c('mcat', 'weight')
#
propExpand = sum(mcatLands[mcatLands$mcat%in%modMcat,'weight'])/sum(mcatLands$weight)
print(propExpand)
