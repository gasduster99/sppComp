rm(list=ls())

#NOTE: need these librarys to change xlim, I think  mostly ggplot2
library(brms)
library(R.utils)
library(ggplot2)
library(matrixStats)
library(RColorBrewer)

#
#MAIN
#

#NOTE: compare INLA and BRMS results

#
path = "./250N1978to1982SPGY:Q/"
load(sprintf('%s/data250N1978to1982SPGY:Q.RData', path))
out = readRDS(sprintf('%s/brms250N1978to1982SPGY:Q.rds', path))

#
#P0
#

#
prop_zero <- function(y) mean(y == 0)
#
prop_zero_testAll = pp_check(out, type="stat", stat="prop_zero")
png(sprintf("%s/p0All%s.png", path, modID)) #, width=960/18*(yHi-yLo), height=400*4)
plot(prop_zero_testAll + coord_cartesian(xlim=c(0.9, 1)) + scale_x_continuous(labels=scales::number_format(accuracy = 0.01))) # + coord_cartesian(xlim=c(0.9, 1)))
dev.off()
#
prop_zero_testSpp = pp_check(out, type="stat_grouped", stat="prop_zero", group="species")
png(sprintf("%s/p0Spp%s.png", path, modID), width=800, res=70) #, width=960/18*(yHi-yLo), height=400*4)
plot(prop_zero_testSpp + coord_cartesian(xlim=c(0.2, 1)) + scale_x_continuous(labels=scales::number_format(accuracy = 0.2)))
dev.off()
#
prop_zero_testPort = pp_check(out, type="stat_grouped", stat="prop_zero", group="port")
png(sprintf("%s/p0Port%s.png", path, modID), res=100) #, width=960/18*(yHi-yLo), height=400*4)
plot(prop_zero_testPort + coord_cartesian(xlim=c(0.9, 1)) + scale_x_continuous(labels=scales::number_format(accuracy = 0.01)))
dev.off()
#
prop_zero_testGear = pp_check(out, type="stat_grouped", stat="prop_zero", group="gear")
png(sprintf("%s/p0Gear%s.png", path, modID)) #, width=960/18*(yHi-yLo), height=400*4)
plot(prop_zero_testGear + coord_cartesian(xlim=c(0.9, 1)) + scale_x_continuous(labels=scales::number_format(accuracy = 0.01)))
dev.off()

#
rm(list=c("prop_zero_testAll", "prop_zero_testSpp", "prop_zero_testPort", "prop_zero_testGear"))
gc()

#
#MEAN
#

#
mTestAll = pp_check(out, type="stat", stat="mean")
png(sprintf("%s/mmAll%s.png", path, modID)) #, width=960/18*(yHi-yLo), height=400*4)
plot(mTestAll) 
dev.off()
#
mTestSpp = pp_check(out, type="stat_grouped", stat="mean", group="species")
png(sprintf("%s/mmSpp%s.png", path, modID), width=800, res=70) #, width=960/18*(yHi-yLo), height=400*4)
plot( mTestSpp + coord_cartesian(xlim=c(0, 22)) + scale_x_continuous(labels=scales::number_format(accuracy = 1)))
dev.off()
#
mTestPort = pp_check(out, type="stat_grouped", stat="mean", group="port")
png(sprintf("%s/mmPort%s.png", path, modID), res=100) #, width=960/18*(yHi-yLo), height=400*4)
plot( mTestPort + coord_cartesian(xlim=c(0, 4)) + scale_x_continuous(labels=scales::number_format(accuracy = 0.5)) )
dev.off()
#
mTestGear = pp_check(out, type="stat_grouped", stat="mean", group="gear")
png(sprintf("%s/mmGear%s.png", path, modID)) #, width=960/18*(yHi-yLo), height=400*4)
plot( mTestGear + coord_cartesian(xlim=c(0, 5)) + scale_x_continuous(labels=scales::number_format(accuracy = 1)) )
dev.off()

#
rm(list=c("mTestAll", "mTestSpp", "mTestPort", "mTestGear"))
gc()

#
#SD
#

#
sTestSD = pp_check(out, type="stat", stat="sd")
#
png(sprintf("%s/sdAll%s.png", path, modID)) #, width=960/18*(yHi-yLo), height=400*4)
plot(sTestSD)#+coord_cartesian(xlim=c(0, 10)))
dev.off()
#
sTestSpp = pp_check(out, type="stat_grouped", stat="sd", group="species")
png(sprintf("%s/sdSpp%s.png", path, modID), width=800, res=70) #, width=960/18*(yHi-yLo), height=400*4)
plot( sTestSpp + coord_cartesian(xlim=c(0, 40)) + scale_x_continuous(labels=scales::number_format(accuracy = 10)))
dev.off()
#
sTestPort = pp_check(out, type="stat_grouped", stat="sd", group="port")
png(sprintf("%s/sdPort%s.png", path, modID), res=100) #, width=960/18*(yHi-yLo), height=400*4)
plot( sTestPort + coord_cartesian(xlim=c(5, 22)) + scale_x_continuous(labels=scales::number_format(accuracy = 5)))
dev.off()
#
rm(list=c("sTestSD", "sTestSpp", "sTestPort"))
gc()
#
sTestGear = pp_check(out, type="stat_grouped", stat="sd", group="gear")
png(sprintf("%s/sdGear%s.png", path, modID)) #, width=960/18*(yHi-yLo), height=400*4)
plot( sTestGear + coord_cartesian(xlim=c(5, 25)) + scale_x_continuous(labels=scales::number_format(accuracy = 5)))
dev.off()


##
#png(sprintf("p0Year%s.png", id)) #, width=960/18*(yHi-yLo), height=400*4)
#plot( prop_zero_test2+coord_cartesian(xlim=c(0.6, 1)) ) #xlim(0.25, 0.75) #+ coord_cartesian(xlim = c(0, 1))
#dev.off()

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


