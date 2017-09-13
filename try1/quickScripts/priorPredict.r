rm(list=ls())

#
library(boot)
library(bbmle)

#
#DEFAULT
#

##
#m = 1000
##
#vq  = rgamma(m, 1, 10^5)
#vy  = rgamma(m, 1, 10^5)
#vyq = rgamma(m, 1, 10^5)
##
#q  = rnorm(m, 0, sqrt(1/vq))
#y  = rnorm(m, 0, sqrt(1/vy))
#yq = rnorm(m, 0, sqrt(1/vyq))
##
#g = rnorm(m, 0, sqrt(10^3))
#p = rnorm(m, 0, sqrt(10^3))
#s = rnorm(m, 0, sqrt(10^3))
##
#size = rnorm(m, 0, sqrt(10^3))
#defPP = rnbinom(m, mu=exp(s+p+g+y+q+yq), size=exp(size))

#
#PICTURED (beta-binomial)
#

##
#N = 100
#m = 10^6
##2e4 #20^3 #20^1
#vq  = rgamma(m, shape=1, scale=20^1)
#vy  = rgamma(m, shape=1, scale=20^1)
#vyq = rgamma(m, shape=1, scale=20^1)
##0.15
#q  = rnorm(m, 0, sqrt(vq))
#y  = rnorm(m, 0, sqrt(vy))
#yq = rnorm(m, 0, sqrt(vyq))
##0.15, sqrt(10^2)
#g = rnorm(m, 0, 32)
#p = rnorm(m, 0, 32)
#s = rnorm(m, 0, 32)#0.001)
##
#theta = rnorm(m, 0, sqrt(1/0.4))
#rho   = inv.logit(theta)
#mup   = inv.logit(s+p+g+y+q+yq)
#alpha = mup*(1-rho)/rho
#beta  = (1-mup)/rho
##
#prob = rbeta(m, alpha, beta) 
#ppBB = rbinom(m, size=N, prob=prob)
#ppBB0 = rbinom(m, size=N, prob=prob)
#ppBB1 = rbinom(m, size=N, prob=prob)
##pdf('priorPredict.pdf', width=5, height=5)
#dev.new()
#plot(density(ppBB, from=0, to=N),
#	lwd=2, 
#	ylim=c(0.000185, 0.005),
#	main="Prior Predictive Weight",
#	xlab="Sampled Weight",
#	ylab="Probability",
#	type="b"
#)
##dev.off()

#
#beta-binomial(slope)
#

#
N = 100
m = 10^6
#2e4 #20^3 #20^1
vq  = rgamma(m, shape=1, scale=20^1)
vy  = rgamma(m, shape=1, scale=20^1)
vyq = rgamma(m, shape=1, scale=20^1)
#0.15
q  = rnorm(m, 0, sqrt(vq))
y  = rnorm(m, 0, sqrt(vy))
yq = rnorm(m, 0, sqrt(vyq))
#0.15, sqrt(10^2)
g = 0#rnorm(m, 0, 32)
p = 0#rnorm(m, 0, 32)
s = 0#-50#vq*100#rnorm(m, 0, 32)#0.001)#
#
theta = rnorm(m, 0, sqrt(1/0.4))
rho   = inv.logit(theta)
mup   = inv.logit(s+p+g+y+q+yq)
alpha = mup*(1-rho)/rho
beta  = (1-mup)/rho
#
prob = rbeta(m, alpha, beta) 
ppBB = rbinom(m, size=N, prob=prob)
ppBB0 = rbinom(m, size=N, prob=prob)
ppBB1 = rbinom(m, size=N, prob=prob)
#pdf('priorPredict.pdf', width=5, height=5)
dev.new()
plot(density(ppBB, from=0, to=N),
	lwd=2, 
	ylim=c(0.000185, 0.005),
	main="Prior Predictive Weight",
	xlab="Sampled Weight",
	ylab="Probability",
	type="b"
)
#dev.off()

#
#size = rgamma(m, 1, 1)
#pp = rnbinom(m, mu=exp(s+p+g+y+q+yq), size=size)

