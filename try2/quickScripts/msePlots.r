rm(list=ls())

#
library(ggpubr)
library(rstatix)

#
load('mses.RData')

#
#
#

#
mses = c(pMSE, bMSE, nbMSE, bbMSE)
like = c(rep('Poisson',length(pMSE)), rep('Binomial',length(bMSE)), rep('Negative Binomial',length(nbMSE)), rep('Beta-Binomial',length(bbMSE)))
facLike = factor(like, levels=c("Binomial", "Poisson", "Negative Binomial", "Beta-Binomial"))
data = data.frame(MSE=mses, Likelihood=facLike)

#
out = pairwise_t_test( data, MSE~Likelihood, 
	paired = F,
    	p.adjust.method = "none" #"fdr" #"BH" #"bonferroni"
)
out = out %>% add_xy_position(x = "Likelihood") 
out = out %>% mutate(y.position = seq(0.24, 0.32, length.out=6))
out = out %>% add_significance(cutpoints=c(0,0.001, 0.01, 0.05, 0.1, 1), symbols=c('***', '**', '*', '.', ''))

#
bp = ggplot(data, aes(x=Likelihood, y=MSE)) + geom_boxplot(outlier.shape=NA) 
bp = bp + theme_classic()
bp = bp + stat_pvalue_manual(out)
bp = bp + ggtitle("Likelihood Performance") + theme(plot.title = element_text(hjust = 0.5))
#
ggsave('mseBoxP.pdf', plot=bp)









#out[c(5,3,6,1,4,2),])
#bp = bp + scale_x_discrete(limits=c("Binomial", "Poisson", "Negative Binomial", "Beta-Binomial")) 








#out = out %>% add_xy_position(x = "time")
#ggboxplot(data, x='like', y='mses') + stat_pvalue_manual(out)
#mseBox = cbind(pMSE, bMSE, nbMSE, bbMSE)
#boxplot(mseBox, ylab="MSE", main="Likelihood Performance", ylim=c(0.06, 0.23)) + stat_pvalue_manual(out)


