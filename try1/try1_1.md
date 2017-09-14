---
title: Improving Catch Estimation Methods in Sparsely Sampled Mixed-Stock Fisheries.
author: Nick Grunloh, Edward Dick, Don Pearson, John Field, Marc Mangel
---

<!--
http://www.nrcresearchpress.com/page/cjfas/authors#page
-->

<!--
# Abstract

Effective management of exploited fish populations, requires accurate estimates 
of commercial fisheries catches to inform monitoring and assessment efforts. 
In California, the high degree of heterogeneity in the species composition of 
many groundfish fisheries, particularly those targeting rockfish 
(genus $Sebastes$), leads to challenges in sampling all potential strata, or 
species, adequately. Limited resources and increasingly complex stratification 
of the sampling system inevitably leads to gaps in sample data. 
In the presence of sampling gaps, ad-hoc species composition point estimation 
is currently obtained according to historically derived “data borrowing” 
(imputation) protocols which do not allow for uncertainty estimation or 
forecasting. In order to move from the current ad-hoc “data-borrowing” 
point estimators, we have constructed Bayesian hierarchical models to estimate 
species compositions, complete with accurate measures of uncertainty, as well 
as theoretically sound out-of-sample predictions. Furthermore, we introduce a 
computational method for discovering consistent “borrowing” strategies 
across over-stratified data. Our modeling approach, along with a 
computationally robust system of inference and model exploration, allows us to 
start to understand the effect of the highly stratified, and sparse, sampling 
system on the kinds of inference possible, while simultaneously making the 
most from the available data. 

# Significance

In order to understand how fish populations respond to fishing, it is critical 
to obtain accurate estimates of how many fish are removed from the ocean 
(catch) and to quantify the precision of those estimates. Traditionally, 
population dynamics models used to measure this response to fishing (“stock 
assessments”) are conditioned on a time series of annual catches. These catch 
estimates are often treated as being known without error, despite the fact 
that they are derived from sampling programs that estimate the proportion of 
unsampled strata must be “filled in” through a process sometimes referred 
to on the U.S. West Coast as “borrowing” (i.e. data imputation). Historically, 
methods used to “borrow” information among strata have been different species 
found within multiple sampling strata.Sampling error introduces uncertainty 
into estimates of the catch, and ad-hoc in nature and driven by expert opinion 
of local managers (Sen et al. 1984, 1986) (Pearson and Erwin  1997). We seek 
to improve upon this practice through development of a model-based approach 
that provides estimates of catch and associated uncertainty, as well as an 
objective, defensible framework for model selection and data imputation. 
Although the theoretical basis for a model based estimation of species 
composition in mixed stock fisheries has been advanced (Shelton et al., 2012), 
it has not yet been implemented successfully using actual historical or 
contemporary data.

![Spase Data](./pictures/sampleComplex.png)

The difficulties associated with the existing ad-hoc approach are magnified by 
an increase in the number of sampling strata over time, specifically the 
number of “market categories,” into which fishermen and dealers sort their 
catch (Figure 1, Bottom). The increase in the number of market categories 
(sampling strata) has not been matched by increases in sampling effort, 
resulting in a decline in the average number of samples per stratum (Figure 1, 
Middle). In other words, data are becoming more sparse, increasing our 
uncertainty in estimates of catch. Since the data are also stratified over a 
number of ports, fishing gear types, years, and quarters, inference is not 
possible without some sort of stratum pooling. Rather than rely so heavily on 
the previous, ad-hoc pooling rules which change based on the availability of 
samples, we hope to standardize any necessary pooling through an exhaustive 
search of the space (possible configurations) of pooled models. Pooling (and 
partial pooling) among strata is achieved using Bayesian hierarchical 
statistical models and model averaging (Gelman et al., 2014).
-->





# Abstract

# Introduction
## Context
<!--##Data, Sampling, \& Prep-->
## Data
* Collection issues
	* funding => nature of sparcity
* Lay down goal modeling goal
	* mean
	* uncertainty

# Methods
## Data Generating Model
<!--simple form experiment explain-->

Something something heirarchical poisson model. Something something (Shelton, 2012).

For the purposes of accuratly modeling not only species composition means, but
also higher moments of the data, such as species composition variances, it is 
neccisary to recognize model limitations with respect to over-dispersed data. 
Amoung the simplest models for count data are the poisson and binomial models. 
Both models are typically specificed with a single degree of freedom for 
modeling all of the moments of the data, and thus they rely heavily on their 
respective data generating processes to accuratly represent higher moments in 
the data. McCullagh and Nelder (1989, pg. 124) commiserate about the 
prevalence of over-dispersed data in cluster sampling, and explain the 
numerious ways in which cluster sampling may result in over-dispersion.

Extending the Poisson and binomial models to deal with over-dispersion, 
typically involves adding additional parameters for the purpose of modeling 
higher moments of the data. The negative binomial (NB) distribution is often 
used as an over-dispersed extension of the poisson model, since it can be 
expressly written as an infite mixture of poisson distributions. While the 
beta-binomial model is typically used to as an over-dispersed extension of the 
binomial model.

###An Example

<!--m=0;for(id in unique(Dat[Dat$mcat==mct & Dat$year==yer & Dat$portComplex==plc & Dat$gear==ger,]$id)){m=m+max(Dat[Dat$mcat==mct & Dat$year==yer & Dat$portComplex==plc & Dat$gear==ger & Dat$id==id,]$clust)}-->
To discern between these models we consider a small scale example of the 
Poisson, binomial, negative binomial, and beta-binomial models fit to the port 
sampling integer weight data from market category 250, in the Montery port 
complex trawl fishery in 1990. $(any will work)$ This stratum was visited 38 
times by port samplers, <!--in 1990,--> collecting a total of 67 cluster samples, 
resulting in 344 model observations across 21 $(at least; URCK)$ unique 
species. Each of the above models are fit to these data. <!--The posteriors from each model imply different predictive species composition distributions.-->
The predictive species composition distributions from each model are 
visualized in Figure (1) as 95% Highest Density Intervals (HDI) $(citations)$, 
plotted on top of the predictive means for each model and the observed species 
compositions from the data in Figure(1). For brevity we only consider the most 
prevalent six species in this example (CLPR, BCAC, WDOW, BLGL, ARRA, BANK). 
Additionally, the 
MSE, DIC, WAIC, and Bayesian marginal likelyhood model probabilities are 
computed for each model as measures of model fit as seen in Table(1).

![Interval Plot](./pictures/compPlot.pdf)

           Poisson        Binomial      NB              BB  
--------- -------------  ------------- --------------  -------------
MSE         0.05286        0.05683      0.05188         0.05170
DIC         5675.25        6759.86      1301.51         1261.00
WAIC        5840.56        6939.74      1302.19         1261.30
$pr(M|y)$   $\approx0$     $\approx0$   $<10^{-10}$     $\approx1$   
					 
<!--NB pr(M|y)=5.175555e-17-->

The large spread of the observed species compositions seen in Figure(1) 
visually demonstrate the degree of overdispersion present in port sampling 
data. The Poisson and binomial models disregaurd this overdispersion to 
prioritize fitting the data mean.
<!--
The Poisson and binomial models attempt to model both the mean and 
residual variances with a single parameter for each species. This can tend to 
biase mean estimates, in overdispersed stratum, toward larger values, for the 
sake of estimating larger residual variances. Similarly variance estimates may 
tend to be biased toward smaller values for the sake of estimating smaller 
means.
-->
In contrast, the negative binomial and beta-binomial models estimate an 
additional parameter which is intended to disentangle the mean and residual 
variance estimates. Thus the negative binomial and beta-binomial models are 
able to produce more accurate estimates of both the mean and residual 
variance.

All of the measures in Table(1) consistently agree that the negative binomial 
and beta-binomial models out perform the overdispersed Poisson and binomial 
models. Furthermore, all of the metrics in Table(1) indicate <!--are also able to discern-->
that the beta-binomial model outperforms the negative binomial model. Depending 
on the users value system toward model selection (e.g. predictive or 
inferential), the support for the beta-binomial model over the negative 
binomial model may vary, but it is worth noting that the more robust model 
selction tools show stronger support for the beta-binomial model, with Bayesian 
model probabilities indicating practically conclusive support for the 
beta-binomial model. 

The split beta-binomial intervals seen in Figure(1) are the consequence of 
confining a large amount of residual variability to the unit interval. The 
beta-binomial is the only model considered here, which estimates such a 
large degree of variablility and thus it is the only model that produces 
predictive species composition distributions of the sort. Figure(2) shows the 
beta-binomial predictive distributions as a violin plot demonstrating how the
beta-binomial model arranges predictive density over the unit interval. The 
predictive intervals in Figure(1) are the smallest possible regions on each 
density so that the intervals contain 95% of the predictive density (i.e. 
these regions represent the densest packing of 95% probbaility under each 
predictive distribution). For the cases of Aurora and Bank rockfish, the 
empty upper regions seen in Figure(1) are understandable in terms of the 
relatively low density region of the posterior they represent, as seen in 
Figure(2).

![Violin Plot](./pictures/compVioplot.pdf)

###Operationalized Model

For a particular market category, $y_{ijklm\eta}$ is the $i^{th}$ sample of 
the $j^{th}$ species' weight, in the $k^{th}$ port, caught with the $l^{th}$
gear, in the $\eta^{th}$ quarter, of year $m$. The $y_{ijklm\eta}$ are modeled 
as observations from a beta-binomial distribution conditional on parameters 
$\mu_{jklm\eta}$ and $\sigma^2_{jklm\eta}$,

<!--$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\bm{\theta}, \rho).$$-->
$$y_{ijklm\eta} \sim BB(\mu_{jklm\eta},~\sigma^2_{jklm\eta}).$$

Where $\mu_{jklm\eta}$ is the stratum level beta-binomial mean weight and 
$\sigma^2_{jklm\eta}$ is the stratum level residual variance. $\mu_{jklm\eta}$ 
is related to a linear predictor, $\theta_{jklm\eta}$, via the mean function,

<!--$$\mu_{jklm\eta} = n~\text{logit}^{-1}(\theta_{jklm\eta})$$-->
$$\mu_{jklm\eta} = n_{ijklm\eta}\frac{\exp(\theta_{jklm\eta})}{1+\exp(\theta_{jklm\eta})}=n~\text{expit}(\theta_{jklm\eta})=n~\text{logit}^{-1}(\theta_{jklm\eta}).$$

Here $n_{ijklm\eta}$ is the known cluster size for each sample. Additionally, 
$\sigma^2_{jklm\eta}$ is related to $\mu_{jklm\eta}$ and the overdispersion 
parameter, $\rho$, via the following equation,

$$\sigma^2_{jklm\eta} = \mu_{jklm\eta}\Big(1-\frac{\mu_{jklm\eta}}{n_{ijklm\eta}}\Big)\Big(1+(n_{ijklm\eta}-1)~\rho\Big).$$

<!--https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1a-epidemiology/clustered-data-->
$\rho$ is the within cluster correlation. The situation where 
$\rho\rightarrow1$ represents identical information content amoung replicates 
within a cluster, with maximal overdispersion relative to the binomial 
distribution. The situation where $\rho\rightarrow0$ represents totally 
independent information content amoug replicates within a cluster, and the 
beta-binomial model approaches the binomial model. $\rho$ explicitly models 
average overdispersion across all stratum, while $\mu_{jklm\eta}$ gives the 
model flexiblity at the stratum level through through it's linear predictor,

$$\theta_{jklm\eta} = \beta_0 + \beta^{(s)}_j + \beta^{(p)}_k + \beta^{(g)}_l + \beta^{(y:q)}_{m\eta}.$$

Firstly, $\theta$ includes an intercept ($\beta_0$) shared among all strata. 
Secondly, $\theta$ is factored among the many strata by simple additive offsets 
for each of the species ($\beta^{(s)}_j$), port-complex ($\beta^{(p)}_k$), and 
gear-group ($\beta^{(g)}_l$) categories. Finally, a year-quarter interaction 
($\beta^{(y:q)}_{m\eta}$) is included to give this model the flexibility 
to model differing seasonality from year to year. In addition to offering 
flexibility in modeling seasonalities, the year-quarter interaction provides 
an ideal structure for partially pooling data through time via a heirarchical 
prior discussed later in $Section(XX)/the following section$.

* justify linear predictor/transistion to priors

## A Heirarchical Prior
$$\text{logit}(\rho) \sim N(0, 2^2)$$
$$\left\{\beta^{(s)}_j, \beta^{(p)}_k, \beta^{(g)}_l\right\} \sim N(0, 32^2)$$
$$\beta_0 \propto 1$$

$$\beta^{(y:q)}_{m\eta} \sim N(0, v_m)$$
-or-
$$\beta^{(y:q)}_{m\eta} \sim N(0, v_\eta)$$
-or-
$$\beta^{(y:q)}_{m\eta} \sim N(0, v)$$

$$v \sim IG(1,~2\times10^{3}) ~~~\forall~~~v$$



### Simple
           $v_m$          $v_\eta$      $v$           
--------- -------------  ------------- -------------- 
MSE        NA             NA            NA       
DIC        -259421.18     -262477.62    -259338.34       
WAIC       625910.06      623015.65     623938.31     
$pr(M|y)$  0.1956109      0.2831926     0.5211966   
<!-- -6968.01, -6967.64, -6967.03-->

### Real
           $v_m$          $v_\eta$      $v$           
--------- -------------  ------------- -------------- 
MSE        NA             NA            NA       
DIC        NA             NA            NA       
WAIC       NA             NA            NA       
$pr(M|y)$  NA             NA            NA   



![Prior Prediction](./pictures/priorPredict.pdf)



## Prediction 
<!--
$$p(y^*_{jklm\eta}|\bm{y}) = \int\!\!\!\!\int\! \text{BB}\Big( y^*_{jklm\eta}|\mu_{jklm\eta}, \sigma^2_{jklm\eta} \Big) P\Big(\mu_{jklm\eta}, \sigma^2\Big) P\Big(\mu_{jklm\eta}, \sigma^2_{jklm\eta} | \bm{y}\Big) d\mu_{jklm\eta} d\sigma^2_{jklm\eta}$$
$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ \bm{y}^*_{klm\eta}\neq \bm{0}$$
-->
$$p(y^*_{jklm\eta}|y) = \int\!\!\!\!\int\! \text{BB}\Big( y^*_{jklm\eta}|\mu_{jklm\eta}, \sigma^2_{jklm\eta} \Big) P\Big(\mu_{jklm\eta}, \sigma^2_{jklm\eta} | y\Big) d\mu_{jklm\eta} d\sigma^2_{jklm\eta}$$
$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ y^*_{klm\eta}\neq 0$$

## Model Exploration \& Averaging

# Results
* General Products
* Degree of smoothing (heirarchical parameters)
* Posterior v. Current
	* Report degree of similarity
* Prediction v. Data
	* Report predictive accuracy

# Conclusions
* General Math/Science
* Database Stuff
* Looking Forward	
	* forcasting/hindcasting
		* simple
		* timeseries models
	* more computation faster
		* broader model exploration
		* broader spatial expansion











<!--
  
The data's suggestion that species compositions are quite variable 
it respondes to the data's suggestion that species compositions are quite 
variable. When large variablity is confined to the exist within the unit 
interval, such as species compositions, 


, not only from a predictive standpoint. 
definative to slight. 


in agreement about 
discerning between the negative binomial and beta-binomial models.  
beta-binomial model outperforms the negative binomial model as well
discerning between the negative binomial and beta-binomial models as well 
although depending on which metric the 

The MSE scores in Table(1) demonstrate that teh  that both the negative binomial and 
beta-binomial models are able to consistently out performnot only is this pattern visually true, but allmetric



  In the most overdispersed 
casesDue to the presence of
overdispersion in  

* describe picture
* notice overdispersion
* impropper variance model biases mean, beta-binomial is flexible to disentagles these effects.
-->
<!--data talk-->
<!--
See proposals...

* maths stuffs
	* mean function
	* variance; introduct $\rho$

For a particular market category, $y_{ijklm\eta}$ is the $i^{th}$ sample of 
the $j^{th}$ species' weight, in the $k^{th}$ port, caught with the $l^{th}$ 
gear, in the $\eta^{th}$ quarter, of year $m$. The $y_{ijklm\eta}$ are modeled 
as observations from a Beta-Binomial distribution ($BB$) conditional on 
parameters $\theta$ and $\rho$.
-->
<!--$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\bm{\theta}, \rho).$$-->
<!--$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\theta, \rho).$$-->

<!--Justify: single stratum poisson v. binomial v. negative binomial v. beta-binomial-->
<!--
Given observed overdispersion relative to the Poisson and Binomial 
distributions, the Beta-Binomial model makes use of a correlation parameter, 
$\rho$, to better model uncertainties, while maintaining a flexible model on 
stratum means through the linear predictor. The linear predictor parameters, 
$\theta$, are then factored as follows among the many strata,

$$\theta_{jklm\eta} = \beta_0 + \beta^{(s)}_j + \beta^{(p)}_k + \beta^{(g)}_l + \beta^{(y:q)}_{m\eta}.$$

As a Bayesian model, we specify any information external to the dataset, 
through our priors on the parameters, $p(\theta)$.Our priors are largely 
diffuse normals, representing relatively little prior information, producing 
behavior similar to classical fixed effect models on species 
($\beta^{(s)}_j$), port ($\beta^{(p)}_k$), and gear ($\beta^{(g)}_l$) 
parameters.Our priors on time parameters ($\beta^{(y:q)}_{m\eta}$) are modeled 
similarly to a classical random effects model, which uses the data to estimate 
a shared variance among all year-quarter interaction terms.Such a hierarchical 
prior thru time, imposes the prior information that data thru time share some 
degree of similarity, however the exact degree of similarity is not specified, 
rather the degree of similarity among time parameters is itself a parameter to 
be estimated from the data.In recent years, inference on these models has 
become faster and easier to compute through the use of computational Laplace 
approximations (Rue et al., 2009); we compute inferences on the above model in 
R (R Core Team, 2015) using the R-INLA package (Rue et al., 2013). 

## Species Composition

Applying the bayesian predictive framework to the above model gives the 
following expressions for predicted weight in each stratum,
$$p(y^*_{jklm\eta}|y) = \int\!\!\!\!\int\! \text{BB}\Big( y^*_{jklm\eta}|\theta_{jklm\eta}, \rho \Big) P\Big(\theta_{jklm\eta}, \rho | y\Big) d\theta_{jklm\eta} d\rho.$$
$p(y^*_{jklm\eta}|y)$ is computed via monte carlo integration and represents 
the model's full predictive distribution for the $j^{th}$ species' weight, in 
the $k^{th}$ port, caught with the $l^{th}$ gear, in the $\eta^{th}$ quarter, 
of year $m$.
Considering the same type of prediction across all species in a particular 
stratum allows for the calculation of predictive species composistions.
The following joint transformation of the species' predictive weights result 
in predictive species compositions, 
$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ y^*_{klm\eta}\neq 0.$$
Because the $y^*$ are random variables, and $\pi^*$ is nothing more than a 
transformation of the $y^*$, $\pi^*$ is too a random variable. 
Furthermore once inference is complete, we can easily sample these 
distributions and compute any desired moments from these samples. 
-->

<!--Trasformation-->
<!--$$p(y^*_{jklm\eta}|\bm{y}) = \int\!\!\!\!\int\! \text{BB}\Big( y^*_{jklm\eta}|\mu_{jklm\eta}, \sigma^2_{jklm\eta} \Big) P\Big(\mu_{jklm\eta}, \sigma^2_{jklm\eta} | \bm{y}\Big) d\mu_{jklm\eta} d\sigma^2_{jklm\eta}$$-->
<!--$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ \bm{y}^*_{klm\eta}\neq \bm{0}$$-->

<!--$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ y^*_{klm\eta}\neq 0$$-->


<!--
Integrations
\begin{align*}
        \lambda^*_{jklm\eta\omega} &=\lambda_{\cdot k l m \eta \omega}\pi^*_{jklm\eta\omega}\\[10pt]
        \lambda^*_{jklm\eta\cdot} &=\sum_{\omega}\lambda^*_{jklm\eta\omega}\\
        \lambda^*_{jklm\cdot\cdot} &=\sum_{\eta}\sum_{\omega}\lambda^*_{jklm\eta\omega}\\
        \lambda^*_{j\cdot lm\cdot\cdot} &=\sum_{k}\sum_{\eta}\sum_{\omega}\lambda^*_{jklm\eta\omega}\\
        \lambda^*_{j\cdot\cdot m\cdot\cdot} &=\sum_{l}\sum_{k}\sum_{\eta}\sum_{\omega}\lambda^*_{jklm\eta\omega}
\end{align*}
-->



<!-- 
With the added ability to automate Bayesian model exploration, we will explore
the space of pooled models to obtain quantitative evidence of optimal pooling behavior in space.
%Furthermore as resources allow, model exploration could easily extend across any
%other difficult modeling decisions which may represent significant
%sources of model uncertainty. 
-->

<!--
The straight-forward spatial model implied by the categorical port complex 
variables do not adequatly resolve in-sample prediction at the observed 
sample sizes. 
Presently these deminishingly small within stratum sample sizes are managed 
by an ad-hoc "borrowing" protocol outlined by Pearson and Erwin (1997).
We aim to formalize this "borrowing" idea via an exhaustive search of spatially 
pooled models, combined with the formalized process of Bayesian Model 
Averaging (BMA) to appropriatly integrate port-complex pooling model 
uncertainty into species composition estimates (Hoeting et al. , 1999).


The space of possible pooled models is well defined in terms of the size of 
the set of items to be partitioned, $K$, as described by the Bell numbers 
($B_K$), 
$$B_K=\sum_{\hat k=0}^{K} \frac{1}{\hat k!} \left( \sum_{j=0}^{\hat k} (-1)^{\hat k -j} \left(\substack{\hat k \\ j}\right) j^K \right).$$
The most straight-forward solution in the presence of this type of model 
uncertainty is to compute all $B_K$ possible pooling schemes.
However, maybe not all pooling schemes represent biologically relevant models.
For example, perhaps it is reasonable to pool only among adjacent ports, or to 
assert that biologically similar regions can only extend across a small number
of ports (if so, how many?).

Each of these hypotheses are easily represented as subsets of the total model 
space, $B_K$, as seen in Figure (2). An exhaustive search of the models in 
these subspaces, and a comparison of the relative predictive accuracy of each 
model, provides concrete quantitative support for, or against, each of these 
hypotheses. Through this technique of exhaustive search and measuring relative 
predictive accuracy, we are able to understand the system to a greater degree 
than before possible. Furthermore such an exhaustive search of these model 
spaces allows for even more accurate estimates of species composition, and 
uncertainty, through the use of Bayesian Model Averaging (BMA) among the 
candidate models.
Bayesian model averaging allows us to account for model uncertainty around 
these difficult modeling decisions, while combining the respective predictive 
capabilities of each model of a given subset of model space 
(Hoeting et al., 1999). 
Once all of the models of a given model space are computed, combining them to 
account for model uncertainty, through BMA, requires trivial computation time, 
but adds substantial robustness to our predictions.

BMA

Bayesian model averaging is straight forward. For the $\mu^{th}$ model, of 
model space $\mathbb{M}$, a straight forward implementation of Bayes theorem 
gives, 
$$Pr(\mathbb{M}_\mu|y) = \frac{ p(y|\mathbb{M}_\mu)p(\mathbb{M}_\mu) }{ \sum_\mu p(y|\mathbb{M}_\mu)p(\mathbb{M}_\mu) } = \omega_\mu$$
Where $\omega_\mu$ is the posterior probability that model $\mu$ is the true 
data generating model of the data, conditional on the subspace of candidate 
models and the observed data. $\omega_\mu$ is then straightforwardly used to 
average together the posteriors of all of the candidate models, as follows
$\bar p(\theta|y) = \sum_{\mu} \omega_\mu p(\theta|y, \mathbb{M}_\mu).$
-->


<!--
writes, ``Over-dispersion is not uncommon in practice. In fact, 
some would maintain that over-dispersion is the norm in practice and nominal 
dispersion the exception.''
McCullagh P. & Nelder, J.A. (1989) Generalized Linear Models, 2nd ed. London: Chapman and Hall.
https://onlinecourses.science.psu.edu/stat504/node/162
page 124
-->
\begin{thebibliography}{1}

%
\bibitem{gelman} Gelman, A., Carlin, J. B., Stern, H. S., \& Rubin, D. B. (2014). Bayesian data analysis (Vol. 2). Boca Raton, FL, USA: Chapman \& Hall/CRC.

%
\bibitem{bma} Hoeting, J. A., Madigan, D., Raftery, A. E., \& Volinsky, C. T. (1999). Bayesian model averaging: a tutorial. Statistical science, 382-401.

%
\bibitem{glmBook} McCullagh P. \& Nelder, J.A. (1989). Generalized Linear Models, 2nd ed. London: Chapman and Hall.

%
\bibitem{pearsonErwin} Pearson, D.E., and Erwin, B. (1997). Documentation of California’s commercial market sampling data entry and expansion programs. NOAA Tech Memo. NOAA-TM-NMFS-SWFSC-240.

%
\bibitem{rCoreTeam} R Core Team (2015). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/.

%
\bibitem{inlaPackage} Rue H., Martino S., Lindgren F., Simpson D., Riebler A. (2013). R-INLA:
Approximate Bayesian Inference using Integrated Nested Laplace
Approximations. Trondheim, Norway. URL http://www.r-inla.org/.

%
\bibitem{inlaMethod} Rue, H., Martino, S., \& Chopin, N. (2009). Approximate Bayesian
inference for latent Gaussian models by using integrated nested Laplace
approximations. Journal of the royal statistical society: Series b
(statistical methodology), 71(2), 319-392.

%
\bibitem{senMemo} Sen, A.R. (1984). Sampling commercial rockfish landings in California. NOAA Tech Memo. NOAA-TM-NMFS-SWFSC-45.

%
\bibitem{senPaper} Sen AR. (1986). Methodological problems in sampling commercial rockfish landings. Fish Bull. 84: 409-421 .

%
\bibitem{sheltonEtAl} Shelton, A. O., Dick, E. J., Pearson, D. E., Ralston, S., \& Mangel, M. (2012). Estimating species composition and quantifying uncertainty in multispecies fisheries: hierarchical Bayesian models for stratified sampling protocols with missing data. Canadian Journal of Fisheries and Aquatic Sciences, 69(2), 231-246.

\end{thebibliography}









<!--
![Complexity](complexity.png)
![Integrate into text; add caption; combine with above graph](samples.png)

NOTE: Lots of stratum: market category, port, gear, year, qtr. Data becoming more and more sparse. reframe figures.

In the present sample-based methodology, data is required in each unique stratification of the system to obtain any estimate.
Given the complex, and numerous, stratification of this system, many stratum do not have observations, and thus inference is not possible without some sort of stratum pooling.
Pooling data will naturally increase the bias of the resulting estimates, but when no other data is available, a slightly biased estimate is better than no estimate at all.
In the current state of affairs, a "historically" derived set of pooling rules have been implemented across ports when no other data is available.
The "historical" pooling rules are largely based on personal experience with the sampling process, although quantifiable evidence for the implemented rules have been hard to produce until recently. 
-->
<!--
 for the implemented pooling rules among ports are hard to explicitly quantify.
are largely based on personal experience with the sampling process.
based on designed to theoretically minimize any bias introduced thru pooling ports,
-->
<!--
Rather than relying so heavily on intangible pooling rules which change based on the availablity of samples. 
We hope to standardize any necessary spatial pooling thru an exhaustive search of the space of pooled models.

# Technical Project Plan

## Model

For a particular market category, $y_{ijklm\eta}$ is the  $i^{th}$ sample of the $j^{th}$ species’ weight, in the $k^{th}$ port, caught with the $l^{th}$ gear, in the $\eta^{th}$ quarter, of year $m$.
In accordance with the typical model based statistical procedure, the $y_{ijklm\eta}$ are said to be observations from a statistical distribution $f$ conditional on some parameters $\theta$ and $\rho$.
$$y_{ijklm\eta} \sim f(y_{ijklm\eta}|\theta, \rho).$$
Given observed overdispersion relative to the Poisson and Binomial distributions, we have found the Beta-Binomial distribution to perform well as an observation model for these data.
The linear predictor parameters, $\theta$, are then factored as follows among the many stratum, 
$$\theta_{jklm\eta} = \beta_0 + \beta^{(s)}_j + \beta^{(p)}_k + \beta^{(g)}_l + \beta^{(y:q)}_{m\eta}.$$
$\rho$ is a correlation parameter among the stratum, which gives the Beta-Binomial added flexibility for modeling overdisperse data.

As a Bayesian model, we specify any information external to the dataset, thru our priors on the parameters, $p(\theta)$.
Our priors are largely diffuse normals, representing relatively little prior information, producing behavior similar to classical fixed effect models on species ($\beta^{(s)}_j$), port ($\beta^{(p)}_k$), and gear ($\beta^{(g)}_l$) parameters.
Our priors on time parameters ($\beta^{(y:q)}_{m\eta}$) are modeled similarly to a classical random effects model, which uses the data to estimate a shared variance among all year-quarter interaction terms.
Such a hierarchical prior thru time, imposes the prior information that data thru time share some degree of similarity, however the exact degree of similarity is not specified, rather the degree of similarity among time parameters is itself a parameter to be estimated from the data.

In the past such models have been difficult to fit, due to relatively slow, unparallelizable, Markov Chain Monte Carlo (MCMC) sampling methods of inference.
In recent years, inference on these models has become faster thru the use of computational Laplace approximations (Rue et al., 2009), as distributed thru the R-INLA package (Rue et al., 2013).
Of primary note, the INLA method of inference is largely parallelizable, and does not rely on the subjective process of determining Markov chain convergence.
Together the parallelizable and "hands-off" nature of INLA inference allows for automated model exploration.

## Model Exploration
-->
<!--, or even beyond spatial parameters, into other stratum, or other difficult modeling decisions, as resources allow.-->
<!--
With the added ability to automate Bayesian model exploration, we desire to explore the space of pooled models with the hope of obtaining quantitative evidence of optimal pooling behavior in space.
Furthermore as resources allow, model exploration could easily extend across any other difficult modeling decisions which may represent significant sources of model uncertainty.
The space of possible pooled models is well defined in terms of the size of the set of items to be partitioned, $K$, as described by the Bell numbers ($B_K$),
$$B_K=\sum_{\hat k=0}^{K} \frac{1}{\hat k!} \left( \sum_{j=0}^{\hat k} (-1)^{\hat k -j} \left(\substack{\hat k \\ j}\right) j^K \right).$$
The most straight-forward solution in the presence of this type of model uncertainty is to simply compute all $B_K$ possible pooling schemes.
However, practically speaking, not all pooling schemes necessarily represent biologically relevant models.
For example, perhaps it is reasonable to pool only among adjacent ports (perhaps not), similarly it may be reasonable to assert that biologically similar regions can only possibly extent across a relatively small number of ports (if so, how many?).

Each of these hypotheses are easily represented as subsets of the total model space, $B_K$, as seen in Figure (1).
An exhaustive search of the models in these subspaces, and a comparison of the relative predictive accuracy of each model, provides concrete quantitative support for, or against, each of these hypotheses.
Thru this technique of exhaustive search and measuring relative predictive accuracy, we are able to understand the system to a greater degree than before possible. 
Furthermore such an exhaustive search of these model spaces allows for even more accurate estimates of species composition, and uncertainty, through the use of Bayesian model averaging among the candidate models.
Bayesian model averaging allows us to account for model uncertainty around these difficult modeling decisions, while combining the respective predictive capabilities of each model of a given subset of model space.
-->
<!--
![$B_k$ represents the total number of models in the model space of pooled models among $K$ ports in a single market category. The colored lines represent different pooling hypotheses, which represent subsets of the models in $B_K$ and show how model exploration would scale with each hypothesis.](scaling.pdf)
-->
<!--
Once all of the models of a given model space are computed, combining them to account for model uncertainty thru Bayesian model averaging is straight forward.
For the $\mu^{th}$ model, of model space $\mathbb{M}$, a straight forward implementation of Bayes theorem gives,
$$Pr(\mathbb{M}_\mu|y) = \frac{ p(y|\mathbb{M}_\mu)p(\mathbb{M}_\mu) }{ \sum_\mu p(y|\mathbb{M}_\mu)p(\mathbb{M}_\mu) } = \omega_\mu$$
Where $\omega_\mu$ is the posterior probability that model $\mu$ is the true data generating model of the data, conditional on the subspace of candidate models and the observed data.
$\omega_\mu$ is then straightforwardly used to average together the posteriors of all of the candidate models, as follows
$$\bar p(\theta|y) = \sum_{\mu} \omega_\mu p(\theta|y, \mathbb{M}_\mu).$$

## Scalability

The above described system is already built and running on 2x12 core processors.
As a relatively small scale test we have considered the directly adjacent pooling possibilities among 5 port complexes at a time, along the coast of California (10 total port complexes), for all market categories.
Thus we were able to compute $26~categories \times \frac{16 models}{category} = 416~models$ in approximately a month of wall clock time running with 24 fold parallelism.
Since this work is almost entirely trivially parallelizable we have observed near linear speedup as we have made the code mode parallel within our current capabilities.

Given our current computational resources it is not practically feasible to attempt explorations that involve many more models than we have already accomplished.
However with access to more parallel infrastructure we foresee the ability to scale our current code with little modification.
The relevant variables determining run time are the size of the data, the number of parameters per model, and total number of models to consider.
Since the data and the maximum number of parameters per model are constants for a given $K$, and for large $K$, work is overwhelmingly dominated by the number of possible candidate models (models are trivially parallelizable), we believe that scaling will not be difficult as the number of available processors increases.
-->
<!-- which is an axis of work which is trivially parallelizable-->


<!--
Model Averaging
$$\omega_j = Pr(\mathbb{M}_j|y) = \frac{ p(y|\mathbb{M}_j)p(\mathbb{M}_j) }{ \sum_i p(y|\mathbb{M}_i)p(\mathbb{M}_i) }$$ 
$$\bar p(\theta|y) = \sum_{j=1}^{\mathcal{M}} \omega_j p(\theta|y, \mathbb{M}_j)$$
if $f$ only depends on $\mathbb{M}$ thru $\theta$, then
$$\bar p(y^*|y) = \int f(y^*|\theta) \bar p(\theta|y) d\theta$$
-->
<!--
# References

Rue, H., Martino, S., & Chopin, N. (2009). Approximate Bayesian inference for latent Gaussian models by using integrated nested Laplace approximations. Journal of the royal statistical society: Series b (statistical methodology), 71(2), 319-392.

Rue  H,  Martino  S,  Lindgren  F,  Simpson  D,  Riebler  A  (2013). R-INLA: Approximate Bayesian Inference using Integrated Nested Laplace Approximations. Trondheim, Norway. URL http://www.r-inla.org/.

* [Stategic Plan](http://www.cio.noaa.gov/it_plans/HPCStrategy_Final_Draft_080913.pdf)
	* "Typical Use": Weather and Climate
	* remove destinction between "operational" and "research" HPC
	* Understanding Ecosystems & Coastal Issues

* [AWS](https://aws.amazon.com/ec2/pricing/on-demand/)
	* AWS GovCloud(US)
	* How parallel?
		* vCPU v. ECU
	* Utility per dollar?

* Infrastructure: [Inteligent Decisions](https://store.intelligent.net/DOC/)
	* How parallel? 
	*Utility per dollar?

-->







<!--
Our computational approach to model exploration is easily parallelized, but requires 
computing resources beyond our laboratory’s current capabilities. 
Adequate exploration of the plausible model space will require additional 
resources to produce results in a timeframe that is useful to managers and NOAA 
Fisheries scientists.
-->

<!--
# Abstract

In order to effectively manage exploited populations, accurate estimates of commercial fisheries catches are necessary to inform monitoring and assessment efforts.
In California, the high degree of heterogeneity in the species composition of many groundfish fisheries, particularly those targeting rockfish (Sebastes), leads to challenges in sampling all market categories, or species, adequately. 
Limited resources and increasingly complex stratification of the sampling system inevitably leads to gaps in sample data. 
In the presence of sampling gaps, ad-hoc species composition point estimation is currently obtained according to historically derived “data borrowing” protocols which do not allow for uncertainty estimation or forecasting. 
In order to move from the current ad-hoc "data-borrowing" design-based point estimators, we have constructed Bayesian hierarchical models to estimate species compositions, complete with accurate measures of uncertainty, as well as theoretically sound out-of-sample predictions.
Furthermore, we introduce a computational method for discovering consistent “borrowing” strategies across overstratified data.
Our modeling approach, along with a computationally robust system of inference and model exploration, allows us to start to understand the affect of the complexly stratified, and sparse, sampling system, on the kinds of inference possible, while simultaneously making the most from the available data.
-->

<!--
*Our  results  indicate  that this  approach  is  likely  to  be  more  robust  than  the  current  system,  particularly  in  the  face  of  sparse  sampling. 
Additionally,  our  method  should  also  help inform,  and prioritize,  future  sampling  efforts.
Perhaps  more  significantly,  this  approach  provides estimates of uncertainty around species-specific catch estimates.*
-->

<!--
Bayes Theorem
$$p(\theta|y) = \frac{f(y|\theta)p(\theta)}{p(y)}$$

INLA, parallelizable numerical inference, and parallel posterior sampling

Complete Predictions (point estimate + uncertainty)
$$p(y^*|y) = \int f(y^*|\theta) p(\theta|y) d\theta$$
-->

<!--

In the present sample-based methodology, data is required in each unique stratification of the system to obtain any inference.
Given the complex, and numerous, stratification of this system, many stratum do not have observations, and thus inference is not possible with out some sort of pooling of stratum.
Pooling data will naturally increase the bias of the resulting estimates, but when no other data is available a slightly biased estimate is better than no estimate at all.
In the current state of affairs, a "historically" derived set of pooling rules have been implemented across ports, so as to theoretically minimize any biased introduced thru pooling ports, the basis for which ports are similar is hard to explicitly quantify as it is largely based on personal experience.

as a function of the number of discrete items to be pooled $K$.
The total number to be considered is 


-->
