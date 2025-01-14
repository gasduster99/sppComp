---
title: Improving Catch Estimation Methods in Sparsely Sampled Mixed-Stock Fisheries.
author: Nick Grunloh, Edward Dick, Don Pearson, John Field, Marc Mangel
date: Aug 15, 2017
---

<!--
# Title Page

* how the methodology will improve assessment/management
  * EJs section from introduction
* Outline Methods
  * Data Prep
    * Define Sample
  * Model
    * Overdispersion Evidence
    * State Model
    * Species Comp. Calculation
  * Model Averaging
  * Performance
-->

# Abstract

Effective management of exploited fish populations, requires accurate estimates 
of commercial fisheries catches to inform monitoring and assessment efforts. 
In California, the high degree of heterogeneity in the species composition of 
many groundfish fisheries, particularly those targeting rockfish 
(genus Sebastes), leads to challenges in sampling all potential strata, or 
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
across over-stratified data.
Our modeling approach, along with a computationally robust system of inference 
and model exploration, allows us to start to understand the effect of the 
highly stratified, and sparse, sampling system on the kinds of inference 
possible, while simultaneously making the most from the available data. 

# Significance

In order to understand how fish populations respond to fishing, it is critical 
to obtain accurate estimates of how many fish are removed from the ocean 
(catch) and to quantify the precision of those estimates.
Traditionally, population dynamics models used to measure this response to 
fishing (“stock assessments”) are conditioned on a time series of annual 
catches.
These catch estimates are often treated as being known without error, despite 
the fact that they are derived from sampling programs that estimate the 
proportion of different species found within multiple sampling strata.
Sampling error introduces uncertainty into estimates of the catch, and 
unsampled strata must be “filled in” through a process sometimes referred 
to on the U.S. West Coast as “borrowing” (i.e. data imputation).
Historically, methods used to “borrow” information among strata have been 
ad-hoc in nature and driven by expert opinion of local managers 
(Sen et al. 1984, 1986) (Pearson and Erwin 1997).
We seek to improve upon this practice through development of a model-based 
approach that provides estimates of catch and associated uncertainty, as well 
as an objective, defensible framework for model selection and data imputation.
Although the theoretical basis for a model based estimation of species 
composition in mixed stock fisheries has been advanced (Shelton et al., 2012), 
it has not yet been implemented successfully using actual historical or 
contemporary data.

![Spase Data](sampleComplex.png)

The difficulties associated with the existing ad-hoc approach are magnified by 
an increase in the number of sampling strata over time, specifically the 
number of “market categories,” into which fishermen and dealers sort their 
catch (Figure 1, Bottom).
The increase in the number of market categories (sampling strata) has not been 
matched by increases in sampling effort, resulting in a decline in the average 
number of samples per stratum (Figure 1, Middle).
In other words, data are becoming more sparse, increasing our uncertainty in 
estimates of catch.
Since the data are also stratified over a number of ports, fishing gear types, 
years, and quarters, inference is not possible without some sort of stratum 
pooling.
Rather than rely so heavily on the previous, ad-hoc pooling rules which change 
based on the availability of samples, we hope to standardize any necessary 
pooling through an exhaustive search of the space (possible configurations) of 
pooled models.
Pooling (and partial pooling) among strata is achieved using Bayesian 
hierarchical statistical models and model averaging (Gelman et al., 2014).

# Methods
## Model

<!--data talk-->
For a particular market category, $y_{ijklm\eta}$ is the $i^{th}$ sample
of the $j^{th}$ species' weight, in the $k^{th}$ port, caught with the
$l^{th}$ gear, in the $\eta^{th}$ quarter, of year $m$.
The $y_{ijklm\eta}$ are said to be observations from a Beta-Binomial 
distribution ($BB$) conditional on parameters $\theta$ and $\rho$.

<!--$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\bm{\theta}, \rho).$$-->
$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\theta, \rho).$$

Given observed overdispersion relative to the Poisson and Binomial 
distributions, the Beta-Binomial model makes use of a correlation parameter, 
$\rho$, to better model uncertainties, while maintaining a flexible model on 
stratum means through the linear predictor. 
The linear predictor parameters, $\theta$, are then factored as follows 
among the many strata,

$$\theta_{jklm\eta} = \beta_0 + \beta^{(s)}_j + \beta^{(p)}_k + \beta^{(g)}_l + \beta^{(y:q)}_{m\eta}.$$

As a Bayesian model, we specify any information external to the dataset, 
through our priors on the parameters, $p(\theta)$.
Our priors are largely diffuse normals, representing relatively little prior 
information, producing behavior similar to classical fixed effect models on 
species ($\beta^{(s)}_j$), port ($\beta^{(p)}_k$), and gear ($\beta^{(g)}_l$) 
parameters.
Our priors on time parameters ($\beta^{(y:q)}_{m\eta}$) are modeled similarly 
to a classical random effects model, which uses the data to estimate a shared 
variance among all year-quarter interaction terms.
Such a hierarchical prior thru time, imposes the prior information that data 
thru time share some degree of similarity, however the exact degree of 
similarity is not specified, rather the degree of similarity among time 
parameters is itself a parameter to be estimated from the data.
In recent years, inference on these models has become faster and easier to 
compute through the use of computational Laplace approximations 
(Rue et al., 2009); we compute inferences on the above model in R 
(R Core Team, 2015) using the R-INLA package (Rue et al., 2013). 

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

## Model Exploration \& Averaging

<!-- 
With the added ability to automate Bayesian model exploration, we will explore
the space of pooled models to obtain quantitative evidence of optimal pooling behavior in space.
%Furthermore as resources allow, model exploration could easily extend across any
%other difficult modeling decisions which may represent significant
%sources of model uncertainty. 
-->

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



\begin{thebibliography}{1}

%
\bibitem{gelman} Gelman, A., Carlin, J. B., Stern, H. S., \& Rubin, D. B. (2014). Bayesian data analysis (Vol. 2). Boca Raton, FL, USA: Chapman \& Hall/CRC.

%
\bibitem{bma} Hoeting, J. A., Madigan, D., Raftery, A. E., \& Volinsky, C. T. (1999). Bayesian model averaging: a tutorial. Statistical science, 382-401.

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
