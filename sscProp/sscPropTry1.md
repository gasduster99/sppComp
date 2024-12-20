---
title: Improving Catch Estimation Methods in Sparsely Sampled Mixed-Stock Fisheries.
author: Nick Grunloh, Edward Dick, Don Pearson, John Field, Marc Mangel
date: Aug 15, 2017
---

# Abstract

Effective management of exploited fish populations, requires accurate estimates
of commercial fisheries catches to inform monitoring and assessment efforts. In
California, the high degree of heterogeneity in the species composition of many
groundfish fisheries, particularly those targeting rockfish (genus $Sebastes$),
leads to challenges in sampling all potential strata, or species, adequately.
Limited resources and increasingly complex stratification of the sampling
system inevitably leads to gaps in sample data. In the presence of sampling
gaps, ad-hoc species composition point estimation is currently obtained
according to historically derived “data borrowing” (imputation) protocols which
do not allow for uncertainty estimation or forecasting. In order to move from
the current ad-hoc “data-borrowing” point estimators, we have constructed
Bayesian hierarchical models to estimate species compositions, complete with
accurate measures of uncertainty, as well as theoretically sound out-of-sample
predictions. Furthermore, we introduce a computational method for discovering
consistent “borrowing” strategies across over-stratified data. Our modeling
approach, along with a computationally robust system of inference and model
exploration, allows us to start to understand the effect of the highly
stratified, and sparse, sampling system on the kinds of inference possible,
while simultaneously making the most from the available data.

# Significance

In order to understand how fish populations respond to fishing, it is critical
to obtain accurate estimates of how many fish are removed from the ocean
(catch) and to quantify the precision of those estimates. Traditionally,
population dynamics models used to measure this response to fishing (“stock
assessments”) are conditioned on a time series of annual catches. These catch
estimates are often treated as being known without error, despite the fact that
they are derived from sampling programs that estimate the proportion of
different species found within multiple sampling strata. Sampling error
introduces uncertainty into estimates of the catch, and unsampled strata must
be “filled in” through a process sometimes referred to on the U.S. West Coast
as “borrowing” (i.e. data imputation). Historically, methods used to “borrow”
information among strata have been ad-hoc in nature and driven by expert
opinion of local managers (Sen et al. 1984, 1986) (Pearson and Erwin 1997). We
seek to improve upon this practice through development of a model-based
approach that provides estimates of catch and associated uncertainty, as well
as an objective, defensible framework for model selection and data imputation.
Although the theoretical basis for a model based estimation of species
composition in mixed stock fisheries has been advanced (Shelton et al., 2012),
it has not yet been implemented successfully using actual historical or
contemporary data.

The difficulties associated with the existing ad-hoc approach are magnified by
an increase in the number of sampling strata over time, specifically the number
of “market categories,” into which fishermen and dealers sort their catch
(Figure 1, Bottom). The increase in the number of market categories (sampling
strata) has not been matched by increases in sampling effort, resulting in a
decline in the average number of samples per stratum (Figure 1, Middle). In
other words, data are becoming more sparse, increasing our uncertainty in
estimates of catch. Since the data are also stratified over a number of ports,
fishing gear types, years, and quarters, inference is not possible without some
sort of stratum pooling. Rather than rely so heavily on the previous, ad-hoc
pooling rules which change based on the availability of samples, we hope to
standardize any necessary pooling through an exhaustive search of the space
(possible configurations) of pooled models. Pooling (and partial pooling) among
strata is achieved using Bayesian hierarchical statistical models and model
averaging (Gelman et al., 2014).

# Methods

## Model

For a particular market category, $y_{ijklm\eta}$ is the $i^{th}$ sample
of the $j^{th}$ species' weight, in the $k^{th}$ port, caught with the
$l^{th}$ gear, in the $\eta^{th}$ quarter, of year $m$.
The $y_{ijklm\eta}$ are said to be observations from a Beta-Binomial
distribution ($BB$) conditional on parameters $\theta$ and $\rho$.

<!--$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\bm{\theta}, \rho).$$-->
$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\theta, \rho).$$

Given observed overdispersion relative to the Poisson and Binomial
distributions, the Beta-Binomial model makes use of a correlation parameter,
$\rho$, to better model uncertainties. The linear predictor parameters, 
$\theta$, are then factored as follows among the many strata,

$$\theta_{jklm\eta} = \beta_0 + \beta^{(s)}_j + \beta^{(p)}_k + \beta^{(g)}_l + \beta^{(y:q)}_{m\eta}.$$

Our priors are largely diffuse, representing relatively little prior
information, producing behavior similar to classical fixed effect models on
species ($\beta^{(s)}_{j}$), port ($\beta^{(p)}_{k}$), and gear ($\beta^{(g)}_{l}$) 
parameters. Our priors on time parameters ($\beta^{(y:q)}_{m\eta}$) are normal 
distributions cenetered at zero with a hierarchical variance shared among all 
year-quarter interaction terms. In recent years, inference on these models has 
become faster and easier to compute through the use of computational Laplace 
approximations (Rue et al., 2009); we compute inferences on the above model in 
R (R Core Team, 2015) using the R-INLA package (Rue et al., 2013).

# Species Composition

Applying the Bayesian predictive framework to the above model gives the
following expressions for predicted weight in each stratum, 

$$p(y^*_{jklm\eta}|y) = \int\!\!\!\!\int\! \text{BB}\Big( y^*_{jklm\eta}|\theta_{jklm\eta}, \rho \Big) P\Big(\theta_{jklm\eta}, \rho | y\Big) d\theta_{jklm\eta} d\rho.$$

$p(y^*_{jklm\eta}|y)$ is computed via monte carlo integration and represents
the model's full predictive distribution for the $j^{th}$ species' weight, in 
the $k^{th}$ port, caught with the $l^{th}$ gear, in the $\eta^{th}$ quarter, 
of year $m$. The following joint transformation of the species' predictive
weights result in predictive species compositions, 

$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ y^*_{klm\eta}\neq 0.$$

Because the $y^*$ are random variables, and $\pi^*$ is nothing more than a 
transformation of the $y^*$, $\pi^*$ is also a random variable. Furthermore 
once inference is complete, we can easily sample these distributions and 
compute any desired moments from the samples.

# Model Exploration \& Averaging

The straight-forward spatial model implied by the typical categorical port-complex
variables do not adequately make in-sample predictions at the observed sample
sizes. Presently the vanishingly small within-stratum sample sizes are
managed by an ad-hoc "borrowing" protocol outlined by Pearson and Erwin (1997).
 
We aim to formalize the idea of "borrowing" via an exhaustive search of
spatially pooled models among port-complexes. We combine this exhaustive search
of the set of possible pooled models, with the formalized process of Bayesian 
Model Averaging (BMA) (Hoeting et al., 1999), to integrate model uncertainty
about port groupings into species composition estimates. BMA both captures 
the model uncertainty around port pooling into species composition estimates, 
while pinning the relative predictive accuracy of each model against other 
models, in the set of possible pooled models, to discover optimal port 
super-complexes in each market category.



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
\bibitem{sheltonEtAl} Shelton, A. O., Dick, E. J., Pearson, D. E., Ralston, S., \& Mangel, M. (2012). Estimating species composition and quantifying uncertainty in multispecies fisheries: hierarchical 
Bayesian models for stratified sampling protocols with missing data. Canadian Journal of Fisheries and Aquatic Sciences, 69(2), 231-246.

\end{thebibliography}


