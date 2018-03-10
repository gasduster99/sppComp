---
title: Improving Catch Estimation Methods in Sparsely Sampled Mixed-Stock Fisheries.
author: Nick Grunloh, E.J. Dick, Don Pearson, John Field, Marc Mangel
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
is currently obtained according to historically derived data borrowing
(imputation) protocols which do not allow for uncertainty estimation or 
forecasting. In order to move from the current ad-hoc data-borrowing
point estimators, we have constructed Bayesian hierarchical models to estimate 
species compositions, complete with accurate measures of uncertainty, as well 
as theoretically sound out-of-sample predictions. Furthermore, we introduce a 
computational method for discovering consistent borrowing strategies 
across over-stratified data. Our modeling approach, along with a 
computationally robust system of inference and model exploration, allows us to 
start to understand the effect of the highly stratified, and sparse, sampling 
system on the kinds of inference possible, while simultaneously making the 
most from the available data. 
-->

<!--
# Significance

In order to understand how fish populations respond to fishing, it is critical 
to obtain accurate estimates of how many fish are removed from the ocean 
(catch) and to quantify the precision of those estimates. Traditionally, 
population dynamics models used to measure this response to fishing (stock 
assessments) are conditioned on a time series of annual catches. These catch 
estimates are often treated as being known without error, despite the fact 
that they are derived from sampling programs that estimate the proportion of 
unsampled strata must be filled in through a process sometimes referred 
to on the U.S. West Coast as borrowing (i.e. data imputation). Historically, 
methods used to borrow information among strata have been different species 
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
number of market categories, into which fishermen and dealers sort their 
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

Effective management of exploited fish populations requires accurate
estimates of commercial fisheries catches to inform monitoring and
assessment efforts. In California, the high degree of heterogeneity in
the species composition of many groundfish fisheries, particularly those
targeting rockfish (genus $Sebastes$), leads to challenges in sampling all
potential strata, or species, adequately. Limited resources and
increasingly complex stratification of the sampling system inevitably
leads to gaps in sample data. In the presence of sampling gaps, ad-hoc
species composition point estimation is currently obtained according to
historically derived "data borrowing" (imputation) protocols which do
not allow for uncertainty estimation or forecasting. In order to move
from the current ad-hoc "data-borrowing" point estimators, we have
constructed Bayesian hierarchical models to estimate species
compositions, complete with accurate measures of uncertainty, as well as
theoretically sound out-of-sample predictions. Furthermore, we introduce
a Bayesian model averaging approach for inferring spatial pooling 
strategies across the over-stratified port sampling system. Our modeling 
approach, along with a computationally robust system of inference and model 
exploration, allows us to 1) quantify uncertainty in historical landings, and 
2) understand the effect of the highly stratified, and sparse, sampling system 
on the kinds of inference possible, while simultaneously making the most from 
the available data.

# Introduction

Estimates of landed catch are a key component of many fishery management 
systems. Stock assessment models (referred to here as assessments) are often 
conditioned on time series of annual catch, usually under the assumption that 
catches are known without error. While some assessment models are able to 
incorporate uncertainty in catch (e.g. Stock Synthesis; Methot and Wetzel, 2013), 
reliable estimates of catch uncertainty are often unavailable. Without this 
information, assessment authors often rely on ad-hoc sensitivity analyses 
which may or may not be incorporated into management advice and/or fail to 
propagate catch uncertainty into quantities of interest to managers.

Over the past decade, the estimation of catch and associated uncertainty has 
become a focus for recreational fisheries in the United States (NAS, 2017). 
Commercial fisheries, on the other hand, are often assumed to have precise 
estimates of catch by species. This is due in part to the availability of 
landing receipts (aka fish tickets) which serve as a record of the weight of 
fish landed into various market categories (sort groups). As noted by 
Pearson et al. (2008), it is important to recognize that species and market categories 
are not synonymous. On the U.S. West Coast, for example, it is common for 
multiple species to be landed within a single market category 
(CALCOM 2017, PacFIN 2017). This is expected for categories that are clearly 
designated as mixed-species categories (e.g. ``nearshore rockfish'', or species 
within a particular genus or family). However, in some states, categories that 
are named after a single species still contain several species, to varying 
degrees, even after regulations require sorting into a particular 
category (Pearson et al., 2008). As a result, estimation of landings for a 
single species based on landing receipt data alone may produce biased 
estimates of total catch. 

Fisherman, dealers, or processors typically decide how to sort species into 
market categories on a landing receipt. Trained port samplers intercept 
vessels offloading catch or during subsequent processing in order to determine 
the species composition of catch landed in a given market category (Sen 1984, 
Crone 1995, Tsou et al. 2015).  These species composition data are used to 
partition, or distribute, the weight of landed catch in a market category 
across species, a process commonly referred to as catch expansion (Pearson and 
Almany, 1995). To estimate total landings for a single species, the expanded 
catch is summed across all relevant market categories. Assuming that landing 
receipt data are a census of the landed catch, uncertainty in total catch for 
a given species reflects variability in the species composition among port 
samples. If information is available to estimate sources of bias in the 
landing receipt data, e.g.  underreporting, then it is possible to also 
incorporate uncertainty in the bias correction factors as well 
(Bousquet et al. 2010). In this study, we quantify sampling uncertainty for 
estimates of landed catch, although the modeling framework could be extended 
to include uncertainty or bias in landing receipt data.

Within market categories, particularly those used historically for groupings
of highly speciose rockfish (Sebastes spp), the species composition of landed 
catch can vary spatially, temporally, by fishing gear, and catch disposition 
(e.g. fish sold alive or dead). These differences are attributable to many 
factors, including market preference, fishing behavior, regulatory constraints
, and biological/ecological characteristics (e.g. spatial distribution) 
of the landed species. As a result, estimates of species composition for 
a given market category are often stratified over time (e.g. quarterly) 
and across other relevant strata (e.g. ports, gears, catch disposition
).  Sampling programs often have limited funds, and attempts to reduce bias 
in species composition estimates through the introduction of additional 
strata comes at a cost, namely reduced precision (Cochran, 1977; Tomlinson, 
1971).  

On the U.S. West Coast, port sampling programs for rockfish and other 
groundfish allocate effort both spatially and temporally, but many domains of 
interest (e.g. market category, gear type, catch disposition) remain unsampled 
or sparsely sampled due to a proliferation of categories over time, logistical 
constraints, and limited resources (Sen 1986; Crone 1995; Pearson et al. 2008; 
Tsou et al. 2015).  In California, for example, commercial port sampling 
effort has changed over time and space (Pearson and Almany 1995). For example, 
regular sampling of California ports north of Point Conception (roughly 
$34^{\circ}$ 27' N. latitude) began as early as 1978, but the more southern ports 
were rarely sampled prior to 1983. This allocation of effort was largely based 
on the statewide distribution of landings, diffuse spatial distribution of 
southern commercial ports, and limitations in funding for port samplers.  

When no port samples are collected for landed strata and domains, species 
composition estimates are borrowed from other strata using deterministic 
algorithms based on expert opinion. These algorithms have unknown bias and 
precision. In contrast, model-based estimators are increasingly used to 
estimate quantities of interest for domains with small sample sizes and/or 
unsampled strata (sometimes referred to as small area estimation; Rao 2003). 
As a pilot study, Shelton et al. (2012) developed a Bayesian hierarchical 
statistical framework for estimating species compositions in rockfish market 
categories from trawl fisheries from a single port in California in two 
separate years. Their model has the ability to partially pool information 
among sparsely sampled strata, predicts species compositions for unsampled 
strata, and can be combined with landing receipts to estimate total landings 
by species, across market categories and other strata, along with associated 
estimates of uncertainty. However, their model considered hierarchical pooling 
only among quarters within a single year, and the authors underscored the need 
to better understand performance of alternative models, and to overcome issues 
with computation time, particularly since commercial port sampling data sets 
often include hundreds of landed strata spanning decades, multiple ports, gear 
types, and other domains of interest.

Among the U.S. West Coast states, the challenge of estimating landings for 
sparsely-sampled mixed stock rockfish fisheries is perhaps greatest for 
California. Although overall landings have historically been greater for 
rockfish off of Oregon and Washington, California has a greater number of 
commercial ports, market categories, and landed species 
(Pearson and Irwin 1997), with greater species diversity among ports due to 
the geographical range of the coast and the observation that species diversity 
for this genus is greatest in the Southern California Bight (Love et al. 2002
). California also includes two major biogeographic features, Point Conception 
and Cape Mendocino, that are associated with different physical oceanographic 
conditions and biological community assemblages (Hickey 1979, Checkley and 
Barth 2009, Gottscho  2016), and these features are also frequently used as 
spatial boundaries for stock assessments and management measures. Of 
particular consequence to the estimation of species compositions is the 
proliferation of landed market categories over time, particularly during the 
1990s (Figure (Sparse Data)). Sampling effort also leveled off in the mid-1990s, 
with a reduction in effort in the early 2000s, associated with substantial 
declines in total catches as well as reductions in sampling resources. The net 
result of increased stratification and flat (or reduced) sampling effort over 
time is a decline in mean sample size per stratum (Figure 1). In this situation
, it is critical to understand how efforts to reduce bias (e.g. increasing the 
number of landed market categories) affect precision of the expanded catch 
estimates.  

Models that take catch uncertainty into account are not new (c.f. Doubleday 
1976), but most assessments on the U.S. West Coast assume catch is known 
without error (PFMC 2018). As a result, catch uncertainty is not propagated 
into management reference points.  However, the implications of catch 
uncertainty are not limited to stock assessment efforts.  In a management 
context, catch estimates with large (but unknown) uncertainty may cause 
managers to react to large, high-frequency deviations in estimated catch, and 
either impose unnecessary restrictions on a fishery, or mistakenly support 
excessive harvest. This is particularly an issue for prohibited and/or "choke" 
species, for which information is limited and may be based solely on estimates 
of discarded catch.  

In this study, we evaluate the model-based framework proposed by Shelton et 
al. (2012) using commercial port sampling data collected in California, U.S.A. 
We describe species composition data collected by the California Cooperative 
Groundfish Survey (CCGS, 2017) over the period 1978-1990. We then extend the 
Shelton et al. framework to address limitations of their approach. 
Specifically, we evaluate alternative likelihoods to address overdispersion, 
compare multiple hierarchical structures for pooling information through time, 
and integrate model predictions across uncertainties in the spatial model 
structure. Finally, we estimate landed catch by species for both sampled and 
unsampled strata, and summarize a general framework for quantifying 
uncertainty including an efficient database design for dissemination of 
results at any level of aggregation.

![Sparce Data: Number of commercial port samples per market category in California,
1978-2014 (upper panel), average sample size per stratum (middle panel), and 
number of market categories recorded on landing receipts (lower panel). On the 
lower panels, points indicate observed values, while the black lines represent 
9 year moving averages](./pictures/sampleComplex.png)

# Methods

<!--
Originally, protocols for sampling rockfish in California were derived by Sen 
(1984, 1986) as a design-based cluster sampling. Detailed descriptions of the sampling 
program and current process for expanding species composition estimates to 
species-specific landings can be found elsewhere (Sen 1984, 1986; Pearson and 
Erwin 1997). We briefly summarize the sampling process here. Sampling follows 
a two-stage stratified sampling plan (Sen 1986). Vessels are selected 
arbitrarily by the port sampler and two 50 or 25 lb clusters of fish are 
selected arbitrarily from bin(s) containing a single market category. The mass 
sampled depended on the size of individual fish in the landings, with 
landings consisting of small fish having 25 lbs sampled and those consisting 
of large fish having 50 lbs sampled. The mass of each species in the clusters 
are recorded. The mass sampled relative to the total landing mass is highly 
variable. In some cases, the entire catch is sampled (e.g., some hook-and-line 
landings), whereas in others (e.g., trawl landings), the proportion of 
landed mass sample is very small. However, as the target population of port 
sampling is the total landed mass in each market category, the proportion of 
landed mass that is sampled is very small. 

Multiple market categories are often landed by a single boat, and samples from 
several different market categories may be collected from a given landing. The 
species present in a market category are known to vary with location (e.g.,the 
port; landings are grouped into 10 major port complexes in California that 
span ~800 miles (1 mile = ~1.61 km) of coastline), time (year and the four 
quarters within the year), fishing technique (trawl, hook and line, gillnet, 
fish pot, or other minor categories), and if the fish are landed alive or dead. 
Each combination of market category, location, time, fishing technique, and 
alive/dead comprise a stratum, and the many categories result in a very large 
number of potential strata. Annually, ~1500 strata have landings reported.
-->

## Data
As outlined in Sen (1984, 1986) the data collection protocol for the species 
composition port sampling data, held within the CALCOM database, are the result 
of a design based cluster sampling procedure across the many strata of 
California's commercial fisheries. Each sample is intended to be composed of 
two fifty pound clusters selected at random from a stratum. Although port 
samplers do their best to follow protocol, in reality the port sampling 
environment does not always allow Sen's original protocol to be followed. 
Variations in the sampling protocol may result in only a single cluster being 
taken, or the size of clusters taken to vary from stratum to stratum based on 
the particular challenges of sampling each stratum. 

Samples are recorded, by port samplers, as integer pounds for each observed 
species, across the landed market categories, gear groups, and port complexes
in time (quarters within year). Presently there are 71 rockfish market 
categories, although not all market categories are always used by fisherman. 
The number of used market categories has gone from less than 25 in 1978 to 
about 55 in $2014$. Landings are grouped into major fishing gear groups 
(trawl, hook and line, gillnet, fish pot, or other minor categories) and ten 
major port complexes spanning the California coast.

<!--
[add more detailed description of data here; e.g. samples are recorded as 
integer weights (pounds) by port samplers, number of market categories, range 
of years, gears, port complexes, ignoring live fish for now, etc.; show map of 
state with port complexes.]
-->

The model based methodology proposed here does not rely strongly upon the 
cluster sampling structure, but rather simply views each sample as independent 
and identically distributed ($i.i.d.$) draws from a data generating model, 
conditional on a parameterization of the stratification system. So long as the 
parameterization and data generating model are sufficiently robust for 
handling the behavior of these data, a conditionally $i.i.d.$ model of these 
data will be practically useful for producing predictions about the data 
generating system.
<!-- even if our model does not replicate the true data generating process exactly.-->

That said, for the purpose of modeling these data, it is enough to know 
which clusters were collected as part of which samples, and how big each cluster 
actually ended up being. This information is readily available from CALCOM, a 
database maintained by the California Cooperative Groundfish Survey 
(CALCOM 2018). Specifically, we aggregate all observed clusters across
<!--or the purpose of the models presented here, all of the-->
a unique sample so that the total weight sampled is the sum of all pounds in 
each unique sample. Similarly the observed weight for a particular species in 
each unique sample is the sum of all of the observed weights, for that 
species, across each cluster of each unique sample.

Although model based data analysis has the potential to add significant 
structure to data, a judicious application of these methods must always confront 
the model with enough empirical information to adequately learn about the system.
In this setting some market categories and time periods may not be well enough 
sampled to learn the parameters of the models presented here. For this reason, 
we refrain from modeling any period where the minimum possible number of 
effective parameters exceeds the number of samples for the modeled period. 
Rather than apply models inappropriately, these landings are speciated as the 
nominal species for their market category. We later demonstrate that due to 
prioritization in sampling heavily landed, or otherwise commercially relevant 
categories, this sample size heuristic leaves relatively few landings to be 
speciated in a statistically uninformed way (i.e. ``nominal'' speciation). <!--nominal speciation in market categories and time periods where total landings are low.--> 
Thus nominal speciation represents a negligible component of the overall 
expanded landings for most species. 

* something about chuncking time at regulation time periods?
	* introduce idea of modeled period (mcat/time chunk)
	* a section got added later on by EJ

## Data Generating Model
<!--Something something hierarchical Poisson model. Something something (Shelton, 2012).-->

For the purposes of accurately modeling not only species composition means, but
also higher moments of the data, such as species composition variances, it is 
necessary to recognize model limitations with respect to over-dispersed data. 
Among the simplest models for count data are the Poisson and binomial models. 
Both models are typically specified with a single parameter for<!--degree of freedom for--> 
modeling all of the moments of the data, and thus they rely heavily on their 
respective data generating processes to accurately represent higher moments in 
the data. McCullagh and Nelder (1989, pg. 124) commiserate about the 
prevalence of over-dispersed data in cluster sampling, and explain ways in 
which cluster sampling itself may result in over-dispersion.

Extending the Poisson and binomial models to deal with over-dispersion, 
typically involves adding additional parameters for the purpose of modeling 
higher moments of the data. The negative binomial (NB) distribution is often 
used as an over-dispersed extension of the Poisson model, since it can be 
expressly written as an infinite mixture of Poisson distributions. The 
beta-binomial model is used as an over-dispersed extension of the binomial 
model.

The Poisson and binomial models attempt to model both the mean and 
residual variance of the data, with a single parameter for each species. By 
definition these models do not have additional parameters to model the variance, 
but rather, residual variances in these models are simply transformations 
their mean parameters. Only estimating the mean parameters in these cases 
may not be sufficient to produce models which predict well.

In contrast, the negative binomial and beta-binomial models estimate an 
additional parameter which can be used to disentangle the mean and residual 
variance estimates. Thus the negative binomial and beta-binomial models may 
produce more accurate estimates of the residual variance, while producing more 
accurate measures of center. We develop an example for a subset of data to 
demonstrate considerably greater statistical support for the beta-binomial 
model (Appendix B), which we have subsequently used for the purposes of 
applying at an operational scale.
<!--
Furthermore, in better modeling the variance, these models may often 
produce more accurate measures of center as well.
-->

###Full-Scale Beta-Binomial Model

For a particular market category, $y_{ijklm\eta}$ is the $i^{th}$ sample of 
the $j^{th}$ species' weight, in the $k^{th}$ port, caught with the $l^{th}$
gear, in the $\eta^{th}$ quarter, of year $m$. As supported by the results in 
Appendix B, the $y_{ijklm\eta}$ are modeled as observations from a beta-binomial 
distribution conditional on parameters $\mu_{jklm\eta}$ and $\sigma^2_{jklm\eta}$,

<!--$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\bm{\theta}, \rho).$$-->
$$y_{ijklm\eta} \sim BB(\mu_{jklm\eta},~\sigma^2_{jklm\eta}).$$

Above, $\mu_{jklm\eta}$ is the stratum level mean weight, and 
$\sigma^2_{jklm\eta}$ is the stratum level residual variance. $\mu_{jklm\eta}$ 
is related to a linear predictor, $\theta_{jklm\eta}$, via the mean function,

<!--$$\mu_{jklm\eta} = n~\text{logit}^{-1}(\theta_{jklm\eta})$$-->
$$\mu_{jklm\eta} = n_{ijklm\eta}\frac{\exp(\theta_{jklm\eta})}{1+\exp(\theta_{jklm\eta})}=n~\text{expit}(\theta_{jklm\eta})=n~\text{logit}^{-1}(\theta_{jklm\eta}).$$

Here $n_{ijklm\eta}$ is the observed aggregate cluster size for each sample. 
Additionally, $\sigma^2_{jklm\eta}$ is related to $\mu_{jklm\eta}$ and the 
overdispersion parameter, $\rho$, via the following equation,

$$\sigma^2_{jklm\eta} = \mu_{jklm\eta}\Big(1-\frac{\mu_{jklm\eta}}{n_{ijklm\eta}}\Big)\Big(1+(n_{ijklm\eta}-1)~\rho\Big).$$

<!--https://www.healthknowledge.org.uk/public-health-textbook/research-methods/1a-epidemiology/clustered-data-->
$\rho$ is the within-cluster correlation. The situation where 
$\rho\rightarrow1$ represents identical information content among replicates 
within a cluster, with maximal overdispersion relative to the binomial 
distribution. The situation where $\rho\rightarrow0$ represents totally 
independent information content among replicates within a cluster, and the 
beta-binomial model approaches the binomial model. $\rho$ explicitly models 
average overdispersion across all strata within a market category, while 
$\mu_{jklm\eta}$ gives the model flexibility at the stratum level through the 
$\theta$ linear predictors,

<!--$$\theta_{jklm\eta} = \beta_0 + \beta^{(s)}_j + \beta^{(p)}_k + \beta^{(g)}_l + \beta^{(y:q)}_{m\eta}.$$-->
$$\theta_{jklm\eta} = \beta_0 + \beta^{(s)}_j + \beta^{(p)}_k + \beta^{(g)}_l + \beta^{(t)}_{m\eta}.$$

Firstly, $\theta$ includes a reference level intercept ($\beta_0$). Secondly, 
$\theta$ is factored among the many strata by additive offsets from $\beta_0$ 
for each of the species ($\beta^{(s)}_j$), port-complexes ($\beta^{(p)}_k$), and 
gear-groups ($\beta^{(g)}_l$). Finally year and quarter parameters 
are indicated generally here inside the $\beta^{(t)}_{m\eta}$ term. Several 
forms for $\beta^{(t)}_{m\eta}$ are explored each implying a different prior 
and partial pooling strategies as described in the following section($Priors$).

## Priors

To complete the Bayesian formulation of this model, priors are expressed in a 
largely diffuse manner. 

$$\beta_0 \propto 1$$
$$\left\{\beta^{(s)}_j, \beta^{(p)}_k, \beta^{(g)}_l\right\} \sim N(0, 32^2)$$

Since the $\beta_0$ reference level is chosen arbitrarily, with no conception 
of which values it may take, no restrictions are placed on the value of the 
intercept. The species ($\beta^{(s)}_j$), port-complex ($\beta^{(p)}_k$), and 
gear-group ($\beta^{(g)}_l$) offsets are assigned diffuse normal priors. The 
large fixed values of the prior variance hyperparameters produce behavior 
similar to classical fixed effect models for species, port-complex, and gear-
group parameters.

In returning to the time parameter model, $\beta^{(t)}_{m\eta}$, it is useful 
to consider how overparameterized models <!--the inclusion of predictively superfluous parameters-->
may cause overfitting and weaken model performance through the bias-variance 
dilemma (Ramasubramanian, K., \& Singh, A., 2016). Simply put, the 
bias-variance dilemma means that model formulation is not simply a bias 
reduction task, but rather the goal is to formulate models which reduce bias, 
while jointly minimizing uncertainty. Janyes (2003, pg. 511) describes how the 
inclusion of estimation bias via the Bayesian methodology may produce better 
performing estimates, more quickly, than unbiased counterparts.  Among the 
simplest ways to see the principle is in the structure of the MSE performance 
metric, and how it can be explicitly written to value both estimator bias and 
variance, as follows. 

$$\text{MSE}(\hat\theta) = \mathbb{E}\left[~(\hat\theta - \theta)^2~\right] = \overbrace{\mathbb{E}\Big[~\left(\hat\theta-\mathbb{E}(\hat\theta)\right)^2~\Big]}^{\text{Var}(\hat \theta)} + \overbrace{\Big(~\mathbb{E}(\hat\theta)-\theta~\Big)^2}^{\text{Bias}(\hat \theta, \theta)^2}$$

<!--
Including additional model parameters is a great way to decrease model bias, 
however it is apparent from the structure of the MSE that decreasing model 
bias alone is not a good way of arriving at a well performing model. Models 
with good MSEs jointly minimize the bias of their parameter estimates, in 
addition to estimation uncertainty of their parameters. 
-->

Furthermore a model can minimize bias, without regard for estimation uncertainty, 
by including one model parameter to be fit to each observation. These 
parameter estimates are totally unbiased, however such a model is also 
predictively useless since each estimated parameter is specifically bound to a  
particular observation, and thus such a model does not generalize. <!--has no information for which to base predictions on future data.-->

For modeling $\beta^{(t)}_{m\eta}$ we consider a spectrum of models which span 
a wide range of partially pooled models with several different predictive 
structures as seen below.

###(M1)
$$\beta^{(t)}_{m\eta} = \beta^{(y)}_{m} + \beta^{(q)}_{\eta}$$
$$\beta^{(y)}_{m} \sim N(0, 32^2)$$
$$\beta^{(q)}_{\eta} \sim N(0, 32^2)$$

(M1) represents a fixed effects model for additive year and quarter parameters. 
Here each year and quarter are assigned totally independent and diffuse priors. 

###(M2)
$$\beta^{(t)}_{m\eta} = \beta^{(y)}_{m} + \beta^{(q)}_{\eta}$$
$$\beta^{(y)}_{m} \sim N(0, v^{(y)})$$
$$\beta^{(q)}_{\eta} \sim N(0, v^{(q)})$$

(M2) estimates two hierarchical variance parameters, $v^{(y)}$ and $v^{(q)}$.
$v^{(y)}$ has the effect of partially pooling information among year parameters, 
while $v^{(q)}$ partially pools information among quarter parameters (i.e. 
treats both year and quarter as ``random effects''). The actual degree of 
pooling among each of the years and quarters is determined by the data.  
Depending on the posterior distributions of $v^{(y)}$ and $v^{(q)}$, the $\beta^{(y)}$ <!-- the $\bm{\beta^{(y)}}$ and $\bm{\beta^{(q)}}$ -->
and $\beta^{(q)}$ may be shrunk back toward the common mean (for small $v$) or 
allowed to take largely distinct values (in the case of large estimates of the $v$).    

<!--
$v^{(y)}$ partially pools information among the $\beta^{(y)}_{m}$, and 
separately $v^{(q)}$ partially pools information among the $\beta^{(q)}_{\eta}$. 
-->

###(M3)
$$\beta^{(t)}_{m\eta} = \beta^{(y)}_{m} + \beta^{(q)}_{\eta} + \beta^{(y:q)}_{m\eta}$$
$$\beta^{(y)}_{m} \sim N(0, v^{(y)})$$
$$\beta^{(q)}_{\eta} \sim N(0, v^{(q)})$$
$$\beta^{(y:q)}_{m\eta} \sim N(0, v)$$

(M3) functions similarly as (M2), in that it has hierarchical partial pooling 
among both the $\beta^{(y)}_{m}$ and $\beta^{(q)}_{\eta}$ parameters, except 
that it introduces a two-way interaction term between year and quarter. This 
interaction term allows estimates for particular quarters to differ from 
year to year, as opposed to the previous models in which quarters 
within a year are assumed to be identical from year to year. 

Furthermore the $\beta^{(y:q)}_{m\eta}$ are also modeled hierarchically to 
introduce a single variance parameter, $v$, shared among all of the $m\eta$ 
time chunks. Although this interaction term adds many parameters to the model, 
the shared $v$ parameter functions to shrink extraneous $\beta^{(y:q)}_{m\eta}$ 
estimates back toward the common stratum mean. 


###(M4)
$$\beta^{(t)}_{m\eta} = \beta^{(y:q)}_{m\eta}$$
$$\beta^{(y:q)}_{m\eta} \sim N(0, v)$$

(M4) simplifies (M3) by excluding year and quarter main effects. This leaves all 
temporal information in the data to be modeled solely by the quarterly 
$\beta^{(y:q)}_{m\eta}$ interaction terms. This model represents more opportunity 
for partial pooling through time than (M3), as fewer time parameters are 
introduced. Furthermore all of the $\beta^{(y:q)}_{m\eta}$ are hierarchically 
pooled back toward a single common stratum mean via the single shared variance 
parameter, $v$. No models with fixed $\beta^{(y:q)}_{m\eta}$ interaction terms are 
considered here, as these models introduce a very large number of parameters.
Due to the sparcity of these data, the models ability to pool through time 
is important for allowing model flexibility in time, while retaining the ability 
to produce predictions for unsampled time periods.
  
###(M5)
$$\beta^{(t)}_{m\eta} = \beta^{(y:q)}_{m\eta}$$
$$\beta^{(y:q)}_{m\eta} \sim N(0, v_\eta)$$

(M5) is largely the same as (M4), but it represents slightly less potential
partial pooling through its hierarchical prior variances, $v_\eta$, on
$\beta^{(y:q)}_{m\eta}$. Here interaction terms are allowed to partially pool
interactions across years, within a common quarter, but since each quarter is 
assigned a separate variance parameter no pooling is possible between quarters.

###(M6)
$$\beta^{(t)}_{m\eta} = \beta^{(y:q)}_{m\eta}$$
$$\beta^{(y:q)}_{m\eta} \sim N(0, v_m)$$

(M6) follows the same idea as (M5), however here interaction terms are allowed 
to partially pool interactions within a common year, across the quarters of 
that year, but not between years. (M6) often involves fitting slightly more 
parameters than (M5) because, at least in this setting, it is typical to model 
more than four years of data at once.

Historically, regulations have been enacted with the aim of isolating catch in 
a market category to a single species (sort requirements). This clearly 
affects the composition of the target market category, but these regulations 
also affect the species composition of other market categories in which the 
target species previously occurred. We incorporate this information into the 
model structure by treating time periods with relatively stable regulatory 
conditions as independent models. In other words, information is only shared 
among years in which regulations were similar. For example, a sort requirement 
for widow rockfish (S. entomelas) was initiated in 1983, which not only 
affected the composition of the widow rockfish market category (269), but 
also the composition of other categories, including the unspecified rockfish 
market category (250). We model the first five years of available data 
(1978-1982) independently from the years 1983-1990. In 1991, a sort 
requirement for bocaccio rockfish (S. paucispinis) was enacted, which is known 
to have affected the composition of other categories, including the 
Chilipepper rockfish market category (254) and the Chilipepper/Bocaccio market 
category (956).

Hierarchical variance parameters are estimated from the data. As the above 
models learn the posteriors of the hierarchical variance parameters, it affects 
the degree of shrinkage as well as the effective number of parameters held 
within the respective hierarchies (Gelman, 2014). To achieve this, each 
variance parameter must itself be assigned a prior to be estimated. For all of 
the hierarchical variance parameters included in the above models $v$ is 
assigned a diffuse inverse gamma (IG) prior $v \sim IG(1,~2\times10^{3})$.
<!--and heavy tailed--> 

<!--first logit transformed to have values spanning the entire real number line and assigned the prior-->
Finally the overdispersion parameter, $\rho$, is assigned a diffuse normal 
prior on the logit scale, $\text{logit}(\rho) \sim N(0, 2^2)$. The $N(0, 2^2)$ 
prior is indeed a symmetric, and far reaching, prior when back transformed to 
the unit interval. To notice this, it is helpful to realize that the central 
95% interval for a $N(0, 2^2)$ (i.e. $0\pm3.91$), includes almost the entirety 
of the back transformed unit interval (i.e. $0.5\pm0.48$).

Each of models M1-M6 are fit to data from 1978-1982 in market category 250, with 
Table($priortable$) guiding the use of model (M4) for the duration of the study. 

## Species Composition Prediction
<!--
$$p(y^*_{jklm\eta}|\bm{y}) = \int\!\!\!\!\int\! \text{BB}\Big( y^*_{jklm\eta}|\mu_{jklm\eta}, \sigma^2_{jklm\eta} \Big) P\Big(\mu_{jklm\eta}, \sigma^2\Big) P\Big(\mu_{jklm\eta}, \sigma^2_{jklm\eta} | \bm{y}\Big) d\mu_{jklm\eta} d\sigma^2_{jklm\eta}$$
$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ \bm{y}^*_{klm\eta}\neq \bm{0}$$
-->

Bayesian inference of the above models gives access to the full posterior 
distribution of all of the parameters of the model given the data. It is 
useful to emphasize that in the Bayesian setting, these parameters have full 
distributions, and they are typically handled as a large number of samples 
from the joint posterior distribution of the parameters. Once the posterior 
sampling is complete, this simplifies parameter mean and variance estimation; 
required moments are simply obtained by computing the desired moments from the 
posterior samples. Additionally, the fact that the parameters are full 
distributions means that any functions of those parameters are themselves 
random variables with the function representing a transformation of those 
parameters.

<!--Model M5 is a model on sampled weight; -->
To obtain predicted species compositions from this model, first consider the 
posterior predictive distribution of sampled weight for a particular stratum.

$$p(y^*_{jklm\eta}|y) = \int\!\!\!\!\int\! \text{BB}\Big( y^*_{jklm\eta}|\mu_{jklm\eta}, \sigma^2_{jklm\eta} \Big) P\Big(\mu_{jklm\eta}, \sigma^2_{jklm\eta} | y\Big) d\mu_{jklm\eta} d\sigma^2_{jklm\eta}.$$

Here BB is the data generating beta-binomial distribution for a predictive 
observation and $P(\mu_{jklm\eta}, \sigma^2_{jklm\eta}|y)$ is the posterior 
distribution of the parameters given the observed data. Integration of the 
parameters, $\mu_{jklm\eta}$ and $\sigma^2_{jklm\eta}$, is done by Monte Carlo 
integration to obtain samples from the predictive distribution, 
$p(y^*_{jklm\eta}|y)$, for sampled weights in the $jklm\eta^{th}$ stratum. 

Obtaining predictive species compositions from predictive weights amounts to 
computing the following transformation,

$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ y^*_{klm\eta}\neq 0.$$ 

For a particular market category, $\pi^*_{jklm\eta}$ is predicted proportion of 
species $j$ in the $k^{th}$ port, caught with the $l^{th}$ gear, in the 
$\eta^{th}$ quarter, of year $m$. 
<!--
The constraint 
$y^*_{klm\eta}\neq 0$ is added to exclude the predictive situations where no 
sampling occurs in a stratum. In these predictive situations predictions are 
not capable of providing species composition information, and the stratum is 
simply re-sampled.
-->

## Expansion

For a particular market category, speciated landings simply amounts to the 
multiplication of the known total landings ($\lambda_{klm\eta}$), reported on 
landing receipts in the $klm\eta^{th}$ stratum, with the posterior predictive 
$\pi^*_{jklm\eta}$, as follows 

$$\lambda^*_{jklm\eta} = \lambda_{klm\eta}\pi^*_{jklm\eta}.$$

$\lambda^*_{jklm\eta}$ is then the posterior predictive landings for species 
$j$ in the $klm\eta^{th}$ stratum of a particular market category. Recall that 
since $\pi^*_{jklm\eta}$ is a random variable, then so is $\lambda^*_{jklm\eta}$. 
Computing the variance of $\lambda^*_{jklm\eta}$ simply amounts to computing 
the variance of random draws from the $\lambda^*_{jklm\eta}$ distribution. 
Furthermore, any level of aggregation of $\lambda^*_{jklm\eta}$ is easily 
obtained by summing $\lambda^*_{jklm\eta}$ draws across the desired indices. 
For example to obtain the distributions of yearly catch of Bocaccio in a 
particular market category, one simply sets $j=BCAC$, and computes the 
following transformation of $\lambda^*_{jklm\eta}$,

$$\lambda^*_{j\cdot\cdot m\cdot} =\sum_{k}\sum_{l}\sum_{\eta}\lambda^*_{jklm\eta}$$.

Distribution summaries such as quantiles, means, or variances may be computed 
by computing those metrics from the random draws of the resulting 
$\lambda^*_{j\cdot\cdot m\cdot}$ distribution.
 

## Model Exploration \& Averaging

Presently, strata with diminishingly small sample sizes are managed by an 
ad-hoc "data borrowing" protocol, as outlined in Pearson and Erwin (1997). The 
protocol for "data borrowing" calls for pooling only when forced to fill holes 
brought about by unsampled strata. Naturally, such a pooling protocol introduces 
bias to fill in unsampled strata, however due to the mathematically unstructured 
way in which this bias is introduced, it is hard to quantitatively justify 
these "data borrowing" rules.  

Model (M4) avoids temporal ad-hoc "borrowing" protocols described in Pearson
and Erwin (1997) by making use of its hierarchical structure to fill temporal 
holes with a posterior predictive distribution for unseen time periods within 
the modeled period. This hierarchical structure uses the data to estimate the 
degree of pooling through time, rather than ad-hoc "data borrowing".

<!--
Furthermore the Bayesian predictive framework, allows holes 
in the data to be filled with posterior predictive distributions for any 
unobserved strata. 
-->

<!-- %efforts to pool data through time via the hierarchical priors described in section (XX) -->
Despite the benefits of modeling these data as Bayesian hierarchical models, 
port sampling data still remains sparse. Given the degree of sparsity in these 
data it is certainly possible that models which consider an additional degree 
of data pooling between port complexes may offer predictive benefits. In 
exploring strategies for pooling data across space it is necessary to formalize 
the port complex pooling scheme in a way which provides a mathematically 
understandable and scalable structure to build upon.

Given the spatial structure, and complex behavior of, port complex parameters, 
the typical zero mean hierarchical regularization priors are not appropriate 
among port complexes. Pooling across spatial categorical parameters in this 
setting requires the ability to pool port complex parameters back toward an 
unknown number ($\le K$) of mean levels.
<!--
 Port complex certainly contains predictive information, 
but port complex 
information is exploring pooled 
Port complex models benefits from 
regularization, but pooling all port complexes back toward a common level 
ignores how port complexes close in space may behave similarly.  
 toward  are not appropriate.
-->
Rather than hierarchically regularize port complex, we frame port complex 
pooling as a model uncertainty problem, in which we consider some degree of 
full pooling among port complex, but the exact degree of pooling, and the particular 
partitioning of the pooled port complexes are not known. 

Port complex pooling is achieved by repeatedly fitting model (M4) with different 
partitionings of the port complex variables within a particular market category 
and modeling time period. This model exploration exercise explores the 
possible ways to produce groupings of the existing port complexes so as to 
discover predictively useful partitionings of the port complexes. Insisting that 
the port complex groupings be partitions of the available port complexes 
provides a well-defined mathematical structure for exploring the space of 
pooled port complexes.

The size of the space of possible pooled models is in the setting is well 
defined in terms of the size of the set of items to be partitioned, $K$, as 
given by the Bell numbers ($B_K$),

$$B_K=\sum_{\hat k=0}^{K} \frac{1}{\hat k!} \left( \sum_{j=0}^{\hat k} (-1)^{\hat k -j} \left(\substack{\hat k \\ j}\right) j^K \right).$$

In the case of California the set of items to be partitioned is the set of 
port complexes in California, of which there are $K=10$, implying a grand total of 
$B_{10}=115975$ ways of partitioning the port complexes in California in each 
market category and modeled time period. The brute force model selection 
strategy of computing all 115975 of these partitionings strategies is 
computationally infeasible. However, not all pooling schemes represent 
biologically relevant models. For example, it is likely reasonable to pool 
only among adjacent ports (i.e. no discontinuities between port complex 
pooling in space) due to species distributions and the presence of 
biogeographical provinces, and it may be similarly reasonable to assert that 
similar regions can only extend across a small number of ports.

Here only adjacent port poolings are considered, such that the maximum size of 
a port complex grouping is three port complexes. These are the only two 
constraints that are enforced on port complex partitions here, although many 
other constraints may, in theory, be chosen. These constraints were chosen so as to 
mirror the currently accepted protocols in Pearson and Erwin (1997) within the 
context of this framework. When these two simple constraints are applied, the 
number of models to explore in each modeled period is reduced to a much more 
manageable 274 models. 

An exhaustive search of the models in the constrained subspace of $B_{10}$, 
allows for a concrete comparison of the relative predictive accuracy of each 
partitioning. Additionally the partitioned models provide a set of candidate 
models for use in Bayesian Model Averaging (BMA) (Hoeting et al., 1999). BMA, 
as applied here, allows the model exploration strategy to average models 
across all potentially relevant partitions of the port complexes, so as to add 
robustness to final species composition estimates.

For the $\iota^{th}$ model in a set of candidate models $M$, then the BMA weight 
for $M_\iota$ follows directly from Bayes Theorem as, 

<!--\mathbb{M}-->
$$\omega_\iota = Pr(M_\iota|y) = \frac{ p(y|M_\iota)p(M_\iota) }{ \sum_\iota p(y|M_\iota)p(M_\iota) }.$$

<!--$??model priors??$--> 
Where $\omega_\iota$ is the posterior probability that model $\iota$ is the true
data generating model of the data, conditional on the subspace of candidate
models and the observed data. $\omega_\iota$ is then straightforwardly used to
average posteriors across all of the models, as 

$$\bar p(\theta|y) = \sum_{\iota} \omega_\iota p(\theta|y, M_\iota).$$

# Results

![mcatsTable](./pictures/MC_summary_table_part_1.png)
![mcatsTable](./pictures/MC_summary_table_part_2.png)

## Characteristics of the Landings Data

In the two time periods modeled here, Figures (bar78) and (bar83) show how
commercial port sampling effort tracks both total landed weight as well as the 
number of species in market categories accounting for the top 99% of total 
landings in each time period. Comparable figures for the periods 1991-1999 and 
2000-2015 are provided in Appendix A, although we have not yet completed 
modeling for these time periods. It is important to notice that since port 
sampling effort prioritizes heavily landed market categories, and our model is 
only fit to market categories with more data than parameters, then market 
categories left with too few samples to fit our model tend to be less landed. 
Thus our model is applied to a relatively large proportion of the landings 
and nominal speciation occurs for a relatively negligible proportion of total 
landings. Applying the sample size heuristic to determine which market categories 
are expended by our model results in 96.8% of landed weight being expanded via 
our model in the 1978-1982 time and 98.3% of landed weight expanded by our model 
in 1983-1990.
<!--
78: 0.9675626
83: 0.982643
Since the our model requires separate 
parameters for each species and rockfish market categories tend to have many 
species, the number of species in a market category should, and apparently is, be an important 

Since port sampling effort 
prioritizes heavily landed market categories, then market categories that are 
left with too few samples to apply our model tend to represent a negligible 
proportion of total landings.
-->

<!--
Total landed weight and the number 
of species landed in a market category are among the most important 
characteristics to prioritize in sampling for the sake of applying 
our model's results to a large proportion of landings $(What proportion of 
landings is the model expanding?)$. Furthermore, since port sampling effort 
prioritizes heavily landed market categories, then market categories that are 
left with too few samples to apply our model tend to represent a negligible 
proportion of total landings.
-->

The lower panels of Figures (bar78) and (bar83) demonstrate just how many 
different species are landed into commercially relevant market categories. 
Although market categories often carry names that label them with a nominal 
species, Figure (bar78) makes it abundantly clear that these names can mislead 
our thinking about the purity, and consistency, of these categories through time. 
To drive this point, consider the sampled species in market category 267 in 
1978-1982. The nominal label for market category 267 is Brown Rockfish, while 
Brown Rockfish only amounts to a small fraction of that category in 1978-1982. 
In fact, only 6.3% of the sampled weight in 1978-1982 consisted of Brown 
Rockfish. In 1978-1982 market category 267 might be better named Widow 
Rockfish as Widow amounts to 92.6% of sampled weight in this time period, 
however market category 267 is composed of 99.6% Brown and 0% Widow in recent 
time periods (see Appendix A Figure (bars00)).

<!--![panel](./pictures/mockUp.png)-->

![
The relative sampling effort and landings across rockfish market categories 
accounting for the top 99% percent of landings for two modeled periods, 
1978-1982 and 1983-1990. In the top panel of each figure the black 
line represents the proportion of landings in each market category, while the 
blue line represents the proportion of samples, by number, taken in each 
market category.  In the bottom panel of each modeled period, colors represent 
the proportion of sampled weight of thirteen commercially relevant species.  
Alternating dark and light grey panels fill in all other species. On the top 
of each bar the number of observed species in each market category is listed.  
Hatched regions indicated the nominal species of a market category where 
applicable.](./pictures/1978to1982Bar3.png)
![same](./pictures/1983to1990Bar3.png)
![legend](./pictures/barplotLegend.png)

<!--
![worse](./pictures/1991to1999Bar3.pdf)
![worst](./pictures/2000to2015Bar3.pdf)
-->

## Predictor and Prior Selection

<!--      Both Fixed  Both Random  Both Random Plus $V$ Only $V$     Only $V_m$   Only $V_\eta$-->            

               M1          M2           M3                  M4            M5           M6               
------------- ----------- ------------ ------------------- ------------  ------------ ----------
MSE            0.127245    0.127042     0.126801            0.122373      0.127236     0.126573           
$\Delta$ DIC   2558.56     2259.94      2013.21             0             2175.32      2174.71    <!--39790.26    39491.64     39244.91            37231.70      39407.02     39406.41-->             
$\Delta$ WAIC  2562.65     2263.58      2009.32             0             2171.18      2170.56    <!--39745.58    39446.51     39192.25            37182.93      39354.11     39353.49-->        
$pr(M|y)$      $\approx0$  $\approx0$   $\approx0$          $\approx1$    $\approx0$   $\approx0$         



<!--0.008399    0.008524            0.008391            0.008443            NA           NA                  NA           --> 
<!--103182.45   102373.85           102332.46           101743.90            101238.84    101241.41          101247.95    -->      
<!--103127.61   102318.63           102277.24           101688.38            101172.87    101175.44          101181.99    --> 
<!--$\approx0$  $\approx10^{-274}$  $\approx10^{-265}$  $\approx10^{-125}$   $\approx1$   $\approx10^{-10}$  $\approx10^{-17}$--> 




<!--
c(-2.688421136e+04, -2.674287641e+04, -2.663455027e+04, -1.362101051e+04, -2.689255e+04, -2.688977508e+04)
mlik,waic,dic
-2.688421136e+04, 3.974557761e+04, 3.979026373e+04
mlik,waic,dic
-2.674287641e+04, 3.944650791e+04, 3.949164445e+04
mlik,waic,dic
-2.663455027e+04, 3.919224608e+04, 3.924490931e+04
mlik,waic,dic
-1.362101051e+04, 3.718292739e+04, 3.723170029e+04
mlik,waic,dic
-2.666632423e+04, 3.919504087e+04, 3.924753028e+04
mlik,waic,dic
-2.688977508e+04, 3.935349306e+04, 3.940641050e+04
-->
<!--mlik , , , , , , -->                        <!--$pr(M|y)$  $\approx0$     5.370390e-275 2.496899e-265  4.870208e-125 $\approx1$ 5.357171e-11 2.083286e-17-->
<!--prob , , , , , , -->                        <!-- -69074.74, -68665.55, -68643.29, -68320.26, -68034.02, -68057.67, -68072.43-->


Table$(Predictor)$ displays the result of fitting models M1-M6 to data from 
1978-1982 in market category 250. Recall models M1-M6 differ in the structure 
of the $\beta^{(t)}_{m\eta}$ time parameters. Table$(Predictor)$ shows the 
relative support for those model structures. From M1 to M4 the models represent a 
spectrum of models with an increasing potential of shrinkage among time 
parameters. <!--which decrease in the possible number of implied parameters.-->
Models M5 and M6 represent models which build in complexity, from M4, via the 
inclusion of multiple hierarchical variance parameters among the interaction 
terms.

Across all of the time models, model M4 displays consistent support over all
other candidate models considered here. It is worth mentioning that among all 
of the models considered here, (M4) offers the largest potential for 
hierarchical partial pooling among the time parameters. Model M4 represents a 
model with maximal potential for pooling through time, while still maintain the 
ability to model differences in seasonality from year to year.

## Model Exploration \& Averaging

![model selection: 1978 to 1982](./pictures/tinyTrim.png)

Figure$(model selection)$ shows the results of port complex model selection 
for the modeled period from 1978 to 1982 in market category 250. Along the 
top BMA weights ($\omega$) for the top 10 models are displayed (each column is a 
distinct model). The following ten rows indicate the ten port complexes in 
California, and the colored cells indicate how port complexes are partitioned 
in each model.

Considering Figure$(model selection)$, the best partitioned model 
(first column, $\omega=0.32$) gives distinct parameters to CRS and ERK, while 
pooling BRG/BDG, OSF/MNT/MRO, and OSB/OLA/OSD. This model uses five parameters 
to model the ten ports complexes in California. Given the set of candidate 
models explained above, the BMA procedure weights this model at approximately 
32% of the truth. Notice that the only difference among the top four models is 
in how the port complexes south of Point Conception are handled. In fact, the 
seven northerly port complexes are identically partitioned in the top four 
models, which also represent all of the possible partitionings of the 
southern three port complexes. 

In this modeled period it is known that no species composition sampling was 
done south of Point Conception, thus it is not surprising that these models 
perform similarly. When no data is present, parameters simply represent place 
holders for out of sample prediction. Since the port complexes south of 
point conception are not informed by data, the predictions are identical in 
these categories. Since the first model makes identical predictions to the 
following three, and does so using the fewest parameters, it is correctly 
identified as the most parsimonious explanation among these data.

Considering how the top four model configurations share identical structure 
in the seven northerly port complexes, while exhaustively spanning the candidate 
partitions south of Point Conception, it is simple to see that BMA assign's 
approximately 71% marginal probability to the northerly model structure.

The results shown here only represent a single market category across the time 
period 1978-1982. Similar results for other market categories and time periods 
are provided in the appendix Figure$(color tables)$.

## Prediction

Repeatedly fitting model (M4) across port complex configurations and applying the 
BMA procedure, ultimately provides access to posterior predictive distributions 
of the species compositions ($\pi^*_{jklm\eta}$) within a market category and 
time interval modeled period. A straight forward way to evaluate the performance 
of the model in each modeled period is to compare the predictions of the model 
in each modeled period with the actual observations of species compositions 
from port samplers.

We evaluate species composition posterior predictive distributions via HDI at 
three levels containing 68%, 95%, and 99% of posterior predictive probability. 
Table$(Prediction)$ shows the proportion of observed species compositions 
which existed within the HDI across all strata, of each prediction level, in 
each modeled period. For example, observed species compositions for market 
category 250 in the 1978-1982 time period fell within the 68% HDI of the 
posterior predictive distribution 67.1% of the time (Table X). 

### 78-82
    68%   95%   99%
--- ----- ----- -----
250 67.1% 96.1% 98.7% 
253 67.3% 96.3% 98.9%
262 67.4% 93.8% 95.3%
265 69.6% 96.0% 97.8%
269 68.2% 88.8% 90.2%
270 68.6% 93.6% 96.7%
956 68.3% 96.7% 99.2%
959 68.5% 96.3% 98.1%
961 69.3% 93.2% 95.3%
AVG 68.3% 94.5% 96.7%

### 83-90
    68%   95%   99%
--- ----- ----- -----
245 60.8% 94.9% 97.7%
250 68.1% 96.0% 99.0%
253 69.3% 97.1% 98.9%
259 83.8% 91.9% 92.9% 
262 68.5% 95.1% 95.9%
269 68.6% 94.2% 94.7% 
270 67.9% 94.2% 96.7%
663 68.1% 94.1% 96.3%
667 69.4% 92.5% 93.5%
956 67.5% 96.2% 99.0%
959 67.4% 96.4% 99.0%
960 68.0% 96.1% 98.6%
961 68.6% 94.6% 97.8%
AVG 68.9% 94.9% 96.9%

Table$(Prediction)$ largely shows that the observed proportion of predicted 
samples aligns appropriately with the predictions made by the model. Considering
the average performance across market categories at each prediction level, it 
 appears that prediction is mostly appropriate with the possible exception of
the 99% prediction level. The 99% prediction level appears to slightly 
under-predict on average, indicating that predictive distributions are slightly 
lighter in the far tails than the data.

# Discussion

## Modeling
Models that can incorporate catch uncertainty are not new (c.f. Doubleday
1976), but estimates of uncertainty in commercial landings are rarely made
available, at least on the U.S. West Coast. Previous research has focused on
discarded and biased (under- or over-reported) catch. Bousquet et al. (2010)
developed a model-based framework for estimation of catch and associated
uncertainty when underreporting is an issue. Their results included a 
reduction in precision of management quantities.

The model based statistical framework allows tremendous flexibility in 
accounting for a dynamic port sampling program. Market forces, regulation 
changes, and fisherman behavior are a few factors, among the many, which 
complicate the task of speciating commercial catch. Unlike a purely 
sample-based statistical framework, model based statistics allows analysts to 
quickly explore a wide range of hypotheses for estimating species compositions. 
The models entertained here manage to achieve generally well behaved predictive 
accuracy (Tables pred78 and pred 83), however these models are by no means 
perfect. The models presented here simply offer a few fundamental improvements toward 
estimating species compositions. 

Among the largest structural changes improving from the Bayesian methodology 
in Shelton et. al. (2012) is the recognition of overdispersion in port sampling 
data. In the absence of highly predictive covariariates, modeling overdispersion 
in port sampling data remains an important modeling consideration. Moving 
forward, modeling decisions will require a careful consideration of 
predictive accuracy and bias/variance trade-off, so as to tease better and 
better performance out of further models. The models presented here offer a 
great operational starting point and provide a basic framework for further 
model exploration.

The Bayesian models presented here provide easy access to estimates of uncertainty 
in commercial catch, at any aggregation of the stratification system. 
Making posterior predictive draws from these models widely available, allows scientists 
the flexibility to easily compute whichever derived distributions or distributional 
summaries that follow from this general modeling approach. 

##Data
Due to observed trends in the sampling of more heavily landed, or specious, market 
categories, the vast majority of commercial landings from 1978-1990 are able to be 
expanded by our model. Moving into the modern era, regulation changes including the 
start of the live fish fishery, and the proliferation in the number of market 
categories, due to mandatory sort requirements, may challenge species composition 
estimation. However, due to the increased number of strata that these changes 
introduce, uncertainty estimation for these time periods will prove to be 
critical. Without a matched increase in sampling effort, alongside increased 
stratification, the number of samples per stratum falls dramatically and 
species composition estimation may well become very uncertain.

<!-- Moving forward-->
Given a data sparse setting, model-based strategies of catch estimation provide 
the best chance of a full statistical treatment of available data. However, 
a more informed path forward involves either increasing sampling effort, or a 
simplification of the stratification system. Either of these changes provide 
models with more data to better infer parameters. Model flexibility and 
justifiable stratum pooling strategies, will become vital for modeling 
data-sparse time periods. Although estimates are likely estimable in these 
sparse time periods, as pooling strategies become more extreme, model fit will 
suffer as both bias and variance estimates increase.

<!--
hilethe performance of this system moving forward.

port sampling presents further 
issues 

have been able to be 
ex


* Sampling
	* Since the our model requires separate parameters for each species and 
	rockfish market categories tend to have many species, the number of species in 
	a market category should, and apparently is, 
	* Sort requirements do not eliminate the need for port sampling.
	* The proliferation of market categories over time in the sampled catch has
	not been matched with an increase in sampling effort, effectively reducing the 
	average number of samples per category over time (Figure X). This reduces 
	precision of catch estimates, increases uncertainty in stock assessment outputs
	, and impedes efforts to monitor removals relative to catch targets.
	* Model-based approach is best course of action given sparse data, but best
	solution is to increase sampling or reduce the number of strata.	

-->


##Future Effort
Future effort in developing models should include an exploration of the effect 
of preferential sampling toward heavily landed stratum. The model-based 
approach makes testing this hypothesis straight-forward, as the hypothesis may 
amount to the inclusion of a single slope parameter, in the linear 
predictor, regressing on landing size. Given the current model's agreement 
with existing data, ?as well as comlands estimates?, it is unlikely that landing 
size has an important predictive effect on estimates, however without testing 
the hypothesis, we can not say whether the effect will prove to be 
explanatory.      

In an attempt to add further flexibility to the models presented here, exploring 
the posibilty of gear-species interactions, as random effects, may prove fruitful. 
Furthermore the inclusion of random vessel effects may also find support.

Finally, further large changes to the methods proposed here might include a 
true multivariate handling of the likelihood. The Beta-Binomial univariate 
model, used here, suggests that the multivariate Dirichelet-Multinomial 
extension might be a good model for these data. We have yet to get these 
models to practically compute, although they would likely impose much needed, 
and appropriate, structure across the many species of this system. 

The BMA procedure presented here adds significant robustness and pooling 
potential to our species composition estimates, however it does so at a 
substantial computational cost. We have found ways (through parallel 
computation and constraining the model search) to make the computation 
tractable, however the solution is far from elegant. By using Dirichelet 
process Bayesian nonparameteric models, among the port parameters, a far more 
elegant, exhaustive, and potentially computationally cheaper model search may 
be possible.      

<!--
* Other modeling choices
	* We treat the fish tickets as a census (total landed weight could be biased)
	* condition model on landed weight, although its unlikely to make a 
	difference based on CALCOM comparison
	* f(gear:spp) flexibility
	* f(vessel)	

* Looking Forward	
	* forecasting/hindcasting
		* simple
		* time series models
	* more computation faster
		* broader model exploration
		* broader spatial expansion

* Structural
	* multivariate
	* Hierarchical/DP Spatial Models 
-->



## ?? Misc. Other ideas ??

* Sen (1986)
	* Recommended a minimum of 4 samples in each category (MC, gear, live) within 
	a port-month stratum, roughly 52 samples per year. Redirect sampling to 
	infrequently landed categories until 4 samples are obtained.
	* An increased number of categories increases the chance that a category will 
	be missed by samplers.
	* Boats are first stage samples within a stratum, with clusters used to avoid 
	sampling bias due to non-random sampling

* Sort requirements do not eliminate the need for port sampling.

* The proliferation of market categories over time in the sampled catch has 
not been matched with an increase in sampling effort, effectively reducing the 
average number of samples per category over time (Figure X). This reduces 
precision of catch estimates, increases uncertainty in stock assessment outputs
, and impedes efforts to monitor removals relative to catch targets.

* Fishermen and Dealers determine Market Categories for landed catch; issue 
with sampling can't get all categories; describe problem; sort requirements
 used to increase proportion of a particular species in a given market 
category, but other species are still landed in these categories (e.g. 
bocaccio in Figure X); can improve precision of important targets, but is not 
practical for large numbers of species; even for major targets, DOESN'T 
ELIMINATE THE NEED FOR SAMPLING; cite example of Dover sole rex sole is 
small fraction, but of a HUGE landing; decline in sampling effort; need for 
model-based approach to impute missing strata; current approach is ad-hoc.

* Statistical framework; focus of estimation is the total landed catch, in 
weight, of a single species; extend Shelton et al. (20xx); model-based
allows for imputation, small-area estimation (Fey and Harriott); model 
selection based on predictive criteria; model averaging to account for model 
uncertainty; quantifies uncertainty.

* Model-based approach is best course of action given sparse data, but best 
solution is to increase sampling or reduce the number of strata.

* Recommend cost/benefit analysis to help identify optimal number of market 
categories given management goals.


<!--
* Prediction: tails lighter due to inla?
	* higher degree Laplace approximation will better capture higher moments of posterior and thus may better capture information in the tails 
	* high $\rho$ in 259 and over prediction in 259? 
	* lower rho might predict more appropriately?

* Model averaging
	* Interpret/Speculate
	* flexibiliy
	* spatial
-->

<!--
Furthermore the struture of the models presented here allow for 
easy aggregation of estimated landings to any level within the stratification system. 


* Statistical framework; focus of estimation is the total landed catch, in 
weight, of a single species; extend Shelton et al. (20xx); model-based – 
allows for imputation, small-area estimation (Fey and Harriott); model 
selection based on predictive criteria; model averaging to account for model 
uncertainty; quantifies uncertainty.

* General Model based argument 
	*[check] Justify Model based statistics
	*[check] Biase/Variance trade off; hierarchical modeling
	*[check] Why Shelton didn’t work
	* Variance Estimates; 	
-->

# Appendix 

## Appendix A: Modern Market Categories and Sampling 

<!--
![panel: The relative sampling effort and landings across rockfish market categories 
accounting for the top 99% percent of landings for four time periods, 
1978-1982, 1983-1990, 1991-1999, and 2000-2015. In the top panel of each figure 
the black line represents the proportion of landings in each market category, 
while the blue line represents the proportion of samples, by number, taken in 
each market category. In the bottom panel of each modeled period, colors 
represent the proportion of sampled weight of thirteen commercially relevant 
species.  Alternating dark and light grey panels fill in all other species. On 
the top of each bar the number of observed species in each market category is 
listed.  Hatched regions indicated the nominal species of a market category 
where applicable.](./pictures/mockUp.png)
![early](./pictures/1978to1982Bar3.pdf))
![best](./pictures/1983to1990Bar3.pdf)
-->

![late: The relative sampling effort and landings across rockfish market categories 
accounting for the top 99% percent of landings for four time periods, 
1991-1999 and 2000-2015. In the top panel of each figure 
the black line represents the proportion of landings in each market category, 
while the blue line represents the proportion of samples, by number, taken in 
each market category. In the bottom panel of each modeled period, colors 
represent the proportion of sampled weight of thirteen commercially relevant 
species.  Alternating dark and light grey panels fill in all other species. On 
the top of each bar the number of observed species in each market category is 
listed.  Hatched regions indicated the nominal species of a market category 
where applicable.](./pictures/1991to1999Bar3.pdf)
![worst](./pictures/2000to2015Bar3.pdf)

Figures(bar91) and (bar00) show structure in port sampling data for two modern 
unmodled time periods, 1991-1999 and 2000-2015. These modern time periods show 
similar patterns in port sampling effort as the above describes modeled time 
periods, although due to the increased number of market categories port 
sampling is spread more thinly amoung the many categories. Namely port 
sampling effort still seems to track both total landed weight, as well as the 
number of species in each market category, however this pattern occurs across 
many more market categories.

In these two modern time periods a key feature of the data is the proliferation 
of the number of market categories. Figures(bar91) and (bar00) show market 
categories accounting for the top 99% of total landings in each time period. In 
the modeled periods, 1978-1982 and 1983-1990, the top 99% of total landings are 
landed into 6 and 12 distinct market categories respectively. In the unmodeled 
periods shown here, 1991-1999 and 2000-2015, the top 99% of total landings are
landed into 20 and 28 distinct market categories respectively.
  
It was noted in Section (Characteristics of the Landings Data) that market 
category 267 (nomially Brown Rockfish) was actually composed of a relatively 
little Brown Rockfish, while instead the market category actually contains 
mostly Widow Rockfish. Figure (bar00) shows that in the most modern time 
period (2000-2015) this pattern reverses as market category 267 is composed 
almost entirely (99.6%) of Brown Rockfish. 

## Appendix B: A Motivating Example

<!--
### An Example

## Data Generating Model
### Example

m=0
for(id in unique(Dat[Dat$marketCategory==mct & Dat$year==yer & Dat$portComplex==plc & Dat$gearGroup==ger,]$sampleNumber)){
	m=m+max(Dat[Dat$marketCategory==mct & Dat$year==yer & Dat$portComplex==plc & Dat$gearGroup==ger & Dat$sampleNumber==id,]$clusterNumber)
}
-->

<!--the port sampling integer weight--> 
                                                                          
<!--$(any will work)$--> 
<!--38 times by port samplers, collecting a total of 67-->
<!--344 model observations across 21 $(at least; URCK)$ unique species.-->

To discern between these discrete modeling options we considered Poisson, 
binomial, negative binomial, and beta-binomial models fit to a subset of the 		
data from market category 250, in the Monterey port complex trawl fishery for 
the second quarter of 1982. This stratum was selected as a relatively data rich 
setting, although other stratum produce similar results. This stratum was 
visited 32 times by port samplers, collecting a total of 59 cluster samples 
across 55 unique species. For brevity, in this example, we only consider the 
six most prevalent species (BCAC, CLPR, WDOW, YTRK, BANK, STRK). 

Simplified models under each of the discrete likelihoods, metioned above, are 
fit to the subset data. 

$$y_{ij}\sim p(y_{ij}|\theta_j, \phi)$$

Here $p$ takes the form of each of the considered Poisson, binomial, negative 
binomial, and beta-binomial models, $\theta_j$ represents the fixed species 
parameters, and $\phi$ is included to generally represent the nuisance 
parameters for modeling overdispersion in the negative binomial, and 
beta-binomial models.

               Poisson        Binomial      NB                 BB  
------------- -------------  ------------- ------------------ -------------
MSE            0.06412        0.06264       0.05171            0.04479
$\Delta$ DIC   1001.41        1230.60       5.03               0             <!--1342.27  1571.46  345.89  340.86-->
$\Delta$ WAIC  1079.95        1323.75       3.43               0             <!--1421.61  1665.41  345.09  341.66-->
$pr(M|y)$      $\approx0$     $\approx0$    $\approx10^{-7}$   $\approx1$

Table(likelihood) shows Mean Squared Error (MSE; computed on the species 
composition scale), delta deviance information criterion ($\Delta$ DIC), delta 
widely applicable information criterion ($\Delta$ WAIC), and marginal Bayesian 
model probabilities ($pr(M|y)$) across the likelihood models fit. These 
measures span a wide range of model selection philosophies and yet here they 
all consistently agree in ranking the likelihood models. 


<!--for the previously described 
subset, market category 250 for the Monterey Trawl fishery in 1982. Here we 
consider

Both of the overdispersion 
models considered here (NB and BB) outperform the more standard Poisson and 
binomial count models, with the most support being for the BB model and the 
Poisson model showing the least support. 
-->

Table($likelihood table$) show a clear preference for the overdispersed models 
(NB and BB), with the most overall support for the beta-binomial model and the 
Poisson model showing the least support. This initial result guides the use of 
the beta-binomial data generating model for the purposes of building a model 
to apply at an operational scale.

![Violin Plot](./pictures/compVioplotQtr2.png)

Figure(violin) shows the beta-binomial predictive distributions as a violin 
plot, with the observed species compositions, from port sampling, plotted atop 
each density. Figure(violin) demonstrates how the beta-binomial model 
distributes predictive density over the unit interval. Species composition is 
bounded on [0, 1], thus in the presence of large variability, predictive 
density may aggregate around the bounds.
 
<!--
For each of the above mentioned models, 
-->
Figure(Interval Plot) visualizes the predictive species composition 
distributions as 95% Highest Density Intervals (HDI) (colored vertical lines), 
plotted on top of the predictive means for each model and the observed species 
compositions (black horizontal lines) from the data in Figure($Interval Plot$). 

![
Interval Plot: The predictive species composition distributions as 95% Highest 
Density Intervals (HDI) (colored vertical lines), plotted on top of the 
predictive means for each model and the observed species compositions (black 
horizontal lines) from the data
](./pictures/compPlot1982Qtr2.png)

The large spread of the observed species compositions seen in Figure(Interval Plot) 
visually demonstrate the degree of overdispersion present in port sampling 
data. The Poisson and binomial models disregard this overdispersion to 
prioritize fitting the data mean. The NB and BB models explicitly model 
overdispersion in the data, and as such they predict a larger subset of the 
data. Notably, only the intervals produced by the BB model include the low 
observed proportions of bocaccio (BCAC) and the high observed proportion of 
chilipepper rockfish (CLPR) in this example.

The split beta-binomial intervals seen in Figure(Interval plot) reflect a 
large amount of residual variability confined on the unit interval. The 
beta-binomial is the only model considered here, that estimates such a large 
degree of variability and thus it is the only model that produces predictive 
species composition distributions that effectively cover the range of observed 
species compositions. The predictive intervals in Figure(likelihoods) are 
the smallest possible regions on each of the densities visualizes in 
Figure (Violin) so that each intervals contain 95% probability. For the 
example of STRK, notice that although the predictive HDI in Figure (Interval) is 
split, the vast majority of density (seen in Figure (Violin)) lies directly 
atop the data.

<!--
Each of the above mentioned models are 
fit to these data, and the preliminary results seen in Table($likelihood$) 
guide the use of the BB likelihood model moving forward.
-->
<!--
For each of the above mentioned models the predictive species composition 
distributions are visualized in Figure($Interval Plot$) as 95% Highest Density Intervals 
(HDI) $(citations)$, plotted on top of the predictive means for each model and 
the observed species compositions from the data in Figure($Interval Plot$). For brevity we 
only consider the most prevalent six species in this example 
(CLPR, BCAC, WDOW, BLGL, ARRA, BANK). Additionally, the MSE, DIC, WAIC, and 
Bayesian marginal likelihood model probabilities are computed for each model 
as measures of model fit as seen in Table($likelihood table$). 

Table($likelihood table$) show a clear preference for the overdispersed models, with the most 
overall support for the beta-binomial model. This initial result guides the 
use of the beta-binomial data generating model for the purposes of building a 
model to apply at an operational scale.
-->

## Appendix C: Prior Predictive

As a final check of the model structure and the implied prior information the 
prior predictive is considered. The prior predictive distribution summarizes 
the information that is intrinsic to the model structure itself, in the absence 
of data. The prior predictive of modeled weight is considered over a 100 pound 
cluster size, which is consistent with aggregating the two nominal 50 pound 
cluster samples described by Sen (1984) in the original sampling protocol. 

![Prior Prediction](./pictures/priorPredict.png)

As seen in Figure$(priorPredictive)$ the prior predictive of (M4) is both 
symmetric and quite diffuse over the 100 pound domain. The U shape of the 
distribution is a side effect of the diffusion of the selected prior. As data 
are added to the model, the indecisive U shape <!--of this distribution-->
collapses toward the data in the posterior. 

## Appendix D: Spatial Model Averaging

Here the spatial model averaging results are shown for all modeled time periods 
(1978-1982 and 1983-1990) in all modeled market categories. For brevity we only show 
the five most highly weighted models in each mcarket category, although 274 candidate 
port complex partioning schemes are computed in each market category. 

Notice that each market category has fairly unique port complex partitioning 
results. The fact that each market category behaves uniquely indicates the 
complexity of this system. Furthermore the fact that the BMA strategry picks 
up on these varied market category behaviors indicates the flexibility of this 
approach. Although the system is very dynamic, key breaks along California 
biogeographic features, such as Cape Mendicino and/or Point Conception, seem to 
be recurrent patterns. 

![modelSelect](./pictures/latexTableCompress1.png)
![modelSelect](./pictures/latexTableCompress2.png)
![modelSelect](./pictures/latexTableCompress3.png)
![modelSelect](./pictures/latexTableCompress4.png)

## Appendinx E: Nuisance Parameters

### 78-82
#### $\rho$
<!--195 & 0.92 & 0.93 & 0.036       \\-->
\begin{tabular}{|c|c|c|c|}
MCAT & Mean & Median & Posterior SD     \\ \hline
250 & 0.55 & 0.55 & 0.004       \\
253 & 0.39 & 0.39 & 0.001       \\
262 & 0.35 & 0.35 & 0.008       \\
265 & 0.64 & 0.64 & 0.002       \\
269 & 0.52 & 0.52 & 0.019       \\
270 & 0.53 & 0.54 & 0.020       \\
956 & 0.35 & 0.35 & 0.007       \\
959 & 0.47 & 0.47 & 0.070       \\
961 & 0.55 & 0.55 & 0.004       \\
\hline
\end{tabular}

#### $v$
<!--195 & 18666.56 & 18817.95 & 4676.48     \\-->
\begin{tabular}{|c|c|c|c|}
MCAT & Mean & Median & Posterior SD     \\ \hline
250 & 12915.85 & 18523.12 & 8699.87     \\
253 & 22747.87 & 23063.76 & 1535.53     \\
262 & 20254.41 & 20506.36 & 2581.87     \\
265 & 15846.22 & 16694.98 & 7601.15     \\
269 & 20135.05 & 19975.15 & 4667.11     \\
270 & 19931.96 & 19955.13 & 6033.35     \\
956 & 19659.11 & 19795.60 & 1227.99     \\
959 & 19159.69 & 13375.80 & 19256.94    \\
961 & 18631.44 & 19498.31 & 7970.44     \\
\hline
\end{tabular}

### 83-90
#### $\rho$
\begin{tabular}{|c|c|c|c|}
MCAT & Mean & Median & Posterior SD     \\ \hline
245 & 0.65 & 0.65 & 0.014       \\
250 & 0.51 & 0.51 & 0.002       \\
253 & 0.47 & 0.47 & 0.010       \\
259 & 0.75 & 0.75 & 0.009       \\
262 & 0.41 & 0.41 & 0.001       \\
269 & 0.57 & 0.57 & 0.046       \\
270 & 0.74 & 0.75 & 0.027       \\
663 & 0.51 & 0.51 & 0.001       \\
667 & 0.49 & 0.49 & 0.022       \\
956 & 0.43 & 0.43 & 0.003       \\
959 & 0.55 & 0.55 & 0.004       \\
960 & 0.45 & 0.45 & 0.004       \\
961 & 0.59 & 0.59 & 0.001       \\
\hline
\end{tabular}

* high $\rho$ in 259 and over prediction in 259? 
* lower rho might predict more appropriately?

#### $v$
\begin{tabular}{|c|c|c|c|}
MCAT & Mean & Median & Posterior SD     \\ \hline
245 & 20211.82 & 20204.95 & 1276.83     \\
250 & 236.03   & 192.53   & 134.67  	\\
253 & 20455.18 & 20140.50 & 1521.72     \\
259 & 20246.14 & 20186.61 & 898.99      \\
262 & 20445.49 & 20348.56 & 343.70      \\
269 & 34386.49 & 25951.03 & 24030.32    \\
270 & 20253.34 & 19908.07 & 9269.02     \\
663 & 19563.87 & 19624.09 & 331.04      \\
667 & 20089.55 & 20078.27 & 2723.34     \\
956 & 20581.67 & 20664.71 & 913.92      \\
959 & 19242.41 & 18707.09 & 5076.03     \\
960 & 20059.66 & 20012.80 & 1703.89     \\
961 & 20127.69 & 20141.04 & 580.80      \\
\hline
\end{tabular}

* Degree of smoothing (hierarchical parameters, rho)
* Report mean/variance of example
* Add $v$ and $\rho$ tables across all runs





<!--
## Data Generating Model
### Example

Table(likelihood) shows model performance metrics for the previously described 
subset, market category 250 for the Monterey Trawl fishery in 1982. Here we 
consider Mean Squared Error (MSE; computed on the species composition scale), 
deviance information criterion (DIC), widely applicable information criterion 
(WAIC), and marginal Bayesian model probabilities ($pr(M|y)$). These measures 
span a wide range of model selection philosophies and yet here they all 
consistently agree in ranking the likelihood models. 
-->
<!--
Both of the overdispersion 
models considered here (NB and BB) outperform the more standard Poisson and 
binomial count models, with the most support being for the BB model and the 
Poisson model showing the least support. 
-->
<!--
Table($likelihood table$) show a clear preference for the overdispersed models 
(NB and BB), with the most overall support for the beta-binomial model and the 
Poisson model showing the least support. This initial result guides the use of 
the beta-binomial data generating model for the purposes of building a model 
to apply at an operational scale.
-->
<!--
For each of the above mentioned models, Figure(Interval Plot) showsvisualizes 
the predictive species composition distributions as 95% Highest Density 
Intervals (HDI) (colored vertical lines), plotted on top of the predictive 
means for each model and the observed species compositions (black horizontal 
lines) from the data in Figure($Interval Plot$). 
-->
<!--
![
Interval Plot: The predictive species composition distributions as 95% Highest 
Density Intervals (HDI) (colored vertical lines), plotted on top of the 
predictive means for each model and the observed species compositions (black 
horizontal lines) from the data
](./pictures/compPlot1982Qtr2.png)

The large spread of the observed species compositions seen in Figure(Interval Plot) 
visually demonstrate the degree of overdispersion present in port sampling 
data. The Poisson and binomial models disregard this overdispersion to 
prioritize fitting the data mean. The NB and BB models explicitly model 
overdispersion in the data, and as such they predict a larger subset of the 
data. Notably, only the intervals produced by the BB model include the low 
observed proportions of bocaccio (BCAC) and the high observed proportion of 
chilipepper rockfish (CLPR) in this example.

![Violin Plot](./pictures/compVioplotQtr2.png)

The split beta-binomial intervals seen in Figure(Interval plot) reflect a 
large amount of residual variability confined on the unit interval. The 
beta-binomial is the only model considered here, that estimates such a large 
degree of variability and thus it is the only model that produces predictive 
species composition distributions that effectively cover the range of observed 
species compositions. Figure(violin) shows the beta-binomial predictive 
distributions as a violin plot demonstrating how the beta-binomial model 
distributes predictive density over the unit interval. The predictive intervals 
in Figure(likelihoods) are the smallest possible regions on each of these 
densities so that each intervals contain 95% probability. Species composition 
is bounded on [0, 1], thus in the presence of large variability predictive 
density may aggregate around the bounds. For the example of STRK, notice that 
although the predictive HDI is split, the vast majority of density lies 
directly atop the data.
-->


<!--
For brevity we only consider 
the most prevalent six species in this example (CLPR, BCAC, WDOW, BLGL, ARRA, 
BANK). 
-->


<!--
Figure(likelihoods) shows observed species 
compositions plotted against Poisson, Binomial, NB, and BB predictive 
intervals. Here 95% highest 
density intervals (HDI) are shown to visualize the predictive accuracy of each 
model against the data.
-->

<!--
 Additionally, the MSE, DIC, WAIC, and 
Bayesian marginal likelihood model probabilities are computed for each model 
as measures of model fit as seen in Table($likelihood table$). 

Table($likelihood table$) show a clear preference for the overdispersed models, with the most 
overall support for the beta-binomial model. This initial result guides the 
use of the beta-binomial data generating model for the purposes of building a 
model to apply at an operational scale.
-->
<!--
expanded landings.
weight toward the overall expansion landings for 


the models are constructed 
so that these cases represent cases where relatively low landings are present and 

gets around the fact that there is always an important empirical 
component to these models

Since model based strategies of data analysis are reliant not only on a
model, but also the models ability the to learn from data, we do our best 
to only apply the models in cases where it is reasonable to expect  models to 
produce valid results. For example in some market categories 
and time periods our models may end up having more parameters that 


ah sample is aggregated across the 
observed clusters

* General Structure of Data
* where can I put something about sub-setting MCATs?
-->
<!--
It is helpful to notice that for a given modeled period all market categories are 
modeled over the same number of years and quarters; furthermore all port complexes 
and gear groups are present in each market category. Thus considering 
Eq($linear predictor$) the primary difference in the number of parameters between 
market categories comes from the number of species present. 

Assuming that it is desirable to better characterize market categories with larger 
landings. If the number of species in each market category were the same, one would 
expect sampling effort to roughly track landings, so as to achieve more samples per 
parameter in more heavily landed market categories. In Figure $(bars)$ we roughly 
observe this pattern. However it is clear that deviations from this pattern appear 
when many more or less species are present in a market category. These deviations 
take samples away from market categories with relatively few parameters and add 
samples to market categories with relatively greater numbers of parameters so as to 
keep the overall number of samples per parameter in track with landed weight. 
-->

<!--
In market except where in market categories 
where the number of species 
the number of species both track 
Thinking of the proportion landed weight as a proxy for commer
within a modeled period is the number of observed species
-->
<!--
###(M2)
$$\beta^{(t)}_{m\eta} = \beta^{(y)}_{m} + \beta^{(q)}_{\eta}$$
$$\beta^{(y)}_{m} \sim N(0, 32^2)$$
$$\beta^{(q)}_{\eta} \sim N(0, v^{(q)})$$

(M2) represents a fixed effects model for year parameters, but estimates a 
single hierarchical variance parameter, $v^{(q)}$, shared among the 
$\beta^{(q)}_{\eta}$. $v^{(q)}$ has the effect of partially pooling 
information among all quarters. The actual degree of pooling is determined 
from the data, through the way the data shapes the $v^{(q)}$ posterior. 

###M3
$$\beta^{(t)}_{m\eta} = \beta^{(y)}_{m} + \beta^{(q)}_{\eta}$$
$$\beta^{(y)}_{m} \sim N(0, v^{(y)})$$
$$\beta^{(q)}_{\eta} \sim N(0, 32^2)$$

(M3) represents a fixed effects model for quarter parameters, but estimates a 
single hierarchical variance parameter, $v^{(y)}$, shared among the 
$\beta^{(y)}_{m}$. $v^{(y)}$ has the effect of partially pooling 
information among years but not quarters.
-->

<!--
 not between
quarters.within a common , across the quarters of that year, but since
each year is assigned a separate variance parameter no pooling is possible
between years.

 however here interaction terms are allowed 
to partially pool across years, within a common quarter, but not between 
quarters. (M7) involves fitting slightly more parameters than (M6) because 
in this setting we model more than 4 years of data, however it offers the 
potential to decrease estimate biases if data within quarters is more similar 
than data within years.
-->


<!--across port complexes calls for spatial pooling only when 
forced to fill holes brought about by unsampled strata. Naturally, such a 
pooling protocol introduces bias in species composition estimates 

 which depends on the 
availability of data in each stratum and thus makes comparisons between 
periods with pooled and unpooled data inconsistent with each other. 
Furthermore, the current ad-hoc "borrowing" protocol makes it difficult to 
know exactly when "borrowing" has occurred.
-->

<!-- while minimizes issues of inconsistent comparisons between stratum.-->

<!--
within a modeled period, brought about by the pooling scheme itself.
 by framing it as a model uncertainty problem.
This has the benefit that  represents  it minimizes inconsistently within model periods

a model  so as to minimize any 
inconsistency brought about by the pooling scheme itself. 
-->


<!--
additionally due to the fact that partitions are 
stationary in time, provides consistency within the modeled period.
-->



<!--
California, within a particular market category and modeling time period, 
provides more consistency among the modeled period than previously available. 
Additionally his formulation of port complex pooling provides a mathematical 
structure for considering the space of possible pooled models. 

it is unknown which ports are appropriate to pool and how the     
To formalize the idea of port pooling 

We only consider pooling strategies 
which remain consistent within the modeled period and explicitly outline 
state-wide pooling strategies, rather than protocols that change from stratum 
to stratum. That is to say, for a particular market category and modeling time 
period, port pooling is defined as some partition of the set of port complexes in 
California. This formulation does not make it clear which ports should be 
pooled together in the partitioning of the state, however this formulation does 
achieve consistency among port pooling within market categories and modeled time 
periods, while providing a mathematical structure for considering the space 
of possible pooled models.  
-->



<!--
In this 
setting BMA accounts for model uncertainty around the port partitioning 
decision. As used here BMA effectively integrates across the possible 
biologically relevant modeling decisions with to port complex pooling. 

 For the $\mu^{th}$ model, of
model space $\mathbb{M}$, a straight forward implementation of Bayes theorem
gives,
$$Pr(\mathbb{M}_\mu|y) = \frac{ p(y|\mathbb{M}_\mu)p(\mathbb{M}_\mu) }{ \sum_\mu p(y|\mathbb{M}_\mu)p(\mathbb{M}_\mu) } = \omega_\mu$$
Where $\omega_\mu$ is the posterior probability that model $\mu$ is the true
data generating model of the data, conditional on the subspace of candidate
models and the observed data. $\omega_\mu$ is then straightforwardly used to
average together the posteriors of all of the candidate models, as follows
$\bar p(\theta|y) = \sum_{\mu} \omega_\mu p(\theta|y, \mathbb{M}_\mu).$


Since set partitioning is finite and scales in a well understood way number of ways to partition port 
complexes BMA amounts to a discrete approximation of integrating 

Once the models have been 
computed the posteriors and posterior predictive distributions from each model 
 
be combined through the use of Bayesian Model Averaging (BMA) among the
candidate models. Bayesian model averaging used in this setting accounts for 
model uncertainty around the port partitioning decision. and when all relevant models have 
been computed 
these difficult mo while combining the respective predictive
capabilities of each model of a given subset of model space
(Hoeting et al., 1999).
-->
 

<!--, provides concrete quantitative support for, or against, each of these
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


combined with the formalized process of Bayesian Model
Averaging (BMA) to appropriately integrate port-complex pooling model
uncertainty into species composition estimates (Hoeting et al. , 1999). 





<!--

The most straight-forward solution in the presence of this type of model
uncertainty is to compute all $B_K$ possible pooling schemes.
However, maybe not all pooling schemes represent biologically relevant models.
For example, perhaps it is reasonable to pool only among adjacent ports, or to
assert that biologically similar regions can only extend across a small number
of ports (if so, how many?).
-->




<!--
The straight-forward spatial model implied by the categorical port complex 
variables do not adequately resolve in-sample prediction at the observed 
sample sizes. 
Presently these diminishingly small within stratum sample sizes are managed 
by an ad-hoc "borrowing" protocol outlined by Pearson and Erwin (1997).
We aim to formalize this "borrowing" idea via an exhaustive search of spatially 
pooled models, combined with the formalized process of Bayesian Model 
Averaging (BMA) to appropriately integrate port-complex pooling model 
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
To achieve consistency in pooling schemes, the protocol for pooling   


consider 
We aim to formalize the fundamental model uncertainty around port complex, while 
enjoying any possible predictive benefits from the exploration of data pooling 
across port complex boundaries.

Rather than only considering spatially pooled models when data become 
prohibitively sparse, we open that possibility that pooling may benefit


In the absence of Citation employes exploring pooling 
options in space  are extraordinarily sparse. 
-->


<!--
* how to deal with ports
	* model uncertainty around port
	* bell number for exploration
	* constrained exploration
	* Bayesian model averaging
-->




<!--
 best as possible No additional biogeographic 
information about which particular ports may be pooled is encoded so as to 
test each partition against each other based solely on relative model 
performance. 
-->



<!--
           Poisson        Binomial      NB                 BB  
--------- -------------  ------------- ------------------ -------------
MSE         0.05286        0.05683      0.05188            0.05170
DIC         5675.25        6759.86      1301.51            1261.00
WAIC        5840.56        6939.74      1302.19            1261.30
$pr(M|y)$   $\approx0$     $\approx0$   $\approx10^{-16}$  $\approx1$
-->

<!--NB pr(M|y)=5.175555e-17-->
<!--
           Poisson        Binomial      NB                 BB  
--------- -------------  ------------- ------------------ -------------
MSE        0.06412        0.06264       0.05171            0.04479
DIC        1342.27        1571.46       345.89             340.86
WAIC       1421.61        1665.41       345.09             341.66
$pr(M|y)$  $\approx0$     $\approx0$    $\approx10^{-7}$     $\approx1$
-->
<!--7.778048e-221 1.782425e-270  1.028494e-07  9.999999e-01-->
<!-- -692.62, -806.92, -201.89, -185.80 -->


<!--
active binomial and beta-binomial models out perform the overdispersed 
Poisson and binomial models. Furthermore, all of the metrics in 
Table(1) indicate that the beta-binomial model outperforms the negative 
binomial model. 
-->
<!--are also able to discern-->



<!--
 Because the 
species composition scale is bounded on [0, 1], in the presence of large variablity predictive density may 
aggregate around the bounds   (i.e. 
these regions represent the densest packing of 95% probability under each 
predictive distribution). 
-->

<!--
The Poisson and binomial models attempt to model both the mean and 
residual variances with a single parameter for each species. This can tend to 
bias mean estimates, in overdispersed stratum, toward larger values, for the 
sake of estimating larger residual variances. Similarly variance estimates may 
tend to be biased toward smaller values for the sake of estimating smaller 
means.
-->
<!--
In contrast, the negative binomial and beta-binomial models estimate an 
additional parameter which is intended to disentangle the mean and residual 
variance estimates. Thus the negative binomial and beta-binomial models are 
able to produce more accurate estimates of both the mean and residual 
variance.
-->

<!--
All of the measures in Table(1) consistently agree that the negative binomial 
and beta-binomial models out perform the overdispersed Poisson and binomial 
models. Furthermore, all of the metrics in Table(1) indicate 
that the beta-binomial model outperforms the negative binomial model. 

Depending 
on the users value system toward model selection (e.g. predictive or 
inferential), the support for the beta-binomial model over the negative 
binomial model may vary, but it is worth noting that the more robust model 
selection tools show stronger support for the beta-binomial model, with Bayesian 
model probabilities indicating practically conclusive support for the 
beta-binomial model. 
-->

<!--
For the cases of Aurora and Bank rockfish, the 
empty upper regions seen in Figure(1) are understandable in terms of the 
relatively low density region of the posterior they represent, as seen in 
Figure(2).
-->

<!--
From M1 to M4 all model selection criterion consistently support the 
inclusion of hierarchical main effects on both year and quarter parameters. 
As various model forms for interaction term are added in M5, M6, and M7 the 
data seem to support prior structures which involve the potential for a higher 
degree of partial pooling while still supporting the inclusion of year-quarter 
interactions. All model selection criterion agree that M5 shows the highest 
degree of support. Presumably M5 strikes a balance between maximal potential for 
hierarchical pooling, while still maintaining the ability to model differences in 
seasonality from year to year.

$Choose Model M4$
-->
<!--
##78-82
    68%   95%   99%
--- ----- ----- -----
195 76.3% 82.4% 90.8%
250 13.3% 23.3% 27.5%
253 16.6% 27.2% 35.4%
262 10.5% 24.6% 35.2%
265 64.8% 82.4% 86.8%
269 26.9% 37.5% 46.2%
270 30.4% 48.8% 51.2%
956 22.5% 40.5% 55.5%
959 38.9% 74.7% 84.2%
961 30.4% 54.3% 65.2% 

##83-90
    68%   95%   99%
--- ----- ----- -----
245 
250 
253 
259  
262  
269 
270 
663 
667 
956  
959  NA   NA    NA
960  NA   NA    NA
961  NA   NA    NA
-->


<!--
* General Products
* Degree of smoothing (hierarchical parameters)
* Posterior v. Current
	* Report degree of similarity
* Prediction v. Data
	* Report predictive accuracy
-->

<!--
# Discussion

* General Math/Science
* Database Stuff
-->




































<!-- 
* Partitioning results
	* WAIC => Biogeography
	* Pr(M) => model averaging

* Prediction v. Data
	* Report predictive accuracy
-->

<!--The posteriors from each model imply different predictive species composition distributions.-->
<!--
The predictive species composition distributions from each model are 
visualized in Figure (1) as 95% Highest Density Intervals (HDI) $(citations)$, 
plotted on top of the predictive means for each model and the observed species 
compositions from the data in Figure(1). For brevity we only consider the most 
prevalent six species in this example (CLPR, BCAC, WDOW, BLGL, ARRA, BANK). 
Additionally, the 
MSE, DIC, WAIC, and Bayesian marginal likelihood model probabilities are 
computed for each model as measures of model fit as seen in Table(1).
-->
<!--
![Interval Plot](./pictures/compPlot.pdf)

           Poisson        Binomial      NB                 BB  
--------- -------------  ------------- ------------------ -------------
MSE         0.05286        0.05683      0.05188            0.05170
DIC         5675.25        6759.86      1301.51            1261.00
WAIC        5840.56        6939.74      1302.19            1261.30
$pr(M|y)$   $\approx0$     $\approx0$   $\approx10^{-16}$  $\approx1$   
-->					 
<!--NB pr(M|y)=5.175555e-17-->
<!--
The large spread of the observed species compositions seen in Figure(1) 
visually demonstrate the degree of overdispersion present in port sampling 
data. The Poisson and binomial models disregard this overdispersion to 
prioritize fitting the data mean.
-->

<!--
 can tend to 
bias mean estimates, in overdispersed stratum, toward larger values, for the 
sake of estimating larger residual variances. Similarly variance estimates may 
tend to be biased toward smaller values for the sake of estimating smaller 
means. 
-->

<!--
All of the measures in Table(1) consistently agree that the negative binomial 
and beta-binomial models out perform the overdispersed Poisson and binomial 
models. Furthermore, all of the metrics in Table(1) indicate that the beta-binomial model outperforms the negative binomial model. Depending 
on the users value system toward model selection (e.g. predictive or 
inferential), the support for the beta-binomial model over the negative 
binomial model may vary, but it is worth noting that the more robust model 
selection tools show stronger support for the beta-binomial model, with Bayesian 
model probabilities indicating practically conclusive support for the 
beta-binomial model. 

The split beta-binomial intervals seen in Figure(1) are the consequence of 
confining a large amount of residual variability to the unit interval. The 
beta-binomial is the only model considered here, which estimates such a 
large degree of variability and thus it is the only model that produces 
predictive species composition distributions of the sort. Figure(2) shows the 
beta-binomial predictive distributions as a violin plot demonstrating how the
beta-binomial model arranges predictive density over the unit interval. The 
predictive intervals in Figure(1) are the smallest possible regions on each 
density so that the intervals contain 95% of the predictive density (i.e. 
these regions represent the densest packing of 95% probability under each 
predictive distribution). For the cases of Aurora and Bank rockfish, the 
empty upper regions seen in Figure(1) are understandable in terms of the 
relatively low density region of the posterior they represent, as seen in 
Figure(2).

![Violin Plot](./pictures/compVioplot.pdf)
-->



<!--0.01948392 0.98055003--> 

<!--
$$v \sim IG(1,~2\times10^{3}) ~~~\forall~~~v$$.  

the model applies to 
stratum contained within each hierarchy
 depending on 
the posteriors of these variance parameters the each model learns  stratum contained within each hierarchy maybe

Any and all hierarchical variance 
parameters are estimated from the data and thus distributed 

$$v \sim IG(1,~2\times10^{3}) ~~~\forall~~~v$$
-->
<!--
$$\text{logit}(\rho) \sim N(0, 2^2)$$
-->

<!--
Finally, a year-quarter interaction 
($\beta^{(y:q)}_{m\eta}$) is included to give this model the flexibility 
to model differing seasonality from year to year. In addition to offering 
flexibility in modeling seasonalities, the year-quarter interaction provides 
an ideal structure for partially pooling data through time via a hierarchical 
prior discussed later in $Section(XX)/the following section$.
-->
<!--
* justify linear predictor/transition to priors
-->




<!--  
The data's suggestion that species compositions are quite variable 
it responds to the data's suggestion that species compositions are quite 
variable. When large variability is confined to the exist within the unit 
interval, such as species compositions, 


, not only from a predictive standpoint. 
definitive to slight. 


in agreement about 
discerning between the negative binomial and beta-binomial models.  
beta-binomial model outperforms the negative binomial model as well
discerning between the negative binomial and beta-binomial models as well 
although depending on which metric the 

The MSE scores in Table(1) demonstrate that the  that both the negative binomial and 
beta-binomial models are able to consistently out perform not only is this pattern visually true, but all metric



  In the most overdispersed 
cases Due to the presence of
overdispersion in  

* describe picture
* notice overdispersion
* improper variance model biases mean, beta-binomial is flexible to disentangles these effects.
-->
<!--data talk-->
<!--
See proposals...

* maths stuffs
	* mean function
	* variance; introduce $\rho$

For a particular market category, $y_{ijklm\eta}$ is the $i^{th}$ sample of 
the $j^{th}$ species' weight, in the $k^{th}$ port, caught with the $l^{th}$ 
gear, in the $\eta^{th}$ quarter, of year $m$. The $y_{ijklm\eta}$ are modeled 
as observations from a Beta-Binomial distribution ($BB$) conditional on 
parameters $\theta$ and $\rho$.
-->
<!--$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\bm{\theta}, \rho).$$-->
<!--$$y_{ijklm\eta} \sim BB(y_{ijklm\eta}|\theta, \rho).$$-->

<!--Justify: single stratum Poisson v. binomial v. negative binomial v. beta-binomial-->
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

Applying the Bayesian predictive framework to the above model gives the 
following expressions for predicted weight in each stratum,
$$p(y^*_{jklm\eta}|y) = \int\!\!\!\!\int\! \text{BB}\Big( y^*_{jklm\eta}|\theta_{jklm\eta}, \rho \Big) P\Big(\theta_{jklm\eta}, \rho | y\Big) d\theta_{jklm\eta} d\rho.$$
$p(y^*_{jklm\eta}|y)$ is computed via Monte Carlo integration and represents 
the model's full predictive distribution for the $j^{th}$ species' weight, in 
the $k^{th}$ port, caught with the $l^{th}$ gear, in the $\eta^{th}$ quarter, 
of year $m$.
Considering the same type of prediction across all species in a particular 
stratum allows for the calculation of predictive species compositions.
The following joint transformation of the species' predictive weights result 
in predictive species compositions, 
$$\pi^*_{jklm\eta} = \frac{y^*_{jklm\eta}}{\sum_j y^*_{jklm\eta}} ~~~ y^*_{klm\eta}\neq 0.$$
Because the $y^*$ are random variables, and $\pi^*$ is nothing more than a 
transformation of the $y^*$, $\pi^*$ is too a random variable. 
Furthermore once inference is complete, we can easily sample these 
distributions and compute any desired moments from these samples. 
-->

<!--Transformation-->
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
Presently these diminishingly small within stratum sample sizes are managed 
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
\bibitem{ccgs} CCGS (California Cooperative Groundfish Survey). (2017). 
Pacific States Marine Fisheries Commission, 350 Harbor Boulevard, Belmont, 
California, 94002. URL:  128.114.3.187.

%
\bibitem{gelman} Gelman, A., Carlin, J. B., Stern, H. S., \& Rubin, D. B. 
(2014). Bayesian data analysis (Vol. 2). Boca Raton, FL, USA: Chapman \& 
Hall/CRC.

%
\bibitem{nas} NAS (National Academies of Sciences, Engineering, and Medicine). 
2017. Review of the Marine Recreational Information Program. Washington, DC: 
The National Academies Press.

%
\bibitem{bma} Hoeting, J. A., Madigan, D., Raftery, A. E., \& Volinsky, C. T. 
(1999). Bayesian model averaging: a tutorial. Statistical science, 382-401.

%
\bibitem{jaynesBook} Jaynes, E. T. (2003). Probability theory: The logic of 
science. Cambridge university press.

%
\bibitem{bvBook} Ramasubramanian, K., \& Singh, A. (2016). Machine Learning 
Using R. Apress.

%
\bibitem{glmBook} McCullagh P. \& Nelder, J.A. (1989). Generalized Linear 
Models, 2nd ed. London: Chapman and Hall.

%
\bibitem{pearsonErwin} Pearson, D.E., and Erwin, B. (1997). Documentation of 
California’s commercial market sampling data entry and expansion programs. 
NOAA Tech Memo. NOAA-TM-NMFS-SWFSC-240.

%
\bibitem{rCoreTeam} R Core Team (2015). R: A language and environment for 
statistical computing. R Foundation for Statistical Computing, Vienna, 
Austria. URL https://www.R-project.org/.

%
\bibitem{inlaPackage} Rue H., Martino S., Lindgren F., Simpson D., Riebler A. 
(2013). R-INLA: Approximate Bayesian Inference using Integrated Nested Laplace 
Approximations.  Trondheim, Norway. URL 
http://www.r-inla.org/.

%
\bibitem{inlaMethod} Rue, H., Martino, S., \& Chopin, N. (2009). Approximate 
Bayesian inference for latent Gaussian models by using integrated nested 
Laplace approximations. Journal of the royal statistical society: 
Series b (statistical methodology), 71(2), 319-392.

%
\bibitem{senMemo} Sen, A.R. (1984). Sampling commercial rockfish landings in 
California. NOAA Tech Memo. NOAA-TM-NMFS-SWFSC-45.

%
\bibitem{senPaper} Sen AR. (1986). Methodological problems in sampling 
commercial rockfish landings. Fish Bull. 84: 409-421 .

%
\bibitem{sheltonEtAl} Shelton, A. O., Dick, E. J., Pearson, D. E., Ralston, 
S., \& Mangel, M. (2012). Estimating species composition and quantifying 
uncertainty in multispecies fisheries: hierarchical Bayesian models for 
stratified sampling protocols with missing data. Canadian Journal of Fisheries 
and Aquatic Sciences, 69(2), 231-246.

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
