---
title: Improving Catch Estimation Methods in Sparsely Sampled Mixed-Stock Fisheries.
author: Nick Grunloh, E.J. Dick, Don Pearson, John Field, Marc Mangel
documentclass: extarticle
geometry: margin=2cm
fontsize: 14pt
---

# Introduction

* I am Nick

* describe the california spp comps. port sampling data for modeling 

* describe our modeling efforts for estimating ssp comps.

# Request: Diagnostic

* Create fully stratified performace diagnostics based on my tabulated (tables 2 and 3) aggragate performance numbers.

* Quickly refresh on the model in question

* Tools for sorting through all of this information

* Diagnostics for evaluating performance

\clearpage

# Beta-Binomial Model

* $y_{ijklm\eta}$: $i^{\text{th}}$ sample of the $j^{\text{th}}$ species' integer weight, in the $k^{\text{th}}$ port, caught with the $l^{\text{th}}$ gear, in the $\eta^{\text{th}}$ \mbox{quarter,} of year $m$, for a particular market \mbox{category.}

* Stratum $\mu$ linked to $\theta$ and observed cluster size ($n$) 

* Stratum $\sigma^2$ is largely a function of $\mu$ but with overdispersion $\rho$
	
	* $\rho\rightarrow0$: Binomial variance
	* $\rho\rightarrow1$: $n$ times Binomial variance

* Modeling of $\theta$ (all predictors are categorical):
	
	* Intercept
	* Additive offsets for: Species, Port, Gear
	* Consider multiple time models

# Diagnostic Files

* Consider a toy example to get our hands dirty with the diagnostic.

* Recall the model (M4).

* Recall we had some model selection criterion. Here I show the DIC and WAIC information criterion not as a diagnostic, but merely to guide our search through the numerous models under consideration.

* Through out this document you will see green underlined tags. 
	
	* Depending on your pdf viewer, these lines may look slightly different and you may get slightly different behavior when clicking.
	* In any case these should be clickable links to various github pages.
	* If you are veiwing this in a browser you may prefer to [ctrl]-click to avoid redirecting away from the presentation tab.

* **click** We'll talk more about them later
	
	* marginal species directories (BCAC pdf and csv)
	* directories of various levels of stratification (pdfs) species-gear-year	
	* stratifcation csvs gearYearSpp68.csv

\clearpage

# MAD Diagnostic

* As we add more models there is a lot of information to sort through, consider the MAD diagnostic as a tool for sorting.
	
	* $\ell_i$: the landings in stratum $i$, 
	* $\mathcal{O}_{ij}$: the observed predictive accuracy of species $j$ in stratum $i$
	* $\aleph$: the nominal level of prediction for a particular model run

* Low MAD scores occur when $\ell_i$ is low -or- $\left|\mathcal{O}_{ij}-\aleph\right|$ is small.

* High MAD scores occur when $\ell_i$ is large and $\left|\mathcal{O}_{ij}-\aleph\right|$ is large.

* ???? example ????
	* High MAD v. Low MAD

# Stratum Plots

* Prediction shown at three levels of stratification
	
	* Disaggregated
	* By species, gear group, and year aggregating across port complexes, and quarters. ("data-rich assessment")
	* By species, and year aggregating across port complexes, gears, and quarters. ("data-moderate/poor assessment")
	* csv versions of these files are in base run directory.

* **click each**

# Diagnostic Wrap-up

* Marginal plots organized by species, each marginal stratum summed over everything else.

* Sort species by MAD, explore margins via margin plots

* Explore within margins via previously described stratum plots 

\clearpage

# Request: Sample Size 

* a request for Sample sizes by mcat and time block

* through out the rest of the requests we work with the top 3 landed mcats in 1978-1982
	
	* 250, 253, 269

* tables show number of port sampling sightings

* other mcats and higher stratifications are provided as supplemental excel files.

# MCAT 250 Sample Sizes

* observed species all time in mcat 250

	* note these are not multinomial sample sizes, but rather sighting occurances.
	* multinomial structure fills in zeros for all unsighted species in a particular sample id.

* We'll see that model performance will get some of the common species, while the less common species are very hard to predict.

	* Common: BCAC, CLPR, CNRY, WDOW, YTRK
	* Intermediate: BANK, BLGL, CWCD *often worrisome
	* Uncommon: BRNZ, MXRF

\clearpage

# MCAT 253 & 269 Sample Sizes

* MCAT 253:

	* Common: BCAC, CLPR, WDOW
	* Intermediate: SNOS, YTRK
	* Uncommon: BLGL, CWCD

* MCAT 269:

	* Common: WDOW
	* Intermediate: CLPR, YTRK // BCAC, CNRY
	* Uncommon: DBRK, POP

# Flatfish and Elasmobranchs

* number of port sampling sightings

* Largest landed Flatfish and Elasmobranchs

	* Sampling Flatfish since 2002
	* Sampling Elasmobranchs since 2009

* **See Flat/Elasmobranch Table**

# Request: Redo modeling w/o So-Cal

* Redo modeling in early time block MCAT 250 w/o Southern California
* Here we look at predictions from top species:
	* CLPR, CNRY, WDOW, YTRK

# Redo SoCal Summary

* Out of sample predictions do not effect observed strata.

* Small difference just come from slight run-by run variation

* When Sample sizes become very sparce it can cause slight model instability. 

# Request: Time Model & Prior Sensitivity

* Top landings MCATS in early time period: 250, 253, 269 
	
	* M models
	* Prior models

# Time Models

* Bayesian Modeling 
	* Heirarchical v. Random Effect Disclaimer

* (M1) Fixed main effect time model
	
	* No pooling

* (M2) Random main effect time model
	
	* years/quarter pool separately

* (M3) Random main effects + random interaction

* (M4) Random interactions jointly pooled

* (M5) Random interactions quarterly variances pooling across years

* (M6) Random interactions yearly variances pooling across quarters 

* All with default IG prior

\clearpage

# TIME MODEL: 250

* M2, M3, M4

* Least MAD worrisome: WDOW, BCAC, CLPR, CNRY
	
	* BCAC: 
		* Most of landings in TWL, in later years, in all qtrs, 
		* largest landings in BRG
		* generally good performance
	* WDOW:
		* Most landings in ERK, TWL, 1986, all qtrs
		* very good performance

* Most MAD worrisome: BRNZ, MXRF, BLGL, CWCD, BANK * consistent

	* CWCD:
		* MRO, HKL (some TWL), all years, spring
		* Over fitting: mostly 0s and interval contains 0
	* MXRF:
		* BRG, TWL, 1980, Winter/Spring
		* Very small sample sizes
			* wouldn't be surprised if some of difference are due to model instability
			* large variance: even a little instability could cause some what larger predictive differences.
			* flat likelihood in MXRK axis => small $\Delta$ likelihood across wide area.

\clearpage

# TIME MODEL: 253

* M4, M5, M6

* Least MAD worrisome: WDOW, BCAC, CLPR, CNRY, BANK * consistent

	* BCAC: 
		* TWL
		* predictions are relatively good
		* not a huge difference between models
	* WDOW:
		* TWL
		* slight overfitting, not bad fit

* Most MAD worrisome: DBRK, CWCD, YTRK, BLGL, SNOS * consistent

	* CWCD:
		* OSF/MRO, TWL, 1978/1981, winter/summer
		* Overfitting: Simpler models do better
			* Mostly zeros
	* DBRK:
		* OSF/MNT, TWL, 1980, summer
		* Underfitting: More complex model works better

\clearpage

# TIME MODEL: 269

* M4, M5, M6

* Few species, some spp show on best and worst
* Least MAD worrisome: YTRK, BCAC, CLPR, CNRY 
		
	* CNRY:
		* CRS, TWL, 1982, Q3
		* Overfitting: simpler model  
	* YTRK:
		* CRS/MRO, TWL, 1982, spring
		* almost no model sensativity
	
* Most MAD worrisome: WDOW, DBRK, POP
	* BCAC:
		* ERK/MNT, TWL, 1982, Fall/Summer
		* Overfitting
		* almost no model sensativity  
	* WDOW:  
		* BDG, TWL, 1982, spring
		* underfitting:?? 

\clearpage

# Request: Landings

* Aggregate across MCATs 250, 253, 269 by year and year:gear for each spp.
	* new model runs against calcom in black

* Only show select species realevant for management

* **WDOW**
	* little sensativity to M model
	* low estimates in TWL, 1979, 1980, 1981

* **BCAC**
	* little sensativeity
	* driven by TWL
	* reasonably small differences

* **CLPR**
	* little sensativity
	* driven by TWL (very similar)
	* other gears off a bit (S:G)

* **DBRK & CWCD**
	* little sensativity
	* lots of variance but basically similar
	* we can estimate variance! wouldn't be able to say that from calcom

* **MXRK**
	* suuuper skewed distributions
		* 9000 samples at 0 and some up to 1
		* high variance 
	
	* Bayesian inference estimating higher moments 	
		* complex posterior as a result of extreme lack of information
		* statistics are breaking down, yet mean not far from calcom. 
		* all of the instability is masked in calcom but the model can seee it.
			* we should want to see those failures in the data; the model does.

\clearpage

# Priors

* Diffuse priors

* Main effects diffuse Normals

* $\rho$ transformed to be a real number

	* $\text{logit}(\rho) \rightarrow (-3.91, 3.91)$
	* $\rho \rightarrow (0.02, 0.98)$  

* Any heirarchical variances:
	
	* Default: IG prior
	* Informative: $\sqrt{v}~$~ Half-Cauchy$(10^{1})$
	* Diffuse: $\sqrt{v}~$~ Half-Cauchy$(10^{3})$
	* Flat: $\sqrt{v}~$~ Unif$(0, 10^4)$

\clearpage

<!--
# Beta-Binomial Fits

* Fit model separately in 78-82 and 83-90 and compare model selection criterion

* 78-82:

	* Consistent support for more pooling
	* All measures point to (M4)

* 83-90:
	
	* Consistent support for interaction models
	* Uncertainty between (M3), (M4), and (M5)
	* Lesser support for (M6)

* We fit model (M4) everywhere
	
	* Stable and relatively fast model to fit
	* Given its support in 78-82, I am drawn to (M4)
		* Each time period seems to have a mind of its own  
\begin{eqnarray*}
&\beta^{(t)}_{m\eta} = \beta^{(y)}_{m} + \beta^{(q)}_{\eta} + \beta^{(y:q)}_{m\eta} & \\
&\beta^{(y)}_{m} \sim N(0, 32) & \\
&\beta^{(q)}_{\eta} \sim N(0, 32) & \\
&\beta^{(y:q)}_{m\eta} \sim N(0, v) &
\end{eqnarray*}

# ?? LUNCH ??

\clearpage

# Posterior Predictive Species Comps.

* Having settled on (M4) in both time periods, how do we build species comps?

* Inference results in samples from posterior distribution $P\Big(\mu_{jklm\eta}, \sigma^2_{jklm\eta} | y\Big)$

* Run samples back through BB likelihood to compute Monte Carlo integral and get posterior predictive distribution of sampled weight.

* Use draws from model posterior predictive weight to compute species comp. distribution
	
	* Plot shows average species compositions 
	* Full distribution for $y^*$ as well as $\pi^*$
	* Each sample sums to 1 and $\sum_j\mathbb{E}[\pi^*_j]$=1

* By adding an unobserved latent time period we can make out-of-sample predictions
	* (M4): unobserved $\beta^{(y)^*}$ and $\beta^{(q)^*}$

# Single Quarter Hindcast

* Recall for 1978-1982 there was no sampling south of point conception.

* Adding an unobserved year and quarter

	* make predictions for each species in each combo of: 
		* three observed gear groups 
		* three southern port complexes

\clearpage

# 78-82 Prediction

* Modeled MCATs

* MCATs in the rows **(ordered by landings)** w/ 3 nominal HDI prediction levels

	* For each stratum of each MCAT compare data to prediction intervals
	* Observed level should match Nomial
	* Prediction higher than nominal => Overfitting
	* Prediction lower => Underfitting (not enough residual variance) 
	
* Most do well
	* Average performance is reasonable
	* Note this is a unweighted, simple, average
	* More accurate would weight average by samples at each stratum 

* Particularly well in heavily landed stratum

	* correlation of sampling effort w/ landings

* Widow is a wild child
	* only example that is off by more than 5% points at any level

# 83-90 Prediction

* Same Table
	* Modeled MCATs **(ordered by landings)** w/ 3 nominal pred. levels

* Again most do well

* Recall landings were spread across more MCATs in 83-90
	* Enough samples to also model more MCATs

* Blackgill, Yellowtail, Cowcod: off by 5% points at some level
	* Negligable Landings

# Speciating Landings

* $\lambda_{\cdot klm\eta}$ is reported on landing reciepts

* $\lambda^*_{jklm\eta}$ stored in DB

* Aggregate to any level
	
	* across quarter, port complex, gear group
	* Also MCAT (I ran out of index variables :/)

* E.J. will show the speciated time series with predictive intervals 
	
	* summed across MCAT
	* as it might be used in asessment

\clearpage

# BMA Story

* Mentioned partial pooling thru time via heirarchical modeling

* But present system also pools in space
	* Given sparcity, it's entirely possible that we also need spatial pooling 

* I show MSE to demonstrate the biase/variance trade off
	
	* Pooling directly exchanges sample size (postior variance) with bias
	* **Far Right** Least Bias
	* **Far Left** Most Bias, but most data for small posterior variance
	* A practicle solution is somewhere in between

* [Bell number] Idea: Try all partions of port complexes

* $\text{B}_{10}=115975$
	
	* Too many
	* add Spatial Modeling Constraints 
	* Biogeography viewed through the lens of human behavior
		* sampling behavior
		* fisherman behavior

* $\bar{\text{B}}_{10}=61136$
	
	* Require partitions to be "small"
	* No super-grouping greater than 3 port complexes
	* points close in space behave similarly (smoothness)

* $\hat{\text{B}}_{10}=512$
	
	* Require continuous partitions (no leap frogging)
	* Like a GP continunity constraint

* $\hat{\bar{\text{B}}}_{10}=274$

	* Together we have "small" and "continuous" partitions
	* smoothness and continuity 
	* a computationally manageable set of models to compute

\clearpage

# BMA Math
	
* Defines a candidate model set

* We could just pick the single "best" model

	* defining "best" is hard
	* model selection criterion are imperfect 

* Prediction results are averaged results

# 78-82 BMA Results

* Describe plot

* Recall no sampling in the south
	* All latent structure filled in by predictive distribution in the south

* 250:
	* Marginal model probability 
		* N1: 32+14+13+12=71%
		* N2: 2+2+2+2=8%

* 253:
	* Central Block
	* BRG/BDG
	* Lump/Split CRS and ERK (among top 5 models; 58% model weight)
		* Split: 0.3448276
		* Lump: 0.6551724

* 269:
	* Sold on the BRG-BDG break

\clearpage

# 83-90 BMA Results

* Recall BRG-OSF Missing data

* 250:
	
	* Missing data story
		* Lump or
		* Quarantine	

* 956:
	
	* Lump/Split CRS and ERK
	* A break at Cape Mendicino
	* BRG/BDG/OSF Quarantine os missing data

* 269:
	
	* Piont Conception Break
	* Cape Mendicino Break


# Conclusions

* Using Bayesian models we have:
	
	* Account for overdispersion
	* Estimate uncertainty (full distribution)
	* Formal Mechanisms for pooling
	* provide structure for making out-of-sample prediction

* Future Modeling
	
	* Explore additional predictore in $\theta$
		* Landing weighting
		* Vessel Effects
		* Speceies:Gear interactions
	
	* Overdispersion Multivate models
		* Dirichelette-Multinomial Model
	
	* Maybe Time Series Models
	
	* Cluster and integrate out spatial parameters via DP?

-->






<!--
# MCATs in Time

* **Top Panel:** Number of samples in rockfish market categories (1978-2015)
	
	* Colors represent different market categories
	* Thickness shows the number of samples

* **Bottom Panel:** Number of rockfish market categories
	
	* Count the colors
	* ~20 mcats in the late 70s
	* ~50 mcats in the recent times

* **Middle Panel:** Average number of samples per stratum
	
	* Find samples for each stratum (mcat, gear, port, year, qtr)
	* Average them

* 1978-1982 

* 1983-1990  

\clearpage

# 78-82 Bars

* **Top Panel:** 
	
	* For each market category accounting for 99% of landings  
		* (blue) Proportion landings by weight
		* (red) Proportion samples by #

* **Bottom Panel:** Aggregated Species Compositions
	* Colors represent 13 select species (others grey)
	* Number above is the # species present
	* Hatching is MCAT nominal species 

* MCATs not pure
	* often nominal species is not even the major species
		* BCAC
		* BRWN

* Sampling Opportunistic
	* Sampling co-occurs with landings
	* Often as more species are present there are more samples
	* This is lucky for modeling
		* More samples than parameters (largely driven by spp)
		* Most landings are modeled (78-82: 96.8%)

* No sampling south of Conception

# 83-90 Bars

* Same picture but 83-90
	
	* (blue) Proportion landings by weight
	* (red) Proportion samples by #
	* Aggregated Species Compositions

* Top 99% of landings in more market categories
	
	* MCATs still largely impure

* 83-90: 98.3% of landings modeled


\clearpage

# Likelihood Forms

* First modeling choice: Pick a Likelihood

* Shelton etal. 2012 Fit Multinomial via the Multinomial-Poisson trans.
	
	* Piece together independent Poissons

* We are not limited to Multinomial distribution
	
	* quantify uncertainty (residual variability)
	* consider modeling overdispersion
	* additional parameter ($\phi$) to disentangle mean from variance

* $y_{ij}$: $i^{\text{th}}$ sample of the $j^{\text{th}}$ species' integer
weight

* Remove all other modeling decisions by modeling a single stratum
	
	* MCAT 250
	* Montery
	* Trawl
	* 1982/Q2

# Likelihood Graphs

* Fit models and look are how they predict

* **Left Pannel:** 95% HDI from each model along side observed sppComp data

	* black horizontal lines are observed species comps
	* blue is Possion (i.e. Multinomial) Model
	* red is Binomial
	* green is the Negative Binomial Model
	* yellow is the Beta-binomial Model

* **Right Panel:** Entire Beta-binomial predictive distribution

* Overdispersion is present (spp comps from [0,1])

* ~50 obsevations => 2.5 missing in 95% interval

	* Maybe NB missing a few to many, and BB missing a few to few
	* BB certaintly finding the most variance
	* split intervals but... very appropriate density

# Likelihood Table

* Consider MSE, DIC, WAIC, and Marginal Likelihood Bayesian Model Prob.

* Varied model selection criterion (Nothing is perfect!) 

* Consistent and large support for the Overdispersion Models
	
	* Most support for BB

* Moving forward I develop the BB model   





-->
