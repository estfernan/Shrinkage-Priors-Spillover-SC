# Shrinkage-Priors-Spillover-SC

Synthetic control (SC) methods are widely used to assess the impact of policy interventions by constructing an artificial (*synthetic*) unit that closely mirrors the characteristics of the treated region in the absence of the intervention.
Typically, neighboring regions spatially closed to the treated unit are selected as potential controls due to their similarities. 
However, when the intervention indirectly influences these neighboring controls (i.e., spills over), the resulting estimates can be easily biased. 

To address this challenge, we introduce a Bayesian SC method with distance-based shrinkage priors, designed to estimate causal effects even in the presence of spillovers.
Our approach extends conventional penalization techniques by incorporating a weighted distance function that considers both covariate information and spatial proximity to the treated unit.
Rather than simply excluding nearby controls, our method data-adaptively selects controls that are less likely to be affected by spillovers, balancing the need for accurate estimation with bias reduction.
We demonstrate the finite-sample properties of our method under varying levels of spillover through simulation studies. 
We then apply this approach to evaluate the impact of Philadelphia's beverage tax on the sales of sugar-sweetened and artificially sweetened beverages in mass merchandise stores.

This repository contains the code and sample data needed to reproduce the results in the manuscript.[^1]

## Data Availability

We use beverage sales data from the NielsenIQ Retail Scanner Data, curated by the Kilts Center at the University of Chicago Booth School of Business. 
Details on registering for data access can be found [here](https://www.chicagobooth.edu/research/kilts/research-data/nielseniq).

We provide sample data in `data/sample_sales.RDS`, which resembles the structure of the real data presented in the manuscript. 
This panel dataset contains multiple time points across various three-digit zip code areas, including Philadelphia and other areas in Pennsylvania, Delaware, Maryland, and New Jersey. 
The sample data is randomly generated and intended solely for illustrative purposes, not for reproducing the study's results.

## Code for Reproducibility

Supporting functions are located in the `pkg/` directory.
The core models implementing the two distance-based shrinkage priors are available in the `pkg/stan/` directory.
Scripts for generating the main figures in the manuscript are labeled by figure number, while figures for the supplementary material are labeled by content.

### Key Scripts:

* `scripts/analysis.R`: This script reads the real or sample data to estimate the causal effect of Philadelphia's beverage tax on the sales of sugar-sweetened and artificially sweetened beverages in mass merchandise stores, using the two distance-based shrinkage priors. Running this script will produce the application results needed to replicate [Figure 2](https://github.com/estfernan/Shrinkage-Priors-Spillover-SC/blob/main/scripts/make_fig02.R) and [Figure S6](https://github.com/estfernan/Shrinkage-Priors-Spillover-SC/blob/main/scripts/val_cutoffs.R).

* `scripts/main.R`: This script runs a single replication of the numerical experiments detailed in the manuscript, by generating simulated data and estimating the treatment effect of an intervention. The script uses the two distance-based shrinkage priors alongside two comparison methods, producing the simulation results needed to replicate [Figure 1](https://github.com/estfernan/Shrinkage-Priors-Spillover-SC/blob/main/scripts/make_fig01.R) and [Figure S1](https://github.com/estfernan/Shrinkage-Priors-Spillover-SC/blob/main/scripts/val_width_RMSE.R).

## Instructions for General Use

To efficiently run a large number of independent replications for the numerical experiments, we used a high-performance computing cluster ([Oscar](https://docs.ccv.brown.edu/oscar)).
The script `scripts/analysis.R` accepts the following arguments:

* `T_0`: Number of pre-intervention periods in the simulated data.
* `J`: Number of control units in the simulated data.
* `kappa_d`: Importance weight for the weighted distance function.
* `mu_d`: Mean difference for generating distance-dependent covariates between neighboring and non-neighboring controls.

For example, to specify 30 pre-intervention periods, 50 control units, an importance weight of zero (i.e., considering only distance to the treated unit), and no covariate differences between neighboring and non-neighboring controls, you can use the following command in a Slurm script:

```bash
Rscript --vanilla scripts/main.R 30 50 0.0 0.0
```

To load the sample data file `data/sample_sales.RDS`, you can use the following R code:

```r
beverage_sales <- readRDS("data/sample_sales.RDS")
dim(beverage_sales)  # shows the number of time points and units
colnames(beverage_sales)  # lists the three-digit zip code areas
```

The `beverage_sales` object can be used to run both `scripts/analysis.R` and `scripts/val_cutoffs.R`.


[^1]: The sections assume familiarity with the methodology and setting described in the manuscript.
