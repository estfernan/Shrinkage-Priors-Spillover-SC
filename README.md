# Shrinkage-Priors-Spillover-SC

Synthetic control (SC) methods are widely used to assess the impact of policy interventions by constructing an artificial (*synthetic*) unit that closely mirrors the characteristics of the treated region in the absence of the intervention.
Typically, neighboring regions spatially closed to the treated unit are selected as potential controls due to their similarities. 
However, when the intervention indirectly influences these neighboring controls (i.e., spills over), the resulting estimates can be easily biased. 

To address this challenge, we introduce a Bayesian SC method with distance-based shrinkage priors, designed to estimate causal effects even in the presence of spillovers.
Our approach extends conventional penalization techniques by incorporating a weighted distance function that considers both covariate information and spatial proximity to the treated unit.
Rather than simply excluding nearby controls, our method data-adaptively selects controls that are less likely to be affected by spillovers, balancing the need for accurate estimation with bias reduction.
We demonstrate the finite-sample properties of our method under varying levels of spillover through simulation studies. 
We then apply this approach to evaluate the impact of Philadelphia's beverage tax on the sales of sugar-sweetened and artificially sweetened beverages in mass merchandise stores.

This repository provides the code and sample data that can reproduce the results in the manuscript.

## Data Availability

We use beverage sales data from the NielsenIQ Retail Scanner Data (NRSD), curated by the Kilts Center at the University of Chicago Booth School of Business. 
Details on registering for data access can be found [here](https://www.chicagobooth.edu/research/kilts/research-data/nielseniq).

We provide sample data in `data/sample_sales.RDS`, which resembles the structure of the real data presented in the manuscript. 
This panel dataset contains multiple time points across various three-digit zip code areas, including Philadelphia and other areas in Pennsylvania, Delaware, Maryland, and New Jersey. 
The sample data is randomly generated and intended solely for illustrative purposes, not for reproducing the study's results.

