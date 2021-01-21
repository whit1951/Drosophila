# Genotype and sex-based host variation in behaviour and susceptibility drives population disease dynamics
Simulation of disease on contact networks derived from empirical Drosophila data infected with Drosophila C Virus (DCV) for White LA, Siva-Jothy JA,
Craft ME, Vale PF. 2020 Genotype and sex-based host variation in behaviour and susceptibility drives population disease dynamics. **Proc. R. Soc. B** 20201653.
http://dx.doi.org/10.1098/rspb.2020.1653

[![DOI](https://zenodo.org/badge/135700079.svg)](https://zenodo.org/badge/latestdoi/135700079)

## Basic Functions
* `Exp1functions.R` - Experiment 1 functions needed to run in parallel on supercomputer
* `Exp2functions.R` - Experiment 2 functions needed to run in parallel on supercomputer
* `Exp3functions.R` - Experiment 2 functions needed to run in parallel on supercomputer

## Supercomputer
* `Exp1Parallel.R`- Run Experiment 1 in parallel via clusters
* `Exp2Parallel.R`- Run Experiment 2 in parallel via clusters
* `Exp3Parallel.R`- Run Experiment 3 in parallel via clusters

* `Exp1Parallel.txt`- bash script for submitting `Exp1Parallel.R`
* `Exp2Parallel.txt`- bash script for submitting `Exp2Parallel.R`
* `Exp3Parallel.txt`- bash script for submitting `Exp3Parallel.R`

## Random Forest Analysis
* `DrosophilaRF.R` -  Random forest analysis for Drosophila simulations: Experiment 1
* `DrosophilaRF2.R` - Random forest analysis for Drosophila simulations: Experiment 2
* `DrosophilaRF3.R` - Random forest analysis for Drosophila simulations: Experiment 3

## Plotting
* `DrosophilaMSIFileCleaning.R`- cleans and combines raw data outputed from the supercomputer
* `DrosophilaAnalysis.R` - creates heatmap plots for Experiments #1, 2 & 3
* `PlotRF.R` - code for plotting variable importance results from random forest analysis for Experiments #1, 2 & 3

## Results
* `Experiment1Summary.csv` - compiled simulation data for Experiment 1
* `Experiment2Summary.csv` - compiled simulation data for Experiment 2
* `Experiment3Summary.csv` - compiled simulation data for Experiment 3

* `Exp#beta...tiff` - Heat map results for each experiment with different parameter combinations
* `Exp#RF_R0...csv`- Random forest results (variable importance scores) for R0
* `Exp#RF_dur...csv`- Random forest results (variable importance scores) for epidemic duration
* `Exp#RF_logit...csv`- Random forest results (variable importance scores) for whether or not outbreak spread beyond initially infected individual
* `Exp#RF_maxI...csv`- Random forest results (variable importance scores) for maximum number of infected individuals

