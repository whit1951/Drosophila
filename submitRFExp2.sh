#!/bin/bash
#

#SBATCH --job-name=RF2

#SBATCH --partition=sesync

#SBATCH --time=15:00:00
Rscript --vanilla DrosophilaRFExp2.R