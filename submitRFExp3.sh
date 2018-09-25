#!/bin/bash
#

#SBATCH --job-name=RF3

#SBATCH --partition=sesync

#SBATCH --time=15:00:00
Rscript --vanilla DrosophilaRFExp3.R