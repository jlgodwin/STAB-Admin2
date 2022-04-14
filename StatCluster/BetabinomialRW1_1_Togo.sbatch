#!/bin/bash
#SBATCH --job-name TGO1
#SBATCH --partition largemem
#SBATCH --ntasks 1
#SBATCH --time 0-11:00
#SBATCH --mem-per-cpu=60000
#SBATCH -o betabin_%j.out
#SBATCH -e betabin_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jlg0003@uw.edu

module load rgdal
module load R
R CMD BATCH BetabinomialRW1_1_Togo.R
