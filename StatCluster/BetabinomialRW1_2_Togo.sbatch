#!/bin/bash
#SBATCH --job-name TGO2
#SBATCH --partition largemem
#SBATCH --ntasks 1
#SBATCH --time 0-11:00
#SBATCH --mem-per-cpu=80000
#SBATCH -o betabin_%j.out
#SBATCH -e betabin_%j.err
#SBATCH --mail-type=ALL
#SBATCH --mail-user=jlg0003@uw.edu

module load rgdal
module load R
R CMD BATCH BetabinomialRW1_2_Togo.R
