#!/bin/bash
#SBATCH --job-name=T47_S2
#SBATCH --output=svm_train_047_set2.out
#SBATCH --error=svm_train_047_set2.err
#SBATCH --time=3-00:00:00
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=75
#SBATCH --mem=980G
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCH --mail-user=vub1@chop.edu

module load R/4.2.1-foss-2022a.lua

echo "Starting job at $(date)"

Rscript --vanilla SVM_AODGF_047_Set2.R

echo "Job completed at $(date)"