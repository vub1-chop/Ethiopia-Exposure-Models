#!/bin/bash
#SBATCH --job-name=AODGF_047_7_0500_Train
#SBATCH --nodes=1
#SBATCH --ntasks=1                # One task using multiple threads
#SBATCH --cpus-per-task=5         # Request 5 cores for multithreading
#SBATCH --mem-per-cpu=196G        # Request 200GB of memory per core
#SBATCH --mail-type=BEGIN,END,FAIL
#SBATCh --mail-user=vub1@chop.edu
#SBATCH --time=7-00:00:00
#SBATCH --output=AODGF_047_7_0500_Train.out
#SBATCH --error=AODGF_047_7_0500_Train.err

module load R/4.2.1-foss-2022a.lua

Rscript --vanilla RF_AODGF_047_7_0500_Train.R