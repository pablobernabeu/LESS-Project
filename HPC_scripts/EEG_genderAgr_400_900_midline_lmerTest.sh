#!/bin/bash

#SBATCH --account=nn10008k
#SBATCH --job-name=EEG_genderAgr_MiniEng_400_900_midline_lmerTest
#SBATCH --partition=normal
#SBATCH --mem=90G
#SBATCH --ntasks=1
#SBATCH --time=7-0:00:00
#SBATCH -o EEG_genderAgr_MiniEng_400_900_midline_lmerTest.out

# it is good to have the following lines in any bash script
set -o errexit  # make bash exit on any error
set -o nounset  # treat unset variables as errors

module restore
module load R-bundle-CRAN/2024.06-foss-2023b

Rscript 'analyses/EEG_genderAgr_MiniEng_400_900_midline_lmerTest.R' > 'EEG_genderAgr_MiniEng_400_900_midline_lmerTest.Rout'
