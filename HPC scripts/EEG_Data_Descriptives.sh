#!/bin/bash

#SBATCH --account=nn10008k
#SBATCH --job-name=EEG_Data_Descriptives
#SBATCH --partition=bigmem
#SBATCH --mem=1000G
#SBATCH --ntasks=1
#SBATCH --time=7-0:00:00
#SBATCH -o EEG_data_descriptives.out 

# it is good to have the following lines in any bash script
set -o errexit  # make bash exit on any error
set -o nounset  # treat unset variables as errors

echo "Starting Singularity job at $(date)"

# Define paths relative to the directory from which you submit the SLURM job.
# This is usually the project directory where your script and SIF are.
# SLURM automatically sets the current working directory to where you submit the job.

HOST_PROJECT_DIR="$(pwd)"
CONTAINER_SIF="./less_container.sif"
# The path to the R script *inside* the container, based on your bind mount
CONTAINER_R_SCRIPT="/project_root/data/EEG data descriptives.R"
# The log file for the R script's output, relative to your job submission directory
R_SCRIPT_LOG="EEG_data_descriptives.log"

# --- Main Command: Running R script inside Singularity via SLURM ---
# The standard output and standard error from *this* command will go to the SLURM -o file (EEG_data_descriptives.out).
# The output *from the R script itself* will be redirected to EEG_data_descriptives.log.
singularity exec \
    --bind "${HOST_PROJECT_DIR}":/project_root \
    --bind /cluster/projects/nn10008k/LESS:/cluster/projects/nn10008k/LESS \
    "${CONTAINER_SIF}" \
    Rscript "${CONTAINER_R_SCRIPT}" \
    > "${R_SCRIPT_LOG}" 2>&1

echo "Singularity job finished at $(date)"
