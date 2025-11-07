#!/bin/bash

#SBATCH --account=nn10008k
#SBATCH --job-name=EEG_VONA_consolidated
#SBATCH --partition=bigmem
#SBATCH --mem=90G
#SBATCH --ntasks=1
#SBATCH --time=7-0:00:00
#SBATCH -o EEG_VONA_consolidated.out

# Best practices for bash scripts
set -o errexit  # Exit on any error
set -o nounset  # Treat unset variables as errors

# For debugging
set -x  # Print commands as they execute

echo "=========================================="
echo "Starting Consolidated VONA EEG Analysis"
echo "=========================================="
echo "Job started at: $(date)"
echo "Running on node: $(hostname)"
echo ""

# Define paths
# Personal space: where scripts and container are located
# SLURM's SUBMITDIR contains the directory from which sbatch was called
HOST_PROJECT_DIR="${SLURM_SUBMIT_DIR:-/cluster/work/users/pbe044/LESS}"
CONTAINER_SIF="${HOST_PROJECT_DIR}/less_container.sif"

# R script path inside container (mounted from personal space)
CONTAINER_R_SCRIPT="/project_root/analyses/EEG/test_revised_analysis/EEG_VONA_lmerTest.R"

# Log file for R script output
R_SCRIPT_LOG="EEG_VONA_consolidated.log"

echo "Personal space (scripts): ${HOST_PROJECT_DIR}"
echo "Container: ${CONTAINER_SIF}"
echo "R script: ${CONTAINER_R_SCRIPT}"
echo "Project space (data): /cluster/projects/nn10008k/LESS"
echo ""

# Verify container exists
if [ ! -f "${CONTAINER_SIF}" ]; then
    echo "ERROR: Container not found at ${CONTAINER_SIF}"
    exit 1
fi

echo "Starting R analysis inside Singularity container..."
echo "Output will be saved to: ${R_SCRIPT_LOG}"
echo ""

# Test that we can write to log file
touch "${R_SCRIPT_LOG}"
echo "Log file created at: $(pwd)/${R_SCRIPT_LOG}"

# Run the R script inside the container
# Bind mounts:
#   - Personal space (${HOST_PROJECT_DIR}) → /project_root (for scripts)
#   - Project space (/cluster/projects/nn10008k/LESS) → same path (for data/results)
echo "Executing singularity command..."
singularity exec \
    --env RENV_CONFIG_AUTOLOADER_ENABLED=FALSE \
    --bind "${HOST_PROJECT_DIR}":/project_root \
    --bind /cluster/projects/nn10008k/LESS:/cluster/projects/nn10008k/LESS \
    "${CONTAINER_SIF}" \
    Rscript "${CONTAINER_R_SCRIPT}" \
    > "${R_SCRIPT_LOG}" 2>&1

RSCRIPT_EXIT_CODE=$?
echo "R script exit code: ${RSCRIPT_EXIT_CODE}"

if [ ${RSCRIPT_EXIT_CODE} -ne 0 ]; then
    echo "ERROR: R script failed with exit code ${RSCRIPT_EXIT_CODE}"
    echo "Check log file: ${R_SCRIPT_LOG}"
    cat "${R_SCRIPT_LOG}"
    exit ${RSCRIPT_EXIT_CODE}
fi

echo ""
echo "=========================================="
echo "Job completed at: $(date)"
echo "=========================================="
