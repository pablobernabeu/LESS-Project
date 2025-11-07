#!/bin/bash

# Complete workflow to fix container and install packages
# Run this from your personal space on HPC where less_container.sif is located

set -e  # Exit on error

echo "=========================================="
echo "LESS Container Dependency Fix"
echo "=========================================="
echo ""
echo "This script will:"
echo "1. Convert SIF to writable sandbox"
echo "2. Install R packages using mamba"
echo "3. Rebuild the SIF container"
echo "4. Verify installations"
echo ""
read -p "Continue? (y/n) " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    echo "Cancelled."
    exit 1
fi

# Check if container exists
if [ ! -f "less_container.sif" ]; then
    echo "ERROR: less_container.sif not found in current directory"
    echo "Current directory: $(pwd)"
    exit 1
fi

echo ""
echo "=========================================="
echo "STEP 1: Creating writable sandbox"
echo "=========================================="
singularity build --sandbox --fix-perms less_sandbox less_container.sif

echo ""
echo "=========================================="
echo "STEP 2: Installing packages in container"
echo "=========================================="
echo "This will take 5-10 minutes..."

singularity exec --fakeroot --writable less_sandbox /bin/bash << 'INSTALL_SCRIPT'
set -e
echo "Activating conda environment..."
source /opt/less/start.sh

echo ""
echo "Installing R packages with mamba..."
mamba install -c conda-forge -y \
    r-rcppeigen \
    r-dplyr \
    r-readxl \
    r-lme4 \
    r-lmertest \
    r-emmeans \
    r-mumin

echo ""
echo "Verifying installations..."
Rscript -e "
packages <- c('dplyr', 'readxl', 'RcppEigen', 'lme4', 'lmerTest', 'emmeans', 'MuMIn')
for (pkg in packages) {
  library(pkg, character.only = TRUE)
  cat(sprintf('✓ %s (%s)\n', pkg, packageVersion(pkg)))
}
cat('\n✓ All packages loaded successfully!\n')
"
INSTALL_SCRIPT

echo ""
echo "=========================================="
echo "STEP 3: Rebuilding SIF container"
echo "=========================================="
echo "This will take 5-10 minutes..."
singularity build less_container_new.sif less_sandbox

echo ""
echo "=========================================="
echo "STEP 4: Testing new container"
echo "=========================================="
singularity exec less_container_new.sif Rscript -e "
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)
cat('SUCCESS: Container is ready!\n')
"

echo ""
echo "=========================================="
echo "STEP 5: Replacing old container"
echo "=========================================="
echo "Backing up old container..."
mv less_container.sif less_container_OLD_$(date +%Y%m%d_%H%M%S).sif
mv less_container_new.sif less_container.sif

echo ""
echo "=========================================="
echo "STEP 6: Cleanup"
echo "=========================================="
read -p "Remove sandbox directory to save space? (y/n) " -n 1 -r
echo ""
if [[ $REPLY =~ ^[Yy]$ ]]; then
    rm -rf less_sandbox
    echo "Sandbox removed."
else
    echo "Sandbox kept at: less_sandbox"
fi

echo ""
echo "=========================================="
echo "COMPLETE!"
echo "=========================================="
echo ""
echo "Your container is ready at: less_container.sif"
echo ""
echo "Next steps:"
echo "1. Test with: bash HPC\ scripts/check_container_setup.sh"
echo "2. Submit job: sbatch HPC\ scripts/EEG_VONA_consolidated_lmerTest.sh"
echo ""
