#!/bin/bash

# Step-by-step guide to fix R package dependencies in the container
# Follow these commands on the HPC to rebuild the container with all required packages

echo "=========================================="
echo "LESS Container Rebuild Guide"
echo "=========================================="
echo ""
echo "This script provides commands to fix dependency issues in the container."
echo "Run these commands MANUALLY, one section at a time."
echo ""

cat << 'EOF'

STEP 1: Convert SIF to writable sandbox
----------------------------------------
singularity build --sandbox --fix-perms less_sandbox less_container.sif

This creates a writable directory called 'less_sandbox' that you can modify.


STEP 2: Enter the container in writable mode
---------------------------------------------
singularity shell --fakeroot --writable less_sandbox

You should now see a Singularity> prompt.


STEP 3: Activate the Conda environment (inside container)
----------------------------------------------------------
source /opt/less/start.sh

This activates the R environment with mamba/conda available.


STEP 4: Install missing system dependencies (inside container)
---------------------------------------------------------------
# Use mamba (faster than conda) to install R packages with all dependencies
# This avoids the dependency hell you're experiencing

mamba install -c conda-forge -y \
    r-dplyr \
    r-readxl \
    r-rcppeigen \
    r-lme4 \
    r-lmertest \
    r-emmeans \
    r-mumin

# Note: allFit is not a standalone package, it's part of lme4
# The allFit() function is available after loading lme4


STEP 5: Verify installations (inside container, still in R environment)
------------------------------------------------------------------------
R

# Inside R, test each package:
library(dplyr)
library(readxl)
library(RcppEigen)
library(lme4)
library(lmerTest)
library(emmeans)
library(MuMIn)

# Test allFit
data(sleepstudy, package="lme4")
fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
allFit(fm1)

# If all load successfully:
q()  # quit R


STEP 6: Exit the container
---------------------------
exit

You should now be back at your normal HPC prompt.


STEP 7: Rebuild the SIF container
----------------------------------
# This converts the modified sandbox back to an immutable .sif file
singularity build less_container_new.sif less_sandbox

This may take several minutes.


STEP 8: Test the new container
-------------------------------
# Run a quick test to verify packages work
singularity exec less_container_new.sif Rscript -e "library(lme4); library(emmeans); cat('SUCCESS!\n')"

If you see "SUCCESS!" then the container is ready.


STEP 9: Replace old container and cleanup
------------------------------------------
# Backup old container (optional)
mv less_container.sif less_container_old.sif

# Use new container
mv less_container_new.sif less_container.sif

# Remove sandbox (saves space)
rm -rf less_sandbox

# Remove old backup (optional, once you've verified new container works)
# rm less_container_old.sif


STEP 10: Verify with your actual script
----------------------------------------
bash HPC\ scripts/check_container_setup.sh


========================================
ALTERNATIVE: Install via R (if mamba approach fails)
========================================

If the mamba installation has issues, you can install packages directly in R
within the writable container:

singularity shell --fakeroot --writable less_sandbox
source /opt/less/start.sh
R

# Inside R:
install.packages(c("dplyr", "readxl"), 
                 repos="https://cloud.r-project.org/", 
                 dependencies=TRUE)

# For packages that need compilation:
install.packages("RcppEigen", 
                 repos="https://cloud.r-project.org/",
                 dependencies=TRUE, 
                 type="source")

install.packages("lme4", 
                 repos="https://cloud.r-project.org/",
                 dependencies=TRUE)

install.packages("lmerTest", 
                 repos="https://cloud.r-project.org/",
                 dependencies=TRUE)

install.packages("emmeans", 
                 repos="https://cloud.r-project.org/",
                 dependencies=TRUE)

install.packages("MuMIn", 
                 repos="https://cloud.r-project.org/",
                 dependencies=TRUE)

q()
exit

# Then continue from STEP 7 above


========================================
TROUBLESHOOTING
========================================

Issue: "No space left on device"
Solution: Check disk quota with 'dusage' and clean up old files

Issue: "Permission denied" 
Solution: Make sure to use --fakeroot flag with singularity shell

Issue: Packages still not found after rebuild
Solution: Check that /opt/less/start.sh actually activates the environment
         Try: singularity exec less_container.sif which R

Issue: Compilation errors
Solution: Use mamba/conda approach instead of install.packages()
         Conda packages are pre-compiled and include all system dependencies

EOF
