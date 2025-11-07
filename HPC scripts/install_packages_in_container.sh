#!/bin/bash

# Script to be run INSIDE the writable container to install all required R packages
# Usage: After entering container with 'singularity shell --fakeroot --writable less_sandbox'
#        run: bash /path/to/this/script

echo "=========================================="
echo "Installing R packages in container"
echo "=========================================="
echo ""

# Activate conda environment
echo "Activating conda environment..."
source /opt/less/start.sh

echo ""
echo "Conda environment info:"
which mamba
which R
R --version | head -n 1

echo ""
echo "=========================================="
echo "Installing core dependencies first"
echo "=========================================="

# Install in dependency order to avoid issues
mamba install -c conda-forge -y \
    r-rcpp \
    r-rcppeigen

echo ""
echo "Installing main packages..."
mamba install -c conda-forge -y \
    r-dplyr \
    r-readxl \
    r-matrix \
    r-lme4 \
    r-lmertest \
    r-emmeans \
    r-mumin

echo ""
echo "=========================================="
echo "Verifying installations"
echo "=========================================="

Rscript -e "
cat('Testing package loads...\n\n')

packages <- c('dplyr', 'readxl', 'RcppEigen', 'lme4', 'lmerTest', 'emmeans', 'MuMIn')
success <- TRUE

for (pkg in packages) {
  result <- tryCatch({
    library(pkg, character.only = TRUE)
    version <- packageVersion(pkg)
    cat(sprintf('✓ %s (version %s)\n', pkg, version))
    TRUE
  }, error = function(e) {
    cat(sprintf('✗ %s FAILED: %s\n', pkg, e\$message))
    FALSE
  })
  success <- success && result
}

cat('\n')
if (success) {
  cat('SUCCESS: All packages loaded successfully!\n')
  
  # Test allFit function
  cat('Testing allFit function...\n')
  data(sleepstudy, package='lme4')
  tryCatch({
    fm1 <- lmer(Reaction ~ Days + (Days|Subject), sleepstudy)
    cat('✓ lmer model fitted\n')
    cat('✓ allFit function available:', 'allFit' %in% ls('package:lme4'), '\n')
  }, error = function(e) {
    cat('✗ Model fitting failed:', e\$message, '\n')
  })
  
} else {
  cat('FAILURE: Some packages failed to load\n')
  quit(status = 1)
}
"

echo ""
echo "=========================================="
echo "Package installation complete!"
echo "=========================================="
echo ""
echo "Next steps:"
echo "1. Exit this container: exit"
echo "2. Rebuild SIF: singularity build less_container_new.sif less_sandbox"
echo "3. Test: singularity exec less_container_new.sif Rscript -e 'library(lme4); library(emmeans)'"
