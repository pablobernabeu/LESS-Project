#!/bin/bash

# Install all packages needed for LESS project data processing and analysis
# Run this inside the writable container

echo "=========================================="
echo "Installing Complete Package Set"
echo "=========================================="

source /opt/less/start.sh

echo "Installing tidyverse and data processing packages..."
mamba install -c conda-forge -y \
    r-tidyverse \
    r-data.table \
    r-purrr

echo ""
echo "Installing analysis packages..."
mamba install -c conda-forge -y \
    r-rcppeigen \
    r-lme4 \
    r-lmertest \
    r-emmeans \
    r-mumin

echo ""
echo "Verifying installations..."
Rscript -e "
packages <- c(
  # Tidyverse components
  'dplyr', 'tidyr', 'readr', 'stringr', 'ggplot2', 'readxl',
  # Data manipulation
  'data.table', 'purrr',
  # Analysis
  'RcppEigen', 'lme4', 'lmerTest', 'emmeans', 'MuMIn'
)

cat('\\nPackage Check:\\n')
cat('==============\\n\\n')

all_good <- TRUE
for (pkg in packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    version <- packageVersion(pkg)
    cat(sprintf('✓ %-20s %s\\n', pkg, version))
  } else {
    cat(sprintf('✗ %-20s MISSING\\n', pkg))
    all_good <- FALSE
  }
}

if (all_good) {
  cat('\\n✓ All packages installed successfully!\\n')
} else {
  cat('\\n✗ Some packages are missing\\n')
  quit(status = 1)
}
"

echo ""
echo "=========================================="
echo "Installation Complete!"
echo "=========================================="
