#!/bin/bash

# Script to initialize renv inside the container with the packages we need
# Run this ONCE after building/updating the container

echo "=========================================="
echo "Initializing renv in Container"
echo "=========================================="

# Enter the container in writable mode
singularity shell --fakeroot --writable less_sandbox << 'EOF'

source /opt/less/start.sh

# Navigate to project directory inside container
cd /project_root

# Initialize renv (creates renv/ directory and renv.lock)
R -e "
if (!requireNamespace('renv', quietly = TRUE)) {
  install.packages('renv', repos = 'https://cloud.r-project.org/')
}

# Initialize renv in this directory
renv::init(bare = TRUE, restart = FALSE)

# Install required packages into renv library
renv::install(c(
  'dplyr',
  'readxl',
  'RcppEigen',
  'lme4',
  'lmerTest',
  'emmeans',
  'MuMIn'
))

# Create snapshot
renv::snapshot()

cat('\\nrenv initialized successfully!\\n')
cat('Lockfile created at: renv.lock\\n')
"

exit
EOF

echo ""
echo "=========================================="
echo "renv Setup Complete"
echo "=========================================="
echo ""
echo "Next steps:"
echo "1. Rebuild container: singularity build less_container_new.sif less_sandbox"
echo "2. The container will now use renv for package management"
