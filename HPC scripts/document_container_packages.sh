#!/bin/bash

# Script to document exact package versions in the container for reproducibility

echo "Container Package Inventory for Reproducibility"
echo "Generated: $(date)"
echo ""

singularity exec less_container.sif Rscript -e "
cat('R Version:\\n')
cat(R.version.string, '\\n\\n')

cat('Installed Packages:\\n')
cat('===================\\n\\n')

packages <- c('dplyr', 'readxl', 'RcppEigen', 'lme4', 'lmerTest', 'emmeans', 'MuMIn')

for (pkg in packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    version <- packageVersion(pkg)
    cat(sprintf('%s: %s\\n', pkg, version))
  }
}

cat('\\n\\nSession Info:\\n')
cat('=============\\n')
sessionInfo()
" > container_package_versions.txt

echo "Package versions saved to: container_package_versions.txt"
cat container_package_versions.txt
