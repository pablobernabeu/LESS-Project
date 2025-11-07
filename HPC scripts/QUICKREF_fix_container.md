# Quick Reference: Fixing Container Dependencies

## The Problem
Your container has R packages trying to install from source with missing dependencies, creating a dependency chain failure.

## The Solution
Use conda/mamba to install pre-compiled R packages with all dependencies included.

## Quick Commands (copy-paste these)

```bash
# 1. Create writable sandbox from SIF
singularity build --sandbox --fix-perms less_sandbox less_container.sif

# 2. Enter container in writable mode
singularity shell --fakeroot --writable less_sandbox

# 3. Inside container - activate environment
source /opt/less/start.sh

# 4. Inside container - install packages with mamba
mamba install -c conda-forge -y \
    r-rcppeigen \
    r-dplyr \
    r-readxl \
    r-lme4 \
    r-lmertest \
    r-emmeans \
    r-mumin

# 5. Inside container - verify
R -e "library(lme4); library(emmeans); cat('SUCCESS\n')"

# 6. Exit container
exit

# 7. Rebuild immutable SIF
singularity build less_container_new.sif less_sandbox

# 8. Test new container
singularity exec less_container_new.sif Rscript -e "library(lme4); library(emmeans)"

# 9. Replace old container
mv less_container.sif less_container_backup.sif
mv less_container_new.sif less_container.sif

# 10. Cleanup (optional)
rm -rf less_sandbox
```

## Why This Works

**Problem with renv/install.packages():**
- Tries to compile from source
- Each package needs specific system libraries
- Dependency chain breaks if one component missing
- Error messages cascade

**Solution with mamba/conda:**
- Pre-compiled binaries for Linux
- All system dependencies included
- Proper dependency resolution
- Much faster installation

## Key Packages Needed

```r
dplyr           # Data manipulation
readxl          # Excel file reading  
RcppEigen       # Required by lme4
lme4            # Mixed-effects models (includes allFit)
lmerTest        # p-values for lme4
emmeans         # Post-hoc contrasts
MuMIn           # R-squared calculations
```

## Verification Test

After rebuilding, run this to verify:

```bash
singularity exec less_container.sif Rscript -e "
  library(dplyr)
  library(readxl)
  library(RcppEigen)
  library(lme4)
  library(lmerTest)
  library(emmeans)
  library(MuMIn)
  cat('All packages loaded successfully!\n')
  cat('allFit available:', 'allFit' %in% ls('package:lme4'), '\n')
"
```

## If You're Still Having Issues

### Option 1: Use existing renv but fix dependencies
The errors show missing packages like: `estimability`, `zoo`, `ucminf`, `DBI`, etc.

These are dependencies of your target packages. Install them first:

```r
# Inside writable container
mamba install -c conda-forge -y \
    r-estimability \
    r-zoo \
    r-ucminf \
    r-dbi \
    r-iterators \
    r-foreach \
    r-checkmate
```

### Option 2: Fresh R library (nuclear option)
If corruption is severe:

```bash
# Inside writable container
rm -rf /opt/less/renv/library/*
mamba install -c conda-forge -y r-essentials r-lme4 r-lmertest r-emmeans r-mumin
```

## Time Estimates

- Creating sandbox: 2-5 minutes
- Installing packages: 5-10 minutes  
- Rebuilding SIF: 5-10 minutes
- **Total: ~20-30 minutes**

## Storage Note

The sandbox directory will be ~2-4GB. Make sure you have space:

```bash
dusage  # Check quota
du -sh less_sandbox  # Check sandbox size
```

Clean up sandbox after successful rebuild to save space.
