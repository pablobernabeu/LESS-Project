# HPC Workflow Guide for LESS Project

## Directory Structure

### Personal Space (where you submit jobs)
- Location: Your home directory on HPC (e.g., `/cluster/home/username`)
- Contains:
  - R scripts (analyses/EEG/test_revised_analysis/*.R)
  - HPC submission scripts (HPC scripts/*.sh)
  - Container file (less_container.sif)
  - Small support files (data/R_functions/*, data/*.csv)

### Project Space (data and results)
- Location: `/cluster/projects/nn10008k/LESS`
- Contains:
  - Raw data (data/raw data/EEG/*, data/raw data/executive functions/*, etc.)
  - Merged data (data/merged data/*.rds)
  - Analysis results (analyses/EEG/*/results/*.rds)
  - Large output files

## Bind Mount Configuration

When running scripts in the container, two directories are mounted:

```bash
singularity exec \
    --bind "${HOST_PROJECT_DIR}":/project_root \
    --bind /cluster/projects/nn10008k/LESS:/cluster/projects/nn10008k/LESS \
    ./less_container.sif \
    Rscript /project_root/path/to/script.R
```

This means:
- **Personal space** → `/project_root` inside container (scripts, small files)
- **Project space** → `/cluster/projects/nn10008k/LESS` inside container (data, results)

## Path Configuration in R Scripts

### For scripts in personal space (sourced files)
Use relative paths from `/project_root`:
```r
source("data/R_functions/merge_trialbytrial_EEG_data.R")
```

### For data in project space (reading)
Use absolute paths:
```r
list.files(path = '/cluster/projects/nn10008k/LESS/data/raw data/EEG')
```

### For results in project space (writing)
Use absolute paths:
```r
saveRDS(model, '/cluster/projects/nn10008k/LESS/analyses/EEG/test_revised_analysis/results/model.rds')
```

## Workflow Steps

### 1. Check Container and Packages

```bash
# From your personal space on HPC
bash HPC\ scripts/check_container_setup.sh
```

This will:
- Verify `less_container.sif` exists
- Check project space accessibility
- Verify all required R packages
- Attempt to install missing packages

### 2. Submit Analysis Job

```bash
# From your personal space on HPC
sbatch HPC\ scripts/EEG_VONA_consolidated_lmerTest.sh
```

### 3. Monitor Job

```bash
# Check job status
squeue -u $USER

# View output log (updates in real-time)
tail -f EEG_VONA_consolidated.log

# View SLURM output
tail -f EEG_VONA_consolidated.out
```

### 4. Retrieve Results

Results will be saved to:
`/cluster/projects/nn10008k/LESS/analyses/EEG/test_revised_analysis/results/`

## Important Notes

1. **Path Strategy**: Keep scripts and functions in personal space (smaller, version-controlled), but data and results in project space (larger, shared)

2. **Source Files**: Functions like `merge_trialbytrial_EEG_data.R` are sourced from personal space but must use project space paths for data

3. **Memory**: Consolidated models require up to 90GB RAM (use `--partition=bigmem`)

4. **Time Limit**: Jobs can run up to 7 days

5. **Container Packages**: If packages are missing, they can be installed at runtime (see check_and_install_packages.R)

## Troubleshooting

### "Cannot open file" errors
- Check if path uses absolute path for project space: `/cluster/projects/nn10008k/LESS/...`
- Check if path uses relative path for personal space scripts

### "Package not found" errors
- Run `bash HPC scripts/check_container_setup.sh` first
- Check that required packages are in the container

### "Bind mount" errors
- Verify project space path: `/cluster/projects/nn10008k/LESS`
- Ensure you have read/write permissions
