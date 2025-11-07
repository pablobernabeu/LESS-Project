# Connecting to HPC from Windows

## Quick Start

Open Git Bash or Windows Terminal and run:

```bash
ssh username@saga.sigma2.no
```

Replace `username` with your HPC username.

## Step-by-Step Instructions

### 1. Open Terminal
- **Git Bash**: Search for "Git Bash" in Start menu
- **Windows Terminal**: Install from Microsoft Store (recommended)
- **CMD**: Can work but less features

### 2. Connect via SSH

```bash
# Basic connection
ssh your_username@saga.sigma2.no

# With X11 forwarding (for graphical applications)
ssh -X your_username@saga.sigma2.no
```

### 3. Navigate to Your Workspace

```bash
# Go to personal space (where your scripts are)
cd ~/LESS-Project  # Or wherever you keep the repository

# Check container exists
ls -lh less_container.sif

# Check project space accessible
ls /cluster/projects/nn10008k/LESS/
```

### 4. Run Setup Check

```bash
# Make script executable (first time only)
chmod +x HPC\ scripts/check_container_setup.sh

# Run the check
bash HPC\ scripts/check_container_setup.sh
```

This will verify:
- Container exists
- R packages are available
- Project space is accessible
- Install any missing packages

### 5. Submit a Job

```bash
# Make sure you're in your project directory
cd ~/LESS-Project

# Submit the job
sbatch HPC\ scripts/EEG_VONA_consolidated_lmerTest.sh
```

### 6. Monitor Progress

```bash
# Check job status
squeue -u $USER

# Watch the R script output (Ctrl+C to exit)
tail -f EEG_VONA_consolidated.log

# Watch SLURM output
tail -f EEG_VONA_consolidated.out
```

### 7. Check Results

```bash
# List result files
ls -lh /cluster/projects/nn10008k/LESS/analyses/EEG/test_revised_analysis/results/

# View file sizes
du -sh /cluster/projects/nn10008k/LESS/analyses/EEG/test_revised_analysis/results/
```

## File Transfer

### Upload files to HPC

```bash
# From your local Windows machine (in Git Bash/PowerShell)
scp path/to/local/file username@saga.sigma2.no:~/destination/

# Upload entire directory
scp -r path/to/local/dir username@saga.sigma2.no:~/destination/
```

### Download from HPC

```bash
# From your local Windows machine
scp username@saga.sigma2.no:~/remote/file path/to/local/

# Download directory
scp -r username@saga.sigma2.no:~/remote/dir path/to/local/
```

### Using VSCode Remote SSH (Recommended)

1. Install "Remote - SSH" extension in VS Code
2. Press F1, type "Remote-SSH: Connect to Host"
3. Enter: `username@saga.sigma2.no`
4. Open your project folder
5. Edit files directly on HPC!

## Common Commands

```bash
# Check job queue
squeue -u $USER

# Cancel a job
scancel JOB_ID

# Check job details
scontrol show job JOB_ID

# View completed job info
sacct -j JOB_ID --format=JobID,JobName,State,ExitCode,Elapsed,MaxRSS

# Check disk usage
du -sh ~/LESS-Project
du -sh /cluster/projects/nn10008k/LESS

# Check quota
dusage

# View log file with less (q to quit)
less EEG_VONA_consolidated.log
```

## Troubleshooting

### Cannot connect
- Check VPN if required
- Verify username/password
- Check HPC status: https://www.sigma2.no/user-support

### Permission denied
- Check file permissions: `ls -la filename`
- Make script executable: `chmod +x script.sh`

### Container not found
- Verify you're in correct directory: `pwd`
- Check container location: `ls -l less_container.sif`

### Job not starting
- Check queue: `squeue -u $USER`
- Check job details: `scontrol show job JOB_ID`
- View any errors: `cat *.out`
