#!/bin/bash

# Helper script to check container and packages on HPC
# This script should be run from your personal space on the HPC

echo "=========================================="
echo "LESS Project HPC Container Setup Check"
echo "=========================================="
echo ""

# Check current location
echo "Current directory: $(pwd)"
echo ""

# Check if container exists
if [ -f "less_container.sif" ]; then
    echo "✓ Container found: less_container.sif"
    ls -lh less_container.sif
    echo ""
else
    echo "✗ Container NOT found: less_container.sif"
    echo "  Expected location: $(pwd)/less_container.sif"
    echo ""
    exit 1
fi

# Display bind mount information
echo "=== Bind Mount Structure ==="
echo "Personal space (scripts/container): $(pwd)"
echo "Project space (data/results): /cluster/projects/nn10008k/LESS"
echo ""

# Check if project space is accessible
if [ -d "/cluster/projects/nn10008k/LESS" ]; then
    echo "✓ Project space accessible"
    echo "  Location: /cluster/projects/nn10008k/LESS"
else
    echo "✗ Project space NOT accessible: /cluster/projects/nn10008k/LESS"
    echo ""
    exit 1
fi

echo ""
echo "=== Checking R packages in container ==="
echo "Running package check script inside container..."
echo ""

# Run the package check script inside the container
singularity exec \
    --bind "$(pwd)":/project_root \
    --bind /cluster/projects/nn10008k/LESS:/cluster/projects/nn10008k/LESS \
    ./less_container.sif \
    Rscript /project_root/HPC\ scripts/check_and_install_packages.R

echo ""
echo "=========================================="
echo "Setup check complete!"
echo "=========================================="
