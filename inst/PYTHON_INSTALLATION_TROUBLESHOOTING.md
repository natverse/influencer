# Python Installation Troubleshooting Guide

This document provides solutions for common Python installation issues encountered when installing ConnectomeInfluenceCalculator.

## Quick Solutions

### For Most Users (Recommended)
```r
# This method includes automatic fallbacks for common issues
influencer::install_python_influence_calculator()
```

### For Advanced Users or Alternative Environments
```r
# UV-based installation with Homebrew dependencies  
influencer::install_python_with_uv()
```

## Common Issues and Automatic Fixes

### 1. MPI Compiler Conflicts

**Problem**: Mixed MPI implementations cause compilation failures
- Error: "PETSc was configured with Open MPI but now appears to be compiling using a non-Open MPI mpi.h"
- Cause: Homebrew uses Open MPI while conda uses MPICH

**Automatic Solution**: 
- Function automatically detects compilation failures
- Falls back to conda-forge pre-compiled packages: `CONDA_SUBDIR=osx-64 conda install -c conda-forge petsc4py slepc4py`
- Avoids compilation entirely by using compatible binaries

### 2. pyproject.toml License Format Issues

**Problem**: Upstream repository has invalid license format
- Error: "configuration error: `project.license` must be valid exactly by one definition"
- Cause: `license = "BSD-3-Clause"` should be `license = {text = "BSD-3-Clause"}`

**Automatic Solution**:
- Function detects pyproject.toml validation errors
- Clones repository locally
- Fixes license format automatically
- Installs from corrected local copy

### 3. Conda Solver Issues

**Problem**: libmamba solver causes dependency resolution failures
- Error: Various conda solver and libarchive issues

**Automatic Solution**:
- Function automatically switches to classic solver: `conda config --set solver classic`
- Ensures compatibility across different conda versions

### 4. SQLite Database Binding Errors

**Problem**: Data type compatibility issues with temporary database
- Error: "Failed to create temporary SQLite database: Can only bind lists of raw vectors (or NULL)"
- Cause: `integer64` and complex data types (e.g., `arrow_list`) from large datasets

**Automatic Solution**:
- Vignette code automatically converts problematic data types:
  ```r
  banc.meta <- banc_codex_annotations() %>%
    mutate(
      root_id = as.character(pt_root_id),
      pt_root_id = as.character(pt_root_id), 
      pt_supervoxel_id = as.character(pt_supervoxel_id)
    ) %>%
    select(-pt_position)  # Remove complex arrow_list column
  ```

## Manual Troubleshooting Steps

If automatic fixes don't work, try these manual steps:

### Step 1: Verify System Configuration
```bash
# Check conda version and configuration
conda --version
conda config --show solver

# Check for conflicting MPI installations  
which mpicc
ls -la /usr/local/bin/mpi*
ls -la /opt/miniconda3/bin/mpi*
```

### Step 2: Clean Installation
```r
# Remove existing environment
conda env remove -n ic-venv

# Force reinstall
influencer::install_python_influence_calculator(force_reinstall = TRUE)
```

### Step 3: Manual Dependency Installation
```bash
# Fix conda solver
conda config --set solver classic

# Install with architecture specification
CONDA_SUBDIR=osx-64 conda install -c conda-forge petsc4py slepc4py -n ic-venv -y

# Fix pyproject.toml and install locally
git clone https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator.git temp_fix
sed -i '' 's/^license = "BSD-3-Clause"/license = {text = "BSD-3-Clause"}/' temp_fix/pyproject.toml
conda run -n ic-venv pip install temp_fix/
rm -rf temp_fix
```

## Installation Methods Comparison

| Method | Best For | Automatic Fixes | RStudio Integration |
|--------|----------|-----------------|-------------------|
| `install_python_influence_calculator()` | Most users | ✅ All fixes | ✅ Excellent |
| `install_python_with_uv()` | Advanced users | ✅ pyproject.toml only | ⚠️ Manual setup needed |

## Verification

Test your installation:
```r
# Load the package
library(influencer)

# Test Python import
reticulate::py_config()
reticulate::import("InfluenceCalculator")

# Run a simple test
ic <- influence_calculator_py(data.frame(pre=1, post=2, norm=0.5), data.frame(id=1:2))
```

## Getting Help

If you encounter issues not covered here:

1. **Check R console output** for specific error messages
2. **Restart R session** before retrying installation
3. **Verify Python environment** with `reticulate::py_config()`
4. **Report persistent issues** with full error logs

## Technical Background

The automatic fixes address these root causes:

- **MPI Conflicts**: Systems often have multiple MPI implementations. Pre-compiled packages avoid compilation conflicts entirely.
- **License Validation**: Python packaging tools have strict validation rules that the upstream repository doesn't currently meet.
- **Solver Issues**: Different conda solver backends have different compatibility characteristics.

These fixes ensure robust installation across diverse system configurations without requiring users to manually diagnose and resolve complex system-level conflicts.