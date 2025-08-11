# influencer 0.1.0

## New Features

### Core Functionality
* **Native R Implementation**: Fast influence calculation using Matrix and RSpectra packages
* **Python Integration**: Seamless wrapper for ConnectomeInfluenceCalculator with automatic environment management
* **R6 Class System**: `InfluenceCalculatorR` class with performance caching providing 3,500x speedup for repeated calculations
* **Dual Implementation Support**: Choose between R (`influence_calculator_r()`) and Python (`influence_calculator_py()`) implementations

### Matrix Operations
* **Signed/Unsigned Networks**: Support for both connectivity matrix types
* **Sparse Matrix Optimization**: Efficient handling of large connectomes using Matrix package
* **Eigendecomposition Caching**: Matrix factorization results cached for dramatic performance improvements
* **Robust Numerics**: Stable computation for large-scale neural networks (tested on 114k neurons, 1.1M edges)

### Data Integration  
* **BANC Connectome Support**: Direct integration with Brain and Nerve Cord connectome data via bancr
* **Flexible Input Formats**: Support for edge lists and metadata with automatic validation
* **SQLite Compatibility**: Automatic data type conversion for seamless database operations

## Installation and Environment Management

### Automatic Python Installation
* **`install_python_influence_calculator()`**: Conda-based installation with automatic issue resolution
* **`install_python_with_uv()`**: UV-based installation using Homebrew dependencies
* **MPI Conflict Resolution**: Automatic fallback to conda-forge pre-compiled packages when mixed MPI implementations cause failures
* **pyproject.toml Fixing**: Automatic license format correction for upstream repository issues
* **RStudio Integration**: Optimized for r-miniconda environment compatibility

### Robust Error Handling
* **Smart Error Detection**: Automatic identification of installation failure causes
* **Multiple Fallback Methods**: 5+ installation approaches with graceful degradation
* **Comprehensive Troubleshooting**: Detailed guides for common installation issues
* **System Compatibility**: Works across macOS, Linux with various Python/conda configurations

## Documentation and Examples

### Comprehensive Vignettes
* **Real-World Analysis**: BANC connectome influence analysis with 114,461 neurons
* **Performance Comparisons**: R vs Python implementation benchmarking and validation
* **Scientific Context**: Mathematical background and connectomics applications
* **Visualization**: ggplot2 integration with publication-ready plots

### User Experience
* **Clear Installation Guide**: Step-by-step setup with troubleshooting
* **Multiple Usage Patterns**: From simple influence queries to complex network analysis  
* **Automatic Environment Detection**: Seamless switching between Python environments
* **Progress Feedback**: Informative messages during long computations

## Performance Benchmarks

* **First Calculation**: ~23 minutes for full BANC connectome (114K neurons, 1.1M edges)
* **Cached Calculations**: ~0.39 seconds for subsequent influence calculations (3,500x speedup)
* **Memory Efficient**: Sparse matrix operations minimize memory footprint
* **Scalable**: Tested on connectomes ranging from hundreds to hundreds of thousands of neurons

## Technical Implementation

### R Implementation
* **Dependencies**: Matrix, RSpectra, R6, dplyr, RSQLite
* **Algorithm**: Influence matrix eigendecomposition with smart caching
* **Data Structures**: Efficient sparse matrix representations
* **Validation**: Cross-validation with Python reference implementation

### Python Integration
* **Backend**: ConnectomeInfluenceCalculator (PETSc/SLEPc-based)
* **Environment Management**: Automatic conda/UV environment setup
* **Interoperability**: Seamless data exchange via reticulate
* **Compatibility**: Support for Python 3.8-3.13 across multiple package managers

## Initial Release Notes

This is the first public release of the influencer package, developed for connectome influence analysis in computational neuroscience. The package provides both native R implementations and Python integration for maximum flexibility and performance.

**Installation**: `remotes::install_github("natverse/influencer")`

**Getting Started**: See `vignette("banc-connectome-analysis")` for a comprehensive tutorial using real BANC connectome data.

**Citation**: Please cite the original influence propagation method and this implementation when using this package in your research.