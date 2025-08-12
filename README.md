# influencer

[![natverse](https://img.shields.io/badge/natverse-Part%20of%20the%20natverse-a241b6)](https://natverse.github.io)
[![DOI](https://zenodo.org/badge/964174582.svg)](https://doi.org/10.5281/zenodo.15999929)
[![R-CMD-check](https://github.com/natverse/influencer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/natverse/influencer/actions/workflows/R-CMD-check.yaml)
[![pkgdown](https://github.com/natverse/influencer/actions/workflows/pkgdown.yaml/badge.svg)](https://github.com/natverse/influencer/actions/workflows/pkgdown.yaml)

Tools for computing influence scores of neurons in connectomes using linear dynamical models of neural signal propagation.

## About

Based on the [ConnectomeInfluenceCalculator](https://doi.org/10.5281/ZENODO.15999930) by [Zaki Ajabi](https://scholar.google.com/citations?user=DQSPi2kAAAAJ&hl=en) and [Jan Drugowitsch](https://scholar.google.com/citations?user=fCUx98wAAAAJ&hl=en), with contributions from [Alexander Shakeel Bates](https://scholar.google.com/citations?user=BOVTiXIAAAAJ&hl=en) and [Rachel I. Wilson](https://scholar.google.com/citations?user=T-RODd8AAAAJ&hl=en). The `influencer` package provides R implementations of their algorithm plus Python wrappers for the original library.

**Algorithm Development**: Mathematical framework and algorithms by Zaki Ajabi and Jan Drugowitsch. This R package implements their methods with an R interface.

**General Purpose**: Developed using *Drosophila* connectome data (particularly BANC) but works with any connectome dataset.

## Install

You can install the development version of `influencer` from GitHub:

```r
remotes::install_github('natverse/influencer')
```

### Python Dependencies (Optional)

For Python wrapper functionality, the package provides streamlined installation:

```r
# Installs all dependencies into r-reticulate environment
install_python_influence_calculator()
```

**What it does:**
- Automatically detects or installs PETSc and SLEPc (via Homebrew or source builds)
- Uses r-reticulate conda environment exclusively (no additional environments)
- Installs ConnectomeInfluenceCalculator with automatic issue resolution
- Local builds install to `~/.local/influencer/` if needed
- Saves configuration to `~/.influencer_env` for reuse

**Manual installation**: See the [ConnectomeInfluenceCalculator installation guide](https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator#install).

## Description

![source_to_targets](https://github.com/natverse/influencer/blob/main/inst/images/source_to_targets.jpg?raw=true)

This package computes the influence scores of a neuron or a group of neurons, as specified through the seed vector **s**, on all downstream neurons in the connectome based on a linear dynamical model of neural signal propagation:

$$
\begin{aligned}
\tau \frac{d \boldsymbol{r}(t)}{dt} &= - \boldsymbol{r}(t) + \boldsymbol{W} \boldsymbol{r}(t) + \boldsymbol{s}(t) \\
&= \left( \boldsymbol{W} - \boldsymbol{I} \right)\boldsymbol{r}(t) + \boldsymbol{s}(t)
\end{aligned}
$$

where **r** is the vector of neural activity, **W** is the connectivity matrix, and **s** is the simulated neural stimulation (applied to the seed neurons and remains constant throughout the simulation). The connectivity matrix is constructed by mapping the neuron IDs to matrix indices and arranging them such that the columns correspond to presynaptic neurons and rows correspond to postsynaptic neurons.

To ensure stable neural dynamics, we rescale **W** such that:

$$
\begin{equation}
    \tilde{\boldsymbol{W}} = \frac{0.99}{\lambda_{max}} \boldsymbol{W}
\end{equation}
$$

where λ_max is the largest real eigenvalue of **W**. The steady-state solution can thus be written as:

$$
\begin{equation}
    \boldsymbol{r}_{\infty} = - \left( \tilde{\boldsymbol{W}} - \boldsymbol{I} \right)^{-1} \boldsymbol{s} .
\end{equation}
$$

The influence of any seed is defined as the magnitude of neural activity at steady state, **r**_∞.

This is a linear system, and so the results should be additive between seed runs. The non-linear effects of convergent information flow are not considered.


## Quick Start

### 1. Install
```r
remotes::install_github('natverse/influencer')
library(influencer)
```

### 2. Basic Usage - R Implementation (a bit slower than the python one)
```r
# Example with simple network data
edges <- data.frame(
  pre = c("A", "B", "C"),
  post = c("B", "C", "D"), 
  count = c(5, 8, 3),
  norm = c(0.5, 0.8, 0.3),
  post_count = c(10, 10, 10)
)

meta <- data.frame(
  id = c("A", "B", "C", "D"),
  root_id = c("A", "B", "C", "D")
)

# Create influence calculator
ic <- influence_calculator_r(edgelist_simple = edges, meta = meta)

# Calculate influence of neuron "A" on the network
influence_scores <- ic$calculate_influence("A")
print(influence_scores)
```

### 3. Real Data Example - BANC Connectome
```r
# Install bancr package for BANC connectome data access
remotes::install_github('flyconnectome/bancr')

library(dplyr)
library(bancr)  # For real connectome data

# Load BANC connectome (may take a few minutes)
banc_meta <- banc_codex_annotations() %>% 
  mutate(root_id = as.character(pt_root_id)) %>%
  select(-pt_position)

banc_edges <- banc_edgelist() %>%
  select(pre = pre_pt_root_id, post = post_pt_root_id, count = n) %>%
  # Add edge processing...

# Analyze descending neurons
ic_banc <- influence_calculator_r(edgelist_simple = banc_edges, meta = banc_meta)
dna_influence <- ic_banc$calculate_influence(seed_neuron_ids)
```

### 4. Want Python Backend?
```r
# One-time setup
install_python_influence_calculator()

# Then use Python implementation
ic_py <- influence_calculator_py(edgelist_simple = edges, meta = meta)
```

**more?** See the [full tutorial](articles/banc-connectome-analysis.html) with real BANC data analysis.

---

## Comparison with other methods

Here is a distribution of adjusted influence scores (`log(influence)+24`, to pass 0) by whether or not the target is directly connected with the source. The source are all descending and ascending neurons in the BANC connectome dataset, the targets are all other neurons:

![adjusted_influence_versus_graph_traversal](https://github.com/natverse/influencer/blob/main/inst/images/adjusted_influence_versus_graph_traversal.jpg?raw=true)

We can also compared our influence measure with the 'cascade hop' model from [Winding, Pedigo et al., 2023](https://www.science.org/doi/10.1126/science.add9330). Here we do so with the olfactory system, the sources are all olfactory sensory neurons in the FAFB dataset:

![adusted_influence_versus_cascade](https://github.com/natverse/influencer/blob/main/inst/images/adusted_influence_versus_cascade.jpg?raw=true)

We may also compared our influence measure with the stochastic 'graph traversal' model from [Schlegel and Bates et al., 2021](https://pubmed.ncbi.nlm.nih.gov/34032214/). Here we do so with the olfactory system, the sources are all olfactory sensory neurons in the FAFB dataset:

![adjusted_influence_versus_graph_traversal](https://github.com/natverse/influencer/blob/main/inst/images/adjusted_influence_versus_graph_traversal.jpg?raw=true)

## Usage

The `influencer` package provides both native R implementations and Python wrappers for connectome influence analysis.

### Native R Implementation (Recommended)

```r
library(influencer)

# Using data frames (default method - most flexible)
# Prepare your edge list and metadata
dummy_edges <- data.frame(
  pre = c(1, 2, 3, 1),          # presynaptic neuron IDs
  post = c(2, 3, 1, 3),         # postsynaptic neuron IDs  
  count = c(10, 8, 5, 12),      # synapse counts
  norm = c(1.0, 1.2, 0.8, 1.1)  # normalised connection strengths
)

dummy_meta <- data.frame(
  root_id = c(1, 2, 3),         # neuron IDs
  cell_type = c("A", "B", "C"), # cell type annotations
  super_class = c("motor", "sensory", "interneuron")
)

# Create InfluenceCalculator object using data frames
ic.r <- influence_calculator_r(edgelist_simple = dummy_edges, meta = dummy_meta)

# Or use the unified interface (defaults to data frames and R implementation)
ic.r <- influence_calculator(edgelist_simple = dummy_edges, meta = dummy_meta)

# Using SQLite database (legacy method)
## ic.sqlite <- influence_calculator_r(filename = 'connectome_dataset.sqlite', sqlite = TRUE)

# Or with unified interface
## ic.sqlite <- influence_calculator(filename = 'connectome_dataset.sqlite', sqlite = TRUE)
```

### Python Wrapper (Simplified)

The Python wrapper uses the r-reticulate environment exclusively:

```r
library(influencer)

# Install Python dependencies (into r-reticulate environment)
install_python_influence_calculator() 

# Create InfluenceCalculator object
ic.py <- influence_calculator_py('connectome_dataset.sqlite')

# Or use with data frames (creates temporary SQLite database)
ic.py <- influence_calculator_py(edgelist_simple = dummy_edges, meta = dummy_meta)
```

**Python Environment**: All Python functionality uses the r-reticulate conda environment, eliminating environment management complexity.

### Basic Analysis

```r
# Using data frames (recommended)
ic.df <- influence_calculator(edgelist_simple = dummy_edges, meta = dummy_meta, 
                             signed = TRUE, count_thresh = 3)

# Define seed neurons (example: motor neurons)
seed.ids <- ic.df$meta$root_id[ic.df$meta$super_class == 'motor']

# Define neurons to silence (example: sensory neurons) 
silenced.neurons <- ic.df$meta$root_id[ic.df$meta$super_class == 'sensory']

# Calculate influence scores  
influence.df <- ic.df$calculate_influence(seed.ids, silenced.neurons)

# View results (includes adjusted_influence column)
head(influence.df)

# The results include:
# - Raw influence scores in Influence_score_(unsigned) or Influence_score_(signed)
# - adjusted_influence = max(0, log(influence_score) + 24)

# Using SQLite database (legacy)
ic.sqlite <- influence_calculator(filename = 'connectome_dataset.sqlite', 
                                  sqlite = TRUE, signed = TRUE, count_thresh = 3)
seed.ids <- ic.sqlite$meta$root_id[ic.sqlite$meta$seed_01 == 'olfactory']
influence.df <- ic.sqlite$calculate_influence(seed.ids)
```

### Signed vs Unsigned Connectivity

The package supports both signed and unsigned connectivity matrices:

```r
# Unsigned connectivity (synaptic weights are all positive)
ic.unsigned <- influence_calculator(edgelist_simple = dummy_edges, meta = dummy_meta, signed = FALSE)

# Signed connectivity (inhibitory neurons get negative weights)
ic.signed <- influence_calculator(edgelist_simple = dummy_edges, meta = dummy_meta, signed = TRUE)
```

### Setting Connection Thresholds

You can specify a minimum threshold for the number of synaptic connections:

```r
# Only consider connections with at least 5 synapses (default is 3)
ic.filtered <- influence_calculator(edgelist_simple = dummy_edges, meta = dummy_meta, count_thresh = 5)
```

## Implementation Comparison

**R implementation** (Recommended for most users):
- Pure R with sparse matrix operations (`Matrix`, `RSpectra`)
- Performance improvements through caching and smart routing
- No external dependencies or environment management needed
- Direct integration with R data analysis workflows

**Python wrapper** (For specialized use cases):
- Uses original ConnectomeInfluenceCalculator with PETSc/SLEPc
- Uses r-reticulate environment exclusively
- May provide speed advantages for very large networks (>100k neurons)
- Automatic PETSc/SLEPc detection and installation

Both implementations produce highly correlated results with the **R implementation recommended** for typical connectome analysis workflows.

## Example: BANC Connectome Analysis

### About the BANC Connectome

The **BANC** (Brain And Nerve Cord) connectome represents the first complete connectome of both brain and ventral nerve cord in a limbed animal - *Drosophila melanogaster*. This landmark dataset comprises approximately 160,000 neurons across the entire central nervous system, revealing how brain and nerve cord work together as an integrated system. The BANC connectome is particularly important for understanding distributed motor control, sensorimotor integration, and the complete neural pathways underlying behavior.

Here's an example using the BANC connectome to analyse descending neuron influence. Not the first time we calciulate influence with a new edgelist it is very slow, the expensive part isn't the influence calculation itself, but the initial eigenvalue computation and matrix. By caching the result, we can speed things up a lot.

```r
library(influencer)
library(bancr)
library(dplyr)
library(ggplot2)

# Load BANC cell type annotations from the centralised codex
banc.meta <- banc_codex_annotations() %>%
  mutate(
    root_id = as.character(pt_root_id),           # Convert integer64 to character for SQLite compatibility
    pt_root_id = as.character(pt_root_id),        # Convert integer64 to character
    pt_supervoxel_id = as.character(pt_supervoxel_id)  # Convert integer64 to character
  ) %>%
  select(-pt_position)  # Remove complex arrow_list column that causes SQLite issues

# Focus analysis on descending neurons that connect brain to VNC
dn.ids <- banc.meta %>% 
  filter(super_class == "descending") %>%
  pull(root_id) %>%
  as.character()

cat("Total neurons in BANC:", nrow(banc.meta), "\n")
cat("Descending neurons:", length(dn.ids), "\n")
cat("DNa03 neurons:", sum(banc.meta$cell_type == "DNa03", na.rm = TRUE), "\n")
cat("DNa06 neurons:", sum(banc.meta$cell_type == "DNa06", na.rm = TRUE), "\n")

# Load the complete BANC connectivity matrix (this may take several minutes)
cat("Loading BANC edge list (this may take a few minutes)...\n")
banc.edges <- banc_edgelist()

# Create edge list with synapse counts and postsynaptic-normalisation
# This normalisation step helps calibrate for neuron size and connection number
# i.e. a neuron with more inputs likely needs more of them 'activated' to have an effect
edges.table <- banc.edges %>%
  select(pre = pre_pt_root_id, post = post_pt_root_id, count = n) %>%
  group_by(post) %>%
  mutate(post_count = sum(count, na.rm=TRUE)) %>%
  mutate(norm = round(count/post_count,4)) %>%
  ungroup() %>%
  mutate(pre = as.character(pre),
         post = as.character(post)) %>%
  filter(count > 5)  # Apply synapse count threshold

cat("Loaded", nrow(edges.table), "edges from BANC connectome\n")
head(edges.table)

# Create influence calculator using native R implementation
ic.dns <- influence_calculator_r(edgelist_simple = edges.table, meta = banc.meta)

# Select DNa03 descending neurons as our seed population
dna03.ids <- banc.meta %>% 
             filter(cell_type == "DNa03") %>%
             pull(pt_root_id) %>%
             as.character()

cat("DNa03 seed IDs:", paste(dna03.ids, collapse = ", "), "\n")

# Get direct targets of DNa03 neurons
dna03.post.ids <- edges.table %>%
                  filter(pre %in% dna03.ids) %>%
                  pull(post) %>%
                  unique() %>%
                  as.character()

cat("Direct targets of DNa03:", length(dna03.post.ids), "neurons\n")

# Calculate how DNa03 neurons influence other neurons (long compute the FIRST time, ~30mins, but then decomposition is cached)
system.time({
  dna03.influence <- ic.dns$calculate_influence(dna03.ids)
})
# ~23 minutes for the first calculation on the full BANC connectome (1.1M edges, 114K neurons)

cat("Calculated influence for", nrow(dna03.influence), "neurons\n")

# Show the structure of results
head(dna03.influence)

# Classify neurons by their direct connection to seeds
dna03.influence$directly_connected <- dna03.influence$id %in% dna03.post.ids

# Summary statistics
table(dna03.influence$directly_connected)

# Create density plot showing influence patterns with scaled densities
g1 <- ggplot(dna03.influence, aes(x = adjusted_influence, 
                                  fill = directly_connected)) +
  geom_density(alpha = 0.7, color = NA, position = "identity", 
               aes(y = after_stat(density))) +
  scale_fill_manual(values = c("FALSE" = "#007BC3", 
                               "TRUE" = "#EE4244"),
                    labels = c("indirectly connected", "directly connected")) +
  labs(title = "DNa03 influence onto other descending neurons",
       subtitle = "adjusted influence onto directly connected and indirectly connected targets",
       x = "adjusted influence score", 
       y = "density",
       fill = "Connection type") +
  theme_minimal() +
  theme(legend.position = "bottom")

print(g1)

# Save  
ggsave(g1, 
    file = "inst/images/banc_dna03_influence_r.png", 
    height = 6, 
    width = 8)
```

Which gave me this for v626 of BANC:

![banc_dna03_influence_r](https://github.com/natverse/influencer/blob/main/inst/images/banc_dna03_influence_r.png?raw=true)

Now that we have run `dns$calculate_influence` once on a large edgelist, the decomposition should be cached (expensive compute). A new calculation with different sources should be much faster:

```r
# Get DNa06 IDs from real BANC data
dna06.ids <- banc.meta %>%
             filter(cell_type == "DNa06") %>%
             pull(pt_root_id) %>%
             as.character()

cat("DNa06 seed IDs found:", length(dna06.ids), "\n")

# Get direct targets of DNa06 neurons
dna06.post.ids <- edges.table %>%
                  filter(pre %in% dna06.ids) %>%
                  pull(post) %>%
                  unique() %>%
                  as.character()

cat("Direct targets of DNa06:", length(dna06.post.ids), "neurons\n")

# Calculate how DNa06 neurons influence other neurons
system.time({
  dna06.influence <- ic.dns$calculate_influence(dna06.ids)
})
# ~0.39 seconds, 3500x speedup for subsequent calculations with different seeds but the same connectivity matrix
dna06.influence$directly_connected <- dna06.influence$id %in% dna06.post.ids

cat("Calculated influence for", nrow(dna06.influence), "neurons (cached calculation)\n")

# Get direct targets for both DNa03 and DNa06
dna03.post.ids <- edges.table %>%
                  filter(pre %in% dna03.ids) %>%
                  pull(post) %>%
                  unique() %>%
                  as.character()

dna06.post.ids <- edges.table %>%
                  filter(pre %in% dna06.ids) %>%
                  pull(post) %>%
                  unique() %>%
                  as.character()

# Merge results for comparison
comparison_data <- merge(
  dna03.influence[, c("id", "adjusted_influence")],
  dna06.influence[, c("id", "adjusted_influence")],
  by = "id",
  suffixes = c("DNa03", "DNa06")
)

# Add detailed direct connection information for both DNa03 and DNa06
comparison_data$direct_to_DNa03 <- comparison_data$id %in% dna03.post.ids
comparison_data$direct_to_DNa06 <- comparison_data$id %in% dna06.post.ids

# Create detailed connection categories
comparison_data$connection_type <- case_when(
  comparison_data$direct_to_DNa03 & comparison_data$direct_to_DNa06 ~ "Both-direct",
  comparison_data$direct_to_DNa03 & !comparison_data$direct_to_DNa06 ~ "DNa03-direct",
  !comparison_data$direct_to_DNa03 & comparison_data$direct_to_DNa06 ~ "DNa06-direct",
  TRUE ~ "Indirect"
)

# Order data frame so indirect points are plotted first (will be covered by others)
comparison_data <- comparison_data[order(comparison_data$connection_type), ]
comparison_data$connection_type <- factor(comparison_data$connection_type, 
                                         levels = c("Indirect", "DNa03-direct", "DNa06-direct", "Both-direct"))

# Comparison plot with La Croix inspired colors
g2 <- ggplot(comparison_data, aes(x = adjusted_influenceDNa03, y = adjusted_influenceDNa06, 
                                 color = connection_type)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("Indirect" = "#808080",           # Grey
                               "DNa03-direct" = "#FF6B9D",       # Pamplemousse Pink
                               "DNa06-direct" = "#4ECDC4",       # Key Lime
                               "Both-direct" = "#FFE66D")) +     # Lemon Yellow
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
  labs(title = "DNa03 vs DNa06 Influence Comparison",
       subtitle = "Comparing influence patterns between different descending neuron types",
       x = "Adjusted Influence (DNa03)",
       y = "Adjusted Influence (DNa06)",
       color = "Connection type") +
  theme_minimal() +
  coord_equal()

print(g2)

# Save plot
ggsave(g2, 
      file = "inst/images/banc_dna03_vs_dna06_influence.png", 
      height = 6, 
      width = 8)

# Calculate correlation between the two cell types
correlation <- cor(comparison_data$adjusted_influenceDNa03, 
                   comparison_data$adjusted_influenceDNa06)
cat("Correlation between DNa03 and DNa06 influence patterns:", round(correlation, 4), "\n")
```

![banc_dna03_vs_dna06_influence](https://github.com/natverse/influencer/blob/main/inst/images/banc_dna03_vs_dna06_influence.png?raw=true)

Now compare with the python implementation:

```r
# Python implementation using r-reticulate environment
system.time({
  ic.dns.py <- influence_calculator_py(edgelist_simple = edges.table, meta = banc.meta)
})

# Time both implementations
cat("Running R implementation for comparison...\n")
system.time({
  dna03.influence.r <- ic.dns$calculate_influence(dna03.ids) 
})

cat("Running Python implementation for comparison...\n")
system.time({
  dna03.influence.py <- calculate_influence_py(ic.dns.py, dna03.ids)
})

# Compare the results - create a comparison plot
# Merge results for comparison
python_comparison_data <- merge(
  dna03.influence.r[, c("id", "adjusted_influence")],
  dna03.influence.py[, c("id", "adjusted_influence")],
  by = "id",
  suffixes = c("_R", "_Python")
)

# Add direct connection information
python_comparison_data$directly_connected <- python_comparison_data$id %in% dna03.post.ids

# Order data frame so indirect points are plotted first
python_comparison_data <- python_comparison_data[order(!python_comparison_data$directly_connected), ]

# Comparison plot with direct connection coloring
subtitle_text <- "Real comparison between R and Python implementations"

g3 <- ggplot(python_comparison_data, aes(x = adjusted_influence_R, y = adjusted_influence_Python,
                                        color = directly_connected)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("FALSE" = "#007BC3", "TRUE" = "#EE4244"),
                    labels = c("indirectly connected", "directly connected")) +
  geom_abline(slope = 1, intercept = 0, color = "black", linetype = "dashed", alpha = 0.5) +
  labs(title = "R vs Python Implementation Comparison",
       subtitle = subtitle_text,
       x = "Adjusted Influence (R implementation)",
       y = "Adjusted Influence (Python implementation)",
       color = "Connection type") +
  theme_minimal() +
  coord_equal()

print(g3)

# Save plot
ggsave(g3, 
      file = "inst/images/banc_dna03_influence_r_vs_python.png", 
      height = 6, 
      width = 8)

# Calculate correlation
python_correlation <- cor(python_comparison_data$adjusted_influence_R, 
                         python_comparison_data$adjusted_influence_Python)
cat("Correlation between R and Python implementations:", round(python_correlation, 4), "\n")
```

![banc_dna03_influence_r_vs_python](https://github.com/natverse/influencer/blob/main/inst/images/banc_dna03_influence_r_vs_python.png?raw=true)

## Why might the R and python versions give slightly different results?

The R and Python implementations use different numerical libraries that can produce small differences in results:

**Eigenvalue computation:**
- **R**: Uses ARPACK (via `RSpectra::eigs`) with Arnoldi iteration
- **Python**: Uses SLEPc with more advanced eigenvalue algorithms

**Linear system solving:**
- **R**: Direct methods via `Matrix::solve` (CHOLMOD/UMFPACK) with optional LU factorization caching  
- **Python**: Iterative GMRES solver with ILU preconditioning via PETSc

**Sparse matrix handling:**
- **R**: CSparse-based Matrix package representation
- **Python**: PETSc's internal sparse matrix format

These different algorithms can lead to slightly different numerical precision, particularly in the eigenvalue computation step that determines matrix scaling. The differences are typically very small (correlation > 0.999) and do not affect biological interpretation.

## Data Format

### Data Frames (Recommended)

Works with standard R data frames:

**edgelist_simple**: Edge list with required columns:
- `pre`: Presynaptic neuron IDs
- `post`: Postsynaptic neuron IDs  
- `count`: Number of synapses
- `norm`: A normalisation of count, typically count/total inputs for the target neuron

**meta**: Metadata with required columns:
- `root_id`: Neuron IDs (must match IDs in edgelist)
- Additional columns (e.g., `super_class`, `cell_type`, neurotransmitter information) are optional

### SQLite Database Format (for large datasets)

For SQLite input (when `sqlite = TRUE`), the database should contain:
- `meta`: Metadata table with columns including `root_id`, `super_class`, neurotransmitter information, etc.
- `edgelist_simple`: Edge list with columns `pre`, `post`, `count`, `norm`

## Contributing

For contributions and bug reports related to this R package, please see the [GitHub repository](https://github.com/natverse/influencer).

For information about the underlying algorithm, issues with the core mathematical implementation, or to learn more about the method, please refer to the [original ConnectomeInfluenceCalculator repository](https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator) by the Drugowitsch Lab. This is the primary repository for the ConnectomeInfluenceCalculator project and contains detailed algorithmic documentation and the original Python implementation.

Please see the [contribution guidelines](https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator/blob/main/CONTRIBUTING.md) from the original project.

## Citing

If you use this package in your research, please cite **all three** of the following:

1. **The original ConnectomeInfluenceCalculator software:**
   Ajabi, Zaki, Alexander S. Bates, and Jan Drugowitsch. 2025. Connectome Influence Calculator. Zenodo. https://doi.org/10.5281/ZENODO.15999930.

2. **The scientific method and algorithm:**
   Bates, Alexander Shakeel, Jasper S. Phelps, Minsu Kim, Helen H. Yang, Arie Matsliah, Zaki Ajabi, Eric Perlman, et al. 2025. "Distributed Control Circuits across a Brain-and-Cord Connectome." bioRxiv. https://doi.org/10.1101/2025.07.31.667571.

3. **This R package:**
   ```r
   citation("influencer")
   ```

   Bates, A. (2025). influencer: R Tools for Connectome Influence Analysis. R package version 0.1.0. https://github.com/natverse/influencer

## Acknowledgements

This R package provides both native R implementations and wrappers for the Python [ConnectomeInfluenceCalculator](https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator) developed by the Drugowitsch Lab. The original Python implementation uses PETSc and SLEPc for efficient sparse matrix computations. This code was developed while A. Bates was in the group of, and supported by, R. I. Wilson at Harvard Medical School.
