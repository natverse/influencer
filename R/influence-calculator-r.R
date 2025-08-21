#' Native R Implementation of Connectome Influence Calculator
#'
#' Create an InfluenceCalculator object using native R implementations.
#' This provides a fast, pure R alternative to the Python implementation.
#'
#' @param edgelist_simple Data frame with edge list containing columns 'pre', 'post', 
#'   'count', and 'norm'. Required when sqlite = FALSE.
#' @param meta Data frame with metadata containing at least 'root_id' column. 
#'   Required when sqlite = FALSE.
#' @param filename Character. Path to SQLite database file. Used when sqlite = TRUE.
#' @param sqlite Logical. Whether to load data from SQLite database (default: FALSE).
#' @param signed Logical. Whether to use signed connectivity matrix (default: FALSE).
#' @param count_thresh Numeric. Minimum threshold count for postsynaptic connections
#'   (default: 3).
#' @param const Constant value added to log(influence) to ensure non-negative adjusted 
#'   influence scores. Should be set to -log(minimum_accepted_influence) where 
#'   minimum_accepted_influence is the smallest influence value considered meaningful. 
#'   Default 24 corresponds to minimum_accepted_influence = exp(-24) ≈ 3.78e-11.
#' @param neg_neurotransmitters Character Neurotransmitters that receive negative signs if requested. Must be present in `top_nt`
#'   field of `meta`.
#'
#' @return InfluenceCalculatorR object (R6 class)
#' @export
#'
#' @examples
#' \dontrun{
#' # Using data frames (recommended approach)
#' ic.r <- influence_calculator_r(edgelist_simple = my_edges, meta = my_meta)
#' 
#' # Using SQLite database (legacy method)
#' ic.sqlite <- influence_calculator_r(filename = "connectome.sqlite", sqlite = TRUE)
#' 
#' # Calculate influence with specific seed neurons
#' results <- ic.r$calculate_influence(seed_ids = c(123, 456))
#' }
influence_calculator_r <- function(edgelist_simple = NULL, meta = NULL, filename = NULL, 
                                   sqlite = FALSE, signed = FALSE, count_thresh = 3, const = 24,
                                   neg_neurotransmitters = c("glutamate", "gaba", "serotonin", "octopamine")) {
  InfluenceCalculatorR$new(edgelist_simple, meta, filename, sqlite, signed, count_thresh, const, neg_neurotransmitters)
}

#' R6 Class for Native R Influence Calculator
#'
#' @description
#' Native R implementation of the ConnectomeInfluenceCalculator functionality.
#'
#' @details
#' This class provides a pure R implementation of connectome influence calculation
#' using efficient sparse matrix operations.
#'
#' @importFrom R6 R6Class
#' @export
InfluenceCalculatorR <- R6::R6Class("InfluenceCalculatorR",
  public = list(
    #' @field W_signed Whether connectivity matrix uses signed weights
    W_signed = NULL,
    #' @field meta Metadata table from database
    meta = NULL,
    #' @field n_neurons Number of neurons in the network
    n_neurons = NULL,
    #' @field W Sparse connectivity matrix
    W = NULL,
    #' @field id_to_index Mapping from neuron IDs to matrix indices
    id_to_index = NULL,
    #' @field index_to_id Mapping from matrix indices to neuron IDs
    index_to_id = NULL,
    #' @field const Constant value for adjusted influence calculation
    const = NULL,
    #' @field edgelist_hash Hash of edgelist for cache validation
    edgelist_hash = NULL,
    #' @field W_normalized Cached normalized connectivity matrix  
    W_normalized = NULL,
    #' @field W_factorization Cached matrix factorization for solving
    W_factorization = NULL,
    #' @field max_eigenvalue Cached maximum eigenvalue for normalization
    max_eigenvalue = NULL,
    
    #' @description
    #' Create a new InfluenceCalculatorR object
    #'
    #' @param edgelist_simple Data frame with edge list
    #' @param meta Data frame with metadata
    #' @param filename Path to SQLite database
    #' @param sqlite Whether to use SQLite database
    #' @param signed Whether to use signed connectivity matrix
    #' @param count_thresh Minimum synapse count threshold
    #' @param const Constant value for adjusted influence calculation
    #' @param neg_neurotransmitters Character Neurotransmitters that receive negative signs if requested. Must be present in `top_nt` field of `meta`.
    initialize = function(edgelist_simple = NULL, meta = NULL, filename = NULL, 
                          sqlite = FALSE, signed = FALSE, count_thresh = 3, const = 24,
                          neg_neurotransmitters = c("glutamate", "gaba", "serotonin", "octopamine")) {
      # Store connectivity matrix type and constant for later reference
      self$W_signed <- signed
      self$const <- const
      
      # Store custom neurotransmitter list if provided
      if (!missing(neg_neurotransmitters)) {
        private$neg_neurotransmitters <- neg_neurotransmitters
      }
      
      # Branch based on data source: SQLite database or R data frames
      if (sqlite) {
        if (is.null(filename)) {
          stop("filename must be provided when sqlite = TRUE")
        }
        # Load connectivity and metadata from SQLite database
        elist <- private$load_sql_data(filename, count_thresh)
      } else {
        if (is.null(edgelist_simple) || is.null(meta)) {
          stop("edgelist_simple and meta must be provided when sqlite = FALSE")
        }
        # Validate input data frames have required columns
        private$validate_input_data(edgelist_simple, meta)
        # Apply synapse count threshold and prepare edge list
        elist <- private$prepare_edgelist(edgelist_simple, count_thresh)
        self$meta <- meta
      }
      
      # Compute hash of edgelist for caching validation
      self$edgelist_hash <- private$compute_edgelist_hash(elist)
      
      # Create bidirectional mapping between neuron IDs and matrix indices
      private$create_neuron_id_mapping(elist)
      # Build sparse connectivity matrix from edge list
      private$create_sparse_W(elist)
    },
    
    #' @description
    #' Calculate influence scores for given seed neurons
    #'
    #' @param seed_ids Vector of seed neuron IDs
    #' @param silenced_neurons Vector of neuron IDs to silence (default: none, for performance)
    #' @param const Constant value added to log(influence) to ensure non-negative adjusted 
    #'   influence scores. Should be set to -log(minimum_accepted_influence) where 
    #'   minimum_accepted_influence is the smallest influence value considered meaningful. 
    #'   If NULL, uses value from initialization (default: NULL).
    #' @return Data frame with influence scores and adjusted influence column
    calculate_influence = function(seed_ids, silenced_neurons = numeric(0), const = NULL) {
      # Initialize seed stimulation vector (pre-allocated for performance)
      seed_vec <- numeric(self$n_neurons)
      # Convert seed neuron IDs to matrix indices
      seed_indices <- self$id_to_index[as.character(seed_ids)]
      seed_indices <- seed_indices[!is.na(seed_indices)]  # Remove invalid IDs
      if (length(seed_indices) > 0) {
        seed_vec[seed_indices] <- 1  # Set seed neurons to receive stimulation
      }
      
      # Check if we need to apply silencing (slower path)
      use_silencing <- length(silenced_neurons) > 0
      
      if (use_silencing) {
        # Apply silencing by removing outgoing connections from specified neurons
        silenced_indices <- self$id_to_index[as.character(silenced_neurons)]
        silenced_indices <- silenced_indices[!is.na(silenced_indices)]
        # Never silence the seed neurons themselves
        silenced_indices <- setdiff(silenced_indices, seed_indices)
        
        if (length(silenced_indices) > 0) {
          # Create modified matrix (slower path)
          W_work <- private$set_columns_to_zero(self$W, silenced_indices)
          W_norm <- private$normalise_W(W_work)
        } else {
          W_norm <- private$get_normalized_W()
        }
      } else {
        # Fast path: use cached normalized matrix
        W_norm <- private$get_normalized_W()
      }
      
      # Solve the linear dynamical system for steady-state activity
      influence_vec <- private$solve_linear_system(W_norm, -seed_vec)
      
      # Use provided const or fall back to instance constant
      const_to_use <- if (is.null(const)) self$const else const
      
      # Format results as a data frame with neuron IDs and influence scores
      private$build_influence_dataframe(influence_vec, seed_vec, const_to_use)
    }
  ),
  
  private = list(
    # Neurotransmitters that receive negative signs if requested
    neg_neurotransmitters = c("glutamate", "gaba", "serotonin", "octopamine"),
    
    compute_edgelist_hash = function(elist) {
      # Create a reproducible hash of the edgelist for caching
      # Include key columns that affect connectivity matrix
      key_data <- paste(elist$pre, elist$post, elist$count, elist$norm, 
                       self$W_signed, collapse = "|")
      digest::digest(key_data, algo = "sha256")
    },
    
    get_normalized_W = function() {
      # Return cached normalized matrix if available, otherwise compute and cache
      if (is.null(self$W_normalized)) {
        self$W_normalized <- private$normalise_W(self$W)
        # Also pre-compute factorization for faster repeated solving
        private$get_factorization(self$W_normalized)
      }
      return(self$W_normalized)
    },
    
    get_factorization = function(W_norm) {
      # Return cached factorization if available, otherwise compute and cache
      if (is.null(self$W_factorization)) {
        # Pre-factorize matrix for faster repeated solving
        self$W_factorization <- Matrix::lu(W_norm)
      }
      return(self$W_factorization)
    },
    
    validate_input_data = function(edgelist_simple, meta) {
      # Check required columns in edgelist_simple
      required_edge_cols <- c("pre", "post", "count", "norm")
      missing_edge_cols <- setdiff(required_edge_cols, names(edgelist_simple))
      if (length(missing_edge_cols) > 0) {
        stop("edgelist_simple is missing required columns: ", 
             paste(missing_edge_cols, collapse = ", "))
      }
      
      # Check required columns in meta
      required_meta_cols <- c("root_id")
      missing_meta_cols <- setdiff(required_meta_cols, names(meta))
      if (length(missing_meta_cols) > 0) {
        stop("meta is missing required columns: ", 
             paste(missing_meta_cols, collapse = ", "))
      }
      
      # Check that edgelist neurons are in meta
      unique_edge_ids <- unique(c(edgelist_simple$pre, edgelist_simple$post))
      missing_meta_ids <- setdiff(unique_edge_ids, meta$root_id)
      if (length(missing_meta_ids) > 0) {
        warning("Some neurons in edgelist are not found in meta: ", 
                length(missing_meta_ids), " neurons missing")
      }
    },
    
    prepare_edgelist = function(edgelist_simple, count_thresh) {
      # Filter by count threshold
      elist <- edgelist_simple[edgelist_simple$count >= count_thresh, , drop = FALSE]
      
      # Add post_count column if not present
      if (!"post_count" %in% names(elist)) {
        elist$post_count <- round(elist$count / elist$norm)
      }
      
      return(elist)
    },
    
    load_sql_data = function(filename, count_thresh) {
      if (!file.exists(filename)) {
        stop("Database file not found: ", filename)
      }
      
      con <- DBI::dbConnect(RSQLite::SQLite(), filename)
      on.exit(DBI::dbDisconnect(con))
      
      # Load metadata
      self$meta <- DBI::dbReadTable(con, "meta")
      
      # Load edgelist with threshold
      query <- sprintf("SELECT * FROM edgelist_simple WHERE count >= %d", count_thresh)
      elist <- DBI::dbGetQuery(con, query)
      
      # Add post_count column
      elist$post_count <- round(elist$count / elist$norm)
      
      return(elist)
    },
    
    create_neuron_id_mapping = function(elist) {
      # Find unique neuron IDs
      unique_ids <- unique(c(elist$post, elist$pre))
      unique_ids <- sort(unique_ids)
      
      self$n_neurons <- length(unique_ids)
      
      # Create bidirectional mapping
      self$id_to_index <- setNames(seq_along(unique_ids), as.character(unique_ids))
      self$index_to_id <- setNames(as.character(unique_ids), seq_along(unique_ids))
    },
    
    create_sparse_W = function(elist, syn_weight_measure = "norm") {
      # Handle signed connectivity if requested
      if (self$W_signed) {
        # Create mask for inhibitory neurons
        inhibitory_mask <- self$meta$top_nt %in% private$neg_neurotransmitters &
                          self$meta$id %in% elist$pre
        inhibitory_ids <- self$meta$id[inhibitory_mask]
        
        # Update edge weights for inhibitory connections
        inhibitory_edges <- elist$pre %in% inhibitory_ids
        elist$count[inhibitory_edges] <- -elist$count[inhibitory_edges]
      }
      
      # Get synaptic weights
      syn_weights <- elist[[syn_weight_measure]]
      
      # Map to matrix indices
      pre_indices <- self$id_to_index[as.character(elist$pre)]
      post_indices <- self$id_to_index[as.character(elist$post)]
      
      # Create sparse matrix (post = rows, pre = columns)
      self$W <- Matrix::sparseMatrix(
        i = post_indices,
        j = pre_indices,
        x = syn_weights,
        dims = c(self$n_neurons, self$n_neurons)
      )
    },
    
    normalise_W = function(W) {
      # Use cached eigenvalue if working with the base connectivity matrix
      if (identical(W, self$W) && !is.null(self$max_eigenvalue)) {
        max_eigenvalue <- self$max_eigenvalue
      } else {
        # Find the largest real eigenvalue to determine scaling factor
        # This ensures stable dynamics by keeping eigenvalues below 1
        eig_result <- RSpectra::eigs(W, k = 1, which = "LR")
        max_eigenvalue <- Re(eig_result$values[1])
        
        # Cache eigenvalue if this is the base connectivity matrix
        if (identical(W, self$W)) {
          self$max_eigenvalue <- max_eigenvalue
        }
      }
      
      # Scale connectivity matrix to ensure eigenvalues stay below 0.99
      # This prevents runaway dynamics in the linear system
      if (max_eigenvalue > 0.99) {
        alpha <- 0.99 / max_eigenvalue
        W <- W * alpha
      }
      
      # Transform matrix for solving: W_final = W - I
      # This converts the ODE dr/dt = (W-I)r + s into the form needed for solving
      # Use more efficient diagonal modification
      Matrix::diag(W) <- Matrix::diag(W) - 1
      
      return(W)
    },
    
    set_columns_to_zero = function(W, silenced_indices) {
      W_new <- W
      W_new[, silenced_indices] <- 0
      return(W_new)
    },
    
    solve_linear_system = function(W_norm, seed_vec) {
      # Solve the linear system (W-I) * r = -s for steady-state activity r
      # This gives the equilibrium neural activity under constant stimulation
      
      # Try to use cached factorization for faster solving
      if (!is.null(self$W_factorization) && identical(W_norm, self$W_normalized)) {
        # Use pre-computed LU factorization
        result <- Matrix::solve(self$W_factorization, seed_vec)
      } else {
        # Fall back to direct solve (slower but handles modified matrices)
        result <- Matrix::solve(W_norm, seed_vec, sparse = TRUE)
      }
      return(as.vector(result))
    },
    
    build_influence_dataframe = function(influence_vec, seed_vec, const) {
      # Convert complex results to real influence magnitudes
      influence_vec <- abs(Re(influence_vec))
      
      # Find which neurons were used as seeds for result annotation
      seed_indices <- which(seed_vec == 1)
      n <- length(influence_vec)
      
      # Pre-allocate vectors for better performance
      matrix_index <- seq_len(n) - 1  # 0-indexed for consistency
      id <- as.character(self$index_to_id[seq_len(n)])
      is_seed <- rep(FALSE, n)
      is_seed[seed_indices] <- TRUE
      
      # Create result data frame with pre-allocated columns
      result_df <- data.frame(
        matrix_index = matrix_index,
        id = id,
        is_seed = is_seed,
        stringsAsFactors = FALSE
      )
      
      # Ensure id column stays as character (prevent automatic conversion to numeric)
      result_df$id <- as.character(result_df$id)
      
      # Add influence score column with descriptive name indicating matrix type
      weight_type <- ifelse(self$W_signed, "signed", "unsigned")
      score_col_name <- paste0("Influence_score_(", weight_type, ")")
      result_df[[score_col_name]] <- influence_vec
      
      # Calculate adjusted influence: log(influence) + const, with floor at 0
      adjusted_inf <- log(influence_vec) + const
      adjusted_inf[adjusted_inf < 0] <- 0  # Apply floor constraint
      result_df$adjusted_influence <- adjusted_inf
      
      return(result_df)
    }
  )
)

#' Unified Interface for Influence Calculation
#'
#' Provides a unified interface that can use either the Python or R implementation
#' of influence calculation.
#'
#' @param edgelist_simple Data frame with edge list containing columns 'pre', 'post', 
#'   'count', and 'norm'. Required when sqlite = FALSE.
#' @param meta Data frame with metadata containing at least 'root_id' column. 
#'   Required when sqlite = FALSE.
#' @param filename Character. Path to SQLite database file. Used when sqlite = TRUE.
#' @param sqlite Logical. Whether to load data from SQLite database (default: FALSE).
#' @param method Character. Either "python" or "r" (default: "r").
#' @param signed Logical. Whether to use signed connectivity matrix (default: FALSE).
#' @param count_thresh Numeric. Minimum threshold count (default: 3).
#' @param const Constant value added to log(influence) to ensure non-negative adjusted 
#'   influence scores. Should be set to -log(minimum_accepted_influence) where 
#'   minimum_accepted_influence is the smallest influence value considered meaningful. 
#'   Default 24 corresponds to minimum_accepted_influence = exp(-24) ≈ 3.78e-11. 
#'   Only used for R implementation.
#'
#' @return InfluenceCalculator object (either Python or R implementation)
#' @export
#'
#' @examples
#' \dontrun{
#' # Use R implementation with data frames (recommended)
#' ic.df <- influence_calculator(edgelist_simple = my_edges, meta = my_meta)
#' 
#' # Use R implementation with SQLite database
#' ic.sqlite <- influence_calculator(filename = "connectome.sqlite", sqlite = TRUE)
#' 
#' # Use Python implementation (requires SQLite database)
#' ic.py <- influence_calculator(filename = "connectome.sqlite", sqlite = TRUE, method = "python")
#' }
influence_calculator <- function(edgelist_simple = NULL, meta = NULL, filename = NULL,
                                 sqlite = FALSE, method = "r", signed = FALSE, count_thresh = 3, const = 24) {
  method <- match.arg(method, choices = c("r", "python"))
  
  if (method == "r") {
    influence_calculator_r(edgelist_simple, meta, filename, sqlite, signed, count_thresh, const)
  } else {
    if (!sqlite || is.null(filename)) {
      stop("Python implementation currently requires SQLite database. Set sqlite = TRUE and provide filename.")
    }
    influence_calculator_py(filename, signed, count_thresh)
  }
}