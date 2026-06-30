#' Native R Implementation of Connectome Influence Calculator
#'
#' Create an InfluenceCalculator object using native R implementations.
#' This provides a fast, pure R alternative to the Python implementation.
#'
#' @param edgelist_simple Data frame with edge list containing columns 'pre',
#'   'post', 'count', and optional 'norm'. If 'norm' is absent it is computed
#'   from 'count' as `count / sum(count) per post`. Required when sqlite = FALSE.
#' @param meta Data frame with metadata containing at least 'root_id' column.
#'   Must also contain 'top_nt' when `signed = TRUE` or `excluded_nts` is given.
#'   Required when sqlite = FALSE.
#' @param filename Character. Path to SQLite database file. Used when sqlite = TRUE.
#' @param sqlite Logical. Whether to load data from SQLite database (default: FALSE).
#' @param signed Logical. Whether to use signed connectivity matrix (default: FALSE).
#'   When `TRUE`, `inhibitory_nts` must be supplied.
#' @param count_thresh Numeric. Minimum threshold count for postsynaptic connections
#'   (default: 3).
#' @param const Constant value added to log(influence) to ensure non-negative adjusted
#'   influence scores. Should be set to -log(minimum_accepted_influence) where
#'   minimum_accepted_influence is the smallest influence value considered meaningful.
#'   Default 24 corresponds to minimum_accepted_influence = exp(-24) ~ 3.78e-11.
#' @param inhibitory_nts Character vector of `top_nt` values whose pre-neurons should
#'   receive negative weights when `signed = TRUE`. The library has no per-organism
#'   default; you must supply this set explicitly when `signed = TRUE`. Common
#'   conventions: `c("glutamate", "gaba", "serotonin", "octopamine")` for *Drosophila*,
#'   `"gaba"` for *C. elegans* (only ACh and GABA have unambiguous signs there).
#' @param excluded_nts Character vector of `top_nt` values whose pre-neurons should
#'   contribute *nothing* to W: their outgoing edges are dropped entirely. Independent
#'   of `signed`. Use this for transmitter classes whose sign at a given target depends
#'   on the receptor mix (e.g. dopamine, serotonin in *C. elegans*) and so cannot be
#'   assigned a single sign safely.
#' @param lambda_max Numeric in `(0, 1)`. Target spectral radius of the rescaled
#'   connectivity matrix W~; controls how much the leading recurrent mode of
#'   `(I - W~)^-1` is amplified (gain = `1/(1 - lambda_max)`). The default `0.99`
#'   gives a `100x` gain -- appropriate for whole-CNS *Drosophila* (BANC-scale) graphs
#'   where you want sensitivity to weak distal influence. Lower values (e.g. `0.5`,
#'   `2x` gain) damp the leading mode and expose per-target seed specificity at the
#'   cost of attenuating long polysynaptic effects -- more appropriate for smaller
#'   graphs like *C. elegans*. Treat it as a "reverb knob": near 1 a signal echoes
#'   through long indirect paths; near 0.5 it mostly traverses short paths.
#' @param neg_neurotransmitters **Deprecated** (renamed to `inhibitory_nts`). Still
#'   accepted for backwards compatibility but emits a warning.
#'
#' @return InfluenceCalculatorR object (R6 class)
#' @export
#'
#' @examples
#' \dontrun{
#' # C. elegans convention (small graph; lower lambda_max)
#' ic <- influence_calculator_r(
#'   edgelist_simple = my_edges,
#'   meta = my_meta,
#'   signed = TRUE,
#'   inhibitory_nts = "gaba",
#'   excluded_nts = c("glutamate", "dopamine", "serotonin", "octopamine"),
#'   lambda_max = 0.5
#' )
#'
#' # Drosophila / BANC convention (whole-CNS; default lambda_max)
#' ic <- influence_calculator_r(
#'   edgelist_simple = banc_edges,
#'   meta = banc_meta,
#'   signed = TRUE,
#'   inhibitory_nts = c("glutamate", "gaba", "serotonin", "octopamine")
#' )
#'
#' # Calculate influence for specific seed neurons
#' results <- ic$calculate_influence(seed_ids = c(123, 456))
#' }
influence_calculator_r <- function(edgelist_simple = NULL, meta = NULL, filename = NULL,
                                   sqlite = FALSE, signed = FALSE, count_thresh = 3, const = 24,
                                   inhibitory_nts = NULL, excluded_nts = NULL,
                                   lambda_max = 0.99,
                                   neg_neurotransmitters = NULL) {
  # Backwards compatibility: accept the old neg_neurotransmitters argument name
  if (!is.null(neg_neurotransmitters)) {
    warning("`neg_neurotransmitters` is deprecated; use `inhibitory_nts` instead.")
    if (is.null(inhibitory_nts)) {
      inhibitory_nts <- neg_neurotransmitters
    }
  }
  InfluenceCalculatorR$new(edgelist_simple = edgelist_simple,
                           meta = meta,
                           filename = filename,
                           sqlite = sqlite,
                           signed = signed,
                           count_thresh = count_thresh,
                           const = const,
                           inhibitory_nts = inhibitory_nts,
                           excluded_nts = excluded_nts,
                           lambda_max = lambda_max)
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
    #' @field lambda_max Target spectral radius of the rescaled matrix
    lambda_max = NULL,
    #' @field inhibitory_nts top_nt values whose pre-neurons get negated weights when signed
    inhibitory_nts = NULL,
    #' @field excluded_nts top_nt values whose pre-neurons contribute nothing to W
    excluded_nts = NULL,

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
    #' @param inhibitory_nts Character vector of `top_nt` values whose pre-neurons
    #'   receive negative weights when `signed = TRUE`. Required for signed mode.
    #' @param excluded_nts Character vector of `top_nt` values whose pre-neurons
    #'   contribute nothing to W (their outgoing edges are dropped). Independent
    #'   of `signed`.
    #' @param lambda_max Target spectral radius of the rescaled W~; default 0.99.
    #'   See [influence_calculator_r()] for the trade-off it controls.
    initialize = function(edgelist_simple = NULL, meta = NULL, filename = NULL,
                          sqlite = FALSE, signed = FALSE, count_thresh = 3, const = 24,
                          inhibitory_nts = NULL, excluded_nts = NULL,
                          lambda_max = 0.99) {
      # Validate lambda_max
      if (!is.numeric(lambda_max) || length(lambda_max) != 1L ||
          !is.finite(lambda_max) || lambda_max <= 0 || lambda_max >= 1) {
        stop("lambda_max must satisfy 0 < lambda_max < 1; got ", lambda_max, ".")
      }
      # signed=TRUE requires inhibitory_nts
      if (signed && (is.null(inhibitory_nts) || length(inhibitory_nts) == 0L)) {
        stop("signed = TRUE requires `inhibitory_nts` to be specified as a ",
             "character vector of neurotransmitter names matching values in ",
             "meta$top_nt.")
      }

      # Store configuration
      self$W_signed <- signed
      self$const <- const
      self$lambda_max <- lambda_max
      self$inhibitory_nts <- if (is.null(inhibitory_nts)) character(0) else as.character(inhibitory_nts)
      self$excluded_nts <- if (is.null(excluded_nts)) character(0) else as.character(excluded_nts)

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

      # `top_nt` is required whenever signed mode or excluded_nts is in play
      if ((signed || length(self$excluded_nts) > 0L) &&
          !"top_nt" %in% names(self$meta)) {
        stop("signed = TRUE or excluded_nts requires meta to contain a ",
             "'top_nt' column identifying neurotransmitter types.")
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

    compute_edgelist_hash = function(elist) {
      # Create a reproducible hash of the edgelist for caching
      # Hash each column separately to avoid exceeding R's 2^31-1 byte paste limit
      # on large edgelists (100M+ rows)
      col_hashes <- c(
        digest::digest(elist$pre, algo = "sha256"),
        digest::digest(elist$post, algo = "sha256"),
        digest::digest(elist$count, algo = "sha256"),
        digest::digest(elist$norm, algo = "sha256"),
        digest::digest(self$W_signed, algo = "sha256"),
        digest::digest(self$lambda_max, algo = "sha256"),
        digest::digest(self$inhibitory_nts, algo = "sha256"),
        digest::digest(self$excluded_nts, algo = "sha256")
      )
      digest::digest(col_hashes, algo = "sha256")
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
      # Required columns in edgelist_simple -- match the Python validation surface:
      # pre + post are mandatory, plus either count or weight.
      required_edge_cols <- c("pre", "post")
      missing_edge_cols <- setdiff(required_edge_cols, names(edgelist_simple))
      if (length(missing_edge_cols) > 0) {
        stop("edgelist_simple is missing required columns: ",
             paste(missing_edge_cols, collapse = ", "),
             ". Expected at minimum 'pre', 'post', and one of 'count' or 'weight'. ",
             "Found columns: ", paste(names(edgelist_simple), collapse = ", "), ".")
      }
      if (!any(c("count", "weight") %in% names(edgelist_simple))) {
        stop("edgelist_simple must contain either 'count' (raw synapse count) or ",
             "'weight' (pre-normalised edge weight). Found columns: ",
             paste(names(edgelist_simple), collapse = ", "), ".")
      }

      # Required columns in meta
      required_meta_cols <- c("root_id")
      missing_meta_cols <- setdiff(required_meta_cols, names(meta))
      if (length(missing_meta_cols) > 0) {
        stop("meta is missing required columns: ",
             paste(missing_meta_cols, collapse = ", "),
             ". Found columns: ", paste(names(meta), collapse = ", "), ".")
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
      # Filter by count threshold (only meaningful when 'count' is present)
      if ("count" %in% names(edgelist_simple)) {
        elist <- edgelist_simple[edgelist_simple$count >= count_thresh, , drop = FALSE]
      } else {
        # 'weight'-only edge list: no per-edge count to filter on
        elist <- edgelist_simple
      }

      # Compute 'norm' from 'count' if absent (matches the Python helper)
      if (!"norm" %in% names(elist)) {
        if ("weight" %in% names(elist) && !"count" %in% names(elist)) {
          # Treat pre-normalised 'weight' as 'norm' directly
          elist$norm <- elist$weight
          elist$count <- elist$weight
        } else {
          post_totals <- stats::ave(elist$count, elist$post, FUN = sum)
          elist$norm <- elist$count / post_totals
        }
      }

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
      # Apply excluded_nts first: drop outgoing edges from pre-neurons whose top_nt
      # is in excluded_nts. Independent of signed=TRUE/FALSE.
      if (length(self$excluded_nts) > 0L) {
        excluded_mask <- self$meta$top_nt %in% self$excluded_nts
        excluded_ids <- self$meta$root_id[excluded_mask]
        if (length(excluded_ids) > 0L) {
          elist <- elist[!elist$pre %in% excluded_ids, , drop = FALSE]
        }
      }

      # Then apply signed-mode negation: pre-neurons whose top_nt is in
      # inhibitory_nts get their weights negated. Negate the column actually used to
      # populate W (syn_weight_measure, default 'norm') -- the historical bug here
      # was negating 'count' while the matrix was built from 'norm', so signed=TRUE
      # silently produced the same matrix as signed=FALSE.
      if (self$W_signed && length(self$inhibitory_nts) > 0L) {
        inhibitory_mask <- self$meta$top_nt %in% self$inhibitory_nts &
                          self$meta$root_id %in% elist$pre
        inhibitory_ids <- self$meta$root_id[inhibitory_mask]
        inhibitory_edges <- elist$pre %in% inhibitory_ids
        elist[[syn_weight_measure]][inhibitory_edges] <-
          -elist[[syn_weight_measure]][inhibitory_edges]
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
        # Find the largest real eigenvalue. Used to rescale W so its largest real
        # eigenvalue equals self$lambda_max exactly (a true control knob over
        # leading-mode amplification rather than just a stability cap).
        eig_result <- RSpectra::eigs(W, k = 1, which = "LR")
        max_eigenvalue <- Re(eig_result$values[1])

        # Cache eigenvalue if this is the base connectivity matrix
        if (identical(W, self$W)) {
          self$max_eigenvalue <- max_eigenvalue
        }
      }

      # Always rescale to lambda_max exactly when the natural spectral radius is
      # meaningfully positive. Mirrors the Python implementation: the leading-mode
      # gain in (I - W~)^-1 is 1/(1 - lambda_max), so this gives the user real
      # control over how much polysynaptic propagation is amplified.
      #
      # Eigenvalues below ~1e-4 are typically ARPACK iteration noise on a
      # near-nilpotent matrix (e.g. RSpectra returns ~5e-6 on a strict-lower-
      # triangular toy graph whose true spectrum is {0, 0, 0}). Rescaling such
      # matrices to lambda_max would inflate them by a factor of ~10^4 to 10^8 and
      # blow up the subsequent LU factorisation; for connectome work that
      # regime never arises in practice, so we leave the matrix alone with a
      # warning instead.
      noise_floor <- 1e-4
      if (is.finite(max_eigenvalue) && max_eigenvalue > noise_floor) {
        alpha <- self$lambda_max / max_eigenvalue
        W <- W * alpha
      } else {
        warning("Largest real eigenvalue of W is ",
                signif(max_eigenvalue, 3),
                " (<= noise floor ", noise_floor,
                "); skipping rescale to lambda_max = ", self$lambda_max,
                ". W is treated as already stable.",
                call. = FALSE)
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
      # In signed mode preserve the real part so net-inhibited targets carry a
      # negative score; in unsigned mode take the magnitude.
      if (isTRUE(self$W_signed)) {
        influence_vec <- Re(influence_vec)
      } else {
        influence_vec <- abs(Re(influence_vec))
      }

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
      weight_type <- ifelse(isTRUE(self$W_signed), "signed", "unsigned")
      score_col_name <- paste0("Influence_score_(", weight_type, ")")
      result_df[[score_col_name]] <- influence_vec

      # Calculate adjusted influence: sign-preserving log + const, with magnitudes
      # below the floor exp(-const) clipped to zero in either sign.
      floor_val <- exp(-const)
      mag <- pmax(abs(influence_vec), floor_val)
      adjusted_inf <- sign(influence_vec) * (log(mag) + const)
      # Magnitudes below the floor map to log(floor) + const = 0; ensure that
      # nominal-zero values are exactly 0 even for the unsigned branch.
      adjusted_inf[abs(influence_vec) < floor_val] <- 0
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
#' @param edgelist_simple Data frame with edge list. See [influence_calculator_r()]
#'   for column requirements.
#' @param meta Data frame with metadata. See [influence_calculator_r()] for column
#'   requirements.
#' @param filename Character. Path to a connectome data file. SQLite is the legacy
#'   format; the Python backend additionally supports CSV / Parquet / Feather
#'   detected by extension.
#' @param sqlite Logical. Whether to load data from SQLite database (default: FALSE).
#' @param method Character. Either "python" or "r" (default: "r").
#' @param signed Logical. Whether to use signed connectivity matrix (default: FALSE).
#' @param count_thresh Numeric. Minimum threshold count (default: 3).
#' @param const Numeric. Adjustment constant for the log transform applied by the R
#'   backend's `calculate_influence()`. See [influence_calculator_r()] for details.
#' @param inhibitory_nts Character vector of `top_nt` values whose pre-neurons get
#'   negated weights when `signed = TRUE`. Required for signed mode.
#' @param excluded_nts Character vector of `top_nt` values whose pre-neurons
#'   contribute nothing to W. Independent of `signed`.
#' @param lambda_max Numeric in `(0, 1)`. Target spectral radius of the rescaled
#'   matrix; default `0.99`. See [influence_calculator_r()].
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
#' # Use Python implementation directly with data frames (no temp SQLite needed)
#' ic.py <- influence_calculator(edgelist_simple = my_edges, meta = my_meta,
#'                                method = "python")
#' }
influence_calculator <- function(edgelist_simple = NULL, meta = NULL, filename = NULL,
                                 sqlite = FALSE, method = "r", signed = FALSE,
                                 count_thresh = 3, const = 24,
                                 inhibitory_nts = NULL, excluded_nts = NULL,
                                 lambda_max = 0.99) {
  method <- match.arg(method, choices = c("r", "python"))

  if (method == "r") {
    influence_calculator_r(edgelist_simple = edgelist_simple,
                           meta = meta,
                           filename = filename,
                           sqlite = sqlite,
                           signed = signed,
                           count_thresh = count_thresh,
                           const = const,
                           inhibitory_nts = inhibitory_nts,
                           excluded_nts = excluded_nts,
                           lambda_max = lambda_max)
  } else {
    # Python backend: from v0.2.0 of ConnectomeInfluenceCalculator no temporary
    # SQLite is needed -- data frames are accepted directly by the DataFrame
    # constructor (InfluenceCalculator(edgelist_df, meta_df)).
    influence_calculator_py(filename = filename,
                            edgelist_simple = edgelist_simple,
                            meta = meta,
                            signed = signed,
                            count_thresh = count_thresh,
                            inhibitory_nts = inhibitory_nts,
                            excluded_nts = excluded_nts,
                            lambda_max = lambda_max)
  }
}
