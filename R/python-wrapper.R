#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}


#' Set Python Environment for InfluenceCalculator
#'
#' Configure the r-reticulate Python environment for use with the ConnectomeInfluenceCalculator
#' Python library.
#'
#' @return Invisible TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' set_python_env()
#' }
set_python_env <- function() {
  # Use r-reticulate environment exclusively
  tryCatch({
    reticulate::use_condaenv("r-reticulate", required = TRUE)
  }, error = function(e) {
    if (grepl("Unable to find conda binary", e$message)) {
      stop("Conda not found. Install conda/miniconda first, then run install_python_influence_calculator()")
    } else {
      stop("Failed to activate r-reticulate environment: ", e$message, "\n",
           "Run: install_python_influence_calculator()")
    }
  })

  # Check if InfluenceCalculator is available
  tryCatch({
    ic <- reticulate::import("InfluenceCalculator")
    message("InfluenceCalculator python library loaded successfully")
    invisible(TRUE)
  }, error = function(e) {
    stop("Failed to import InfluenceCalculator from r-reticulate environment.\n",
         "Run: install_python_influence_calculator()\n",
         "Error: ", e$message)
  })
}

#' Create InfluenceCalculator Python Object
#'
#' Create an InfluenceCalculator object using the Python library from the r-reticulate
#' environment. From v0.2.0 of ConnectomeInfluenceCalculator the Python library accepts
#' data frames and several on-disk formats directly, so no temporary SQLite database
#' needs to be written when you pass `edgelist_simple` / `meta`.
#'
#' @param filename Character. Path to a connectome data file. The format is detected
#'   from the file extension: `.sqlite`/`.db` use the original SQLite constructor,
#'   `.csv` uses `from_csv`, `.parquet` uses `from_parquet`, and `.feather`/`.arrow`
#'   use `from_feather`. If the extension is unrecognised the original SQLite path
#'   is tried (matching pre-v0.2.0 behaviour).
#' @param edgelist_simple Data frame with edge list containing columns 'pre', 'post',
#'   and either 'count' (raw synapse count) or 'weight' (pre-normalised). 'norm' is
#'   optional and recomputed from 'count' if absent. Used when filename is NULL.
#' @param meta Data frame with metadata containing at least 'root_id' column. Must
#'   also contain 'top_nt' when `signed = TRUE` or `excluded_nts` is supplied. Used
#'   when filename is NULL.
#' @param signed Logical. Whether to use signed connectivity matrix (default: FALSE).
#'   When `TRUE`, `inhibitory_nts` must be supplied.
#' @param count_thresh Numeric. Minimum threshold count for postsynaptic connections
#'   (default: 0, i.e. no filtering — make any silent filtering visible to the caller).
#' @param inhibitory_nts Character vector of `top_nt` values whose pre-neurons should
#'   receive negative weights when `signed = TRUE`. The Python library has no
#'   per-organism default; you must supply this set explicitly when `signed = TRUE`.
#' @param excluded_nts Character vector of `top_nt` values whose pre-neurons should
#'   contribute *nothing* to W. Independent of `signed`. Use for transmitter classes
#'   whose sign at a given target depends on the receptor mix and so cannot be
#'   assigned a single sign safely.
#' @param lambda_max Numeric in `(0, 1)`. Target spectral radius of the rescaled
#'   connectivity matrix W~ (default `0.99`). Sets the leading-mode gain to
#'   `1/(1 - lambda_max)`. Lower values (e.g. `0.5`) are more appropriate for small
#'   graphs like *C. elegans*; the default is calibrated for whole-CNS *Drosophila*
#'   (BANC-scale) graphs. See [influence_calculator_r()] for the full discussion.
#'
#' @return InfluenceCalculator Python object
#' @export
#'
#' @examples
#' \dontrun{
#' # SQLite (legacy)
#' ic <- influence_calculator_py("connectome.sqlite")
#'
#' # Feather (e.g. the BANC edge list on the lab's GCS bucket)
#' ic <- influence_calculator_py("banc_888_edgelist_simple_v2.feather", meta = banc_meta)
#'
#' # Data frames (no temp file written)
#' ic <- influence_calculator_py(edgelist_simple = my_edges, meta = my_meta,
#'                                signed = TRUE,
#'                                inhibitory_nts = c("glutamate", "gaba",
#'                                                   "serotonin", "octopamine"),
#'                                lambda_max = 0.99)
#' }
influence_calculator_py <- function(filename = NULL,
                                    edgelist_simple = NULL,
                                    meta = NULL,
                                    signed = FALSE,
                                    count_thresh = 0,
                                    inhibitory_nts = NULL,
                                    excluded_nts = NULL,
                                    lambda_max = 0.99) {
  # Validate input arguments
  if (is.null(filename) && is.null(edgelist_simple)) {
    stop("Either filename or edgelist_simple (and meta) must be provided")
  }
  if (!is.null(filename) && (!is.null(edgelist_simple) || !is.null(meta))) {
    stop("Provide either filename OR edgelist_simple/meta, not both")
  }
  if (signed && (is.null(inhibitory_nts) || length(inhibitory_nts) == 0L)) {
    stop("signed = TRUE requires `inhibitory_nts` to be specified as a ",
         "character vector of neurotransmitter names matching values in ",
         "meta$top_nt.")
  }
  if (!is.numeric(lambda_max) || length(lambda_max) != 1L ||
      !is.finite(lambda_max) || lambda_max <= 0 || lambda_max >= 1) {
    stop("lambda_max must satisfy 0 < lambda_max < 1; got ", lambda_max, ".")
  }

  # Use r-reticulate environment exclusively
  tryCatch({
    reticulate::use_condaenv("r-reticulate", required = TRUE)
  }, error = function(e) {
    if (grepl("Unable to find conda binary", e$message)) {
      stop("Conda not found. Install conda/miniconda first, then run ",
           "install_python_influence_calculator()\n",
           "Or use the R implementation: influence_calculator_r()")
    } else {
      stop("Failed to activate r-reticulate environment: ", e$message, "\n",
           "Run: install_python_influence_calculator()\n",
           "Or use the R implementation: influence_calculator_r()")
    }
  })

  # Import Python module with enhanced error handling
  ic_module <- tryCatch({
    reticulate::import("InfluenceCalculator")
  }, error = function(e) {
    stop("ConnectomeInfluenceCalculator not found in r-reticulate environment.\n",
         "Run: install_python_influence_calculator()\n",
         "Or use the R implementation: influence_calculator_r()\n",
         "Original error: ", e$message)
  })

  # Convert NT vectors to lists for clean Python set() coercion (NULL stays NULL)
  inhibitory_py <- if (length(inhibitory_nts) > 0L) as.list(as.character(inhibitory_nts)) else NULL
  excluded_py   <- if (length(excluded_nts)   > 0L) as.list(as.character(excluded_nts))   else NULL

  ic <- tryCatch({
    if (!is.null(edgelist_simple)) {
      # DataFrame path -- no temp SQLite needed. From v0.2.0 the DataFrame
      # edge list is the primary constructor (__init__) itself, so we call
      # the class directly rather than a from_* classmethod.
      ic_module$InfluenceCalculator(
        edgelist_df    = edgelist_simple,
        meta_df        = meta,
        signed         = signed,
        count_thresh   = as.integer(count_thresh),
        inhibitory_nts = inhibitory_py,
        excluded_nts   = excluded_py,
        lambda_max     = lambda_max
      )
    } else {
      # File path -- dispatch on extension to the matching from_* classmethod.
      ext <- tolower(tools::file_ext(filename))
      switch(ext,
        "sqlite" = ,
        "db"     = ic_module$InfluenceCalculator$from_sql(
          filename       = filename,
          signed         = signed,
          count_thresh   = as.integer(count_thresh),
          inhibitory_nts = inhibitory_py,
          excluded_nts   = excluded_py,
          lambda_max     = lambda_max
        ),
        "csv"    = ic_module$InfluenceCalculator$from_csv(
          edgelist_path  = filename,
          meta_path      = NULL,
          signed         = signed,
          count_thresh   = as.integer(count_thresh),
          inhibitory_nts = inhibitory_py,
          excluded_nts   = excluded_py,
          lambda_max     = lambda_max
        ),
        "parquet" = ic_module$InfluenceCalculator$from_parquet(
          edgelist_path  = filename,
          meta_path      = NULL,
          signed         = signed,
          count_thresh   = as.integer(count_thresh),
          inhibitory_nts = inhibitory_py,
          excluded_nts   = excluded_py,
          lambda_max     = lambda_max
        ),
        "feather" = ,
        "arrow"   = ic_module$InfluenceCalculator$from_feather(
          edgelist_path  = filename,
          meta_path      = NULL,
          signed         = signed,
          count_thresh   = as.integer(count_thresh),
          inhibitory_nts = inhibitory_py,
          excluded_nts   = excluded_py,
          lambda_max     = lambda_max
        ),
        # Fallback: try the SQLite loader (matches pre-v0.2.0 behaviour,
        # where SQLite was the only supported on-disk format).
        ic_module$InfluenceCalculator$from_sql(
          filename       = filename,
          signed         = signed,
          count_thresh   = as.integer(count_thresh),
          inhibitory_nts = inhibitory_py,
          excluded_nts   = excluded_py,
          lambda_max     = lambda_max
        )
      )
    }
  }, error = function(e) {
    stop("Failed to create InfluenceCalculator: ", e$message)
  })

  class(ic) <- c("InfluenceCalculatorPy", class(ic))
  return(ic)
}

#' Calculate Influence Scores Using Python Implementation
#'
#' Calculate influence scores for seed neurons using the Python implementation.
#'
#' @param ic InfluenceCalculator Python object created by `influence_calculator_py()`.
#' @param seed_ids Character vector. Root IDs of seed neurons.
#' @param silenced_neurons Character vector. Root IDs of neurons to silence
#'   (default: empty vector).
#' @param seed_name Optional, a seed name for the seed you are running. Will be
#'   added as a column value.
#' @param const Constant value added to log(influence) to ensure non-negative adjusted
#'   influence scores. Should be set to -log(minimum_accepted_influence) where
#'   minimum_accepted_influence is the smallest influence value considered meaningful.
#'   Default 24 corresponds to minimum_accepted_influence = exp(-24) ~ 3.78e-11.
#'
#' @return Data frame with columns: matrix_index, id, is_seed, influence score, and
#'   adjusted_influence (sign-preserving in signed mode).
#' @export
#'
#' @examples
#' \dontrun{
#' ic <- influence_calculator_py("connectome.sqlite")
#' results <- calculate_influence_py(ic, seed_ids = c(123, 456))
#' }
calculate_influence_py <- function(ic,
                                   seed_ids,
                                   silenced_neurons = numeric(0),
                                   seed_name = NULL,
                                   const = 24) {
  if (!inherits(ic, "InfluenceCalculatorPy")) {
    stop("ic must be an InfluenceCalculator Python object created by influence_calculator_py()")
  }

  # Convert R vectors to appropriate Python types
  seed_ids_py <- as.list(seed_ids)
  silenced_neurons_py <- if (length(silenced_neurons) == 0) list() else as.list(silenced_neurons)

  # adjust = FALSE: return only the raw influence column. We apply the
  # log/const transform once on the R side below (using this function's
  # `const`), which keeps the output columns exactly as documented and
  # avoids the Python default (adjust = TRUE, adjust_const = 24) silently
  # adding three differently-scaled adjusted columns.
  result <- tryCatch({
    ic$calculate_influence(seed_ids = seed_ids_py,
                           silenced_neurons = silenced_neurons_py,
                           adjust = FALSE)
  }, error = function(e) {
    stop("Failed to calculate influence: ", e$message)
  })

  # reticulate auto-converts pandas DataFrame to R data.frame
  r_result <- result

  # Ensure id column is character type (prevent scientific notation for large IDs)
  if ("id" %in% names(r_result)) {
    r_result$id <- as.character(r_result$id)
  }
  if (!is.null(seed_name)) {
    r_result$seed <- seed_name
  }

  # Add adjusted_influence column with the same sign-preserving formula the
  # Python `adjust_influence` uses: sign(x) * (log(max(|x|, exp(-const))) + const).
  influence_col <- grep("Influence_score", names(r_result), value = TRUE)[1]
  if (!is.na(influence_col)) {
    influence_values <- r_result[[influence_col]]
    floor_val <- exp(-const)
    mag <- pmax(abs(influence_values), floor_val)
    adjusted_inf <- sign(influence_values) * (log(mag) + const)
    adjusted_inf[abs(influence_values) < floor_val] <- 0
    r_result$adjusted_influence <- adjusted_inf
  }

  return(r_result)
}
