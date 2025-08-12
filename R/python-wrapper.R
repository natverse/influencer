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
  reticulate::use_condaenv("r-reticulate", required = TRUE)
  
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
#' Create an InfluenceCalculator object using the Python library from the r-reticulate environment.
#' Can accept either a SQLite database file or data frames.
#'
#' @param filename Character. Path to SQLite database file containing connectome data.
#' @param edgelist_simple Data frame with edge list containing columns 'pre', 'post', 
#'   'count', and 'norm'. Used when filename is NULL.
#' @param meta Data frame with metadata containing at least 'root_id' column.
#'   Used when filename is NULL.
#' @param signed Logical. Whether to use signed connectivity matrix (default: FALSE).
#' @param count_thresh Numeric. Minimum threshold count for postsynaptic connections
#'   (default: 5).
#'
#' @return InfluenceCalculator Python object
#' @export
#'
#' @examples
#' \dontrun{
#' # Using SQLite database
#' ic <- influence_calculator_py("connectome.sqlite")
#' 
#' # Using data frames
#' ic <- influence_calculator_py(edgelist_simple = my_edges, meta = my_meta)
#' }
influence_calculator_py <- function(filename = NULL, edgelist_simple = NULL, meta = NULL, 
                                   signed = FALSE, count_thresh = 5) {
  # Validate input arguments
  if (is.null(filename) && (is.null(edgelist_simple) || is.null(meta))) {
    stop("Either filename or both edgelist_simple and meta must be provided")
  }
  
  if (!is.null(filename) && (!is.null(edgelist_simple) || !is.null(meta))) {
    stop("Provide either filename OR edgelist_simple/meta, not both")
  }
  
  temp_db_path <- NULL
  
  tryCatch({
    # Use r-reticulate environment exclusively
    reticulate::use_condaenv("r-reticulate", required = TRUE)
    
    # Import Python module with enhanced error handling
    ic_module <- tryCatch({
      reticulate::import("InfluenceCalculator")
    }, error = function(e) {
      stop("ConnectomeInfluenceCalculator not found in r-reticulate environment.\n",
           "Run: install_python_influence_calculator()\n",
           "Or use the R implementation: influence_calculator_r()\n",
           "Original error: ", e$message)
    })
    
    # Determine the filename to use for the Python object
    final_filename <- filename
    
    # If data frames provided, create temporary SQLite database
    if (is.null(filename)) {
      temp_db_path <- create_temp_sqlite(edgelist_simple, meta, count_thresh)
      final_filename <- temp_db_path
    }
    
    # Create InfluenceCalculator object
    ic <- tryCatch({
      ic_module$InfluenceCalculator(filename = final_filename, 
                                    signed = signed, 
                                    count_thresh = count_thresh)
    }, error = function(e) {
      # Clean up temporary file if creation failed
      if (!is.null(temp_db_path) && file.exists(temp_db_path)) {
        unlink(temp_db_path)
      }
      stop("Failed to create InfluenceCalculator: ", e$message)
    })
    
    # Store temp file info for later cleanup
    attr(ic, "temp_db_path") <- temp_db_path
    
    class(ic) <- c("InfluenceCalculatorPy", class(ic))
    return(ic)
    
  }, error = function(e) {
    # Clean up temporary file on any error
    if (!is.null(temp_db_path) && file.exists(temp_db_path)) {
      unlink(temp_db_path)
    }
    stop(e$message)
  })
}

#' Calculate Influence Scores Using Python Implementation
#'
#' Calculate influence scores for seed neurons using the Python implementation.
#'
#' @param ic InfluenceCalculator Python object created by `influence_calculator_py()`.
#' @param seed_ids Character vector. Root IDs of seed neurons.
#' @param silenced_neurons Character vector. Root IDs of neurons to silence
#'   (default: empty vector).
#'
#' @return Data frame with columns: matrix_index, id, is_seed, influence score, and adjusted_influence.
#' @export
#'
#' @examples
#' \dontrun{
#' ic <- influence_calculator_py("connectome.sqlite")
#' results <- calculate_influence_py(ic, seed_ids = c(123, 456))
#' }
calculate_influence_py <- function(ic, seed_ids, silenced_neurons = numeric(0)) {
  if (!inherits(ic, "InfluenceCalculatorPy")) {
    stop("ic must be an InfluenceCalculator Python object created by influence_calculator_py()")
  }
  
  temp_db_path <- attr(ic, "temp_db_path")
  
  tryCatch({
    # Convert R vectors to appropriate Python types
    seed_ids_py <- as.list(seed_ids)
    if (length(silenced_neurons) == 0) {
      silenced_neurons_py <- list()
    } else {
      silenced_neurons_py <- as.list(silenced_neurons)
    }
    
    # Call Python method
    result <- tryCatch({
      ic$calculate_influence(seed_ids = seed_ids_py, 
                             silenced_neurons = silenced_neurons_py)
    }, error = function(e) {
      stop("Failed to calculate influence: ", e$message)
    })
    
    # Convert pandas DataFrame to R data frame
    r_result <- reticulate::py_to_r(result)
    
    # Ensure id column is character type (prevent scientific notation for large IDs)
    if ("id" %in% names(r_result)) {
      r_result$id <- as.character(r_result$id)
    }
    
    # Add adjusted influence column: log(influence) + 24, with floor at 0
    influence_col <- grep("Influence_score", names(r_result), value = TRUE)[1]
    if (!is.na(influence_col)) {
      influence_values <- r_result[[influence_col]]
      adjusted_inf <- log(influence_values) + 24
      adjusted_inf[adjusted_inf < 0] <- 0  # Apply floor constraint
      r_result$adjusted_influence <- adjusted_inf
    }
    
    return(r_result)
    
  }, error = function(e) {
    stop(e$message)
  }, finally = {
    # Clean up temporary database file
    if (!is.null(temp_db_path) && file.exists(temp_db_path)) {
      unlink(temp_db_path)
      attr(ic, "temp_db_path") <- NULL
    }
  })
}

#' Create Temporary SQLite Database from Data Frames
#'
#' Internal helper function to create a temporary SQLite database from R data frames
#' for use with the Python InfluenceCalculator library.
#'
#' @param edgelist_simple Data frame with edge list
#' @param meta Data frame with metadata
#' @param count_thresh Minimum synapse count threshold
#'
#' @return Path to temporary SQLite database file
create_temp_sqlite <- function(edgelist_simple, meta, count_thresh) {
  # Validate required columns
  required_edge_cols <- c("pre", "post", "count", "norm")
  missing_edge_cols <- setdiff(required_edge_cols, names(edgelist_simple))
  if (length(missing_edge_cols) > 0) {
    stop("edgelist_simple is missing required columns: ", 
         paste(missing_edge_cols, collapse = ", "))
  }
  
  required_meta_cols <- c("root_id")
  missing_meta_cols <- setdiff(required_meta_cols, names(meta))
  if (length(missing_meta_cols) > 0) {
    stop("meta is missing required columns: ", 
         paste(missing_meta_cols, collapse = ", "))
  }
  
  # Create temporary file
  temp_db_path <- tempfile(pattern = "influencer_", fileext = ".sqlite")
  
  # Connect to SQLite database
  con <- DBI::dbConnect(RSQLite::SQLite(), temp_db_path)
  
  tryCatch({
    # Filter edgelist by count threshold
    filtered_edgelist <- edgelist_simple[edgelist_simple$count >= count_thresh, , drop = FALSE]
    
    # Add post_count column if not present
    if (!"post_count" %in% names(filtered_edgelist)) {
      filtered_edgelist$post_count <- round(filtered_edgelist$count / filtered_edgelist$norm)
    }
    
    # Ensure ID columns are character type before writing to SQLite
    meta_to_write <- meta
    if ("root_id" %in% names(meta_to_write)) {
      meta_to_write$root_id <- as.character(meta_to_write$root_id)
    }
    if ("id" %in% names(meta_to_write)) {
      meta_to_write$id <- as.character(meta_to_write$id)
    }
    
    filtered_edgelist_to_write <- filtered_edgelist
    if ("pre" %in% names(filtered_edgelist_to_write)) {
      filtered_edgelist_to_write$pre <- as.character(filtered_edgelist_to_write$pre)
    }
    if ("post" %in% names(filtered_edgelist_to_write)) {
      filtered_edgelist_to_write$post <- as.character(filtered_edgelist_to_write$post)
    }
    
    # Write tables to database
    DBI::dbWriteTable(con, "meta", meta_to_write, overwrite = TRUE)
    DBI::dbWriteTable(con, "edgelist_simple", filtered_edgelist_to_write, overwrite = TRUE)
    
  }, error = function(e) {
    # Clean up on error
    DBI::dbDisconnect(con)
    if (file.exists(temp_db_path)) {
      unlink(temp_db_path)
    }
    stop("Failed to create temporary SQLite database: ", e$message)
  }, finally = {
    DBI::dbDisconnect(con)
  })
  
  return(temp_db_path)
}