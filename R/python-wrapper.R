#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Get platform-appropriate r-miniconda path
#' @noRd
get_r_miniconda_path <- function() {
  if (Sys.info()[["sysname"]] == "Darwin") {
    file.path(Sys.getenv("HOME"), "Library", "r-miniconda")
  } else {
    file.path(Sys.getenv("HOME"), ".local", "share", "r-miniconda")
  }
}

#' Detect Current Python Environment
#'
#' Internal helper function to detect the currently active Python environment.
#'
#' @return List with environment information (type, path, name) or NULL if none active
get_current_python_env <- function() {
  tryCatch({
    py_config <- reticulate::py_config()
    if (!is.null(py_config$python)) {
      # Try to extract conda environment name from path
      conda_env_name <- NULL
      if (grepl("/envs/", py_config$python)) {
        env_path_parts <- strsplit(py_config$python, "/envs/")[[1]]
        if (length(env_path_parts) == 2) {
          env_name_part <- strsplit(env_path_parts[2], "/")[[1]][1]
          conda_env_name <- env_name_part
        }
      }
      
      return(list(
        type = if (!is.null(conda_env_name)) "conda" else "python_path",
        path = py_config$python,
        name = conda_env_name
      ))
    }
    return(NULL)
  }, error = function(e) {
    return(NULL)
  })
}

#' Switch to Python Environment with Fallback
#'
#' Internal helper function to switch to a specific Python environment,
#' with automatic fallback to architecture-compatible environments on Apple Silicon.
#'
#' @param target_env Character. Name of target conda environment or "auto" for automatic selection
#' @param original_env List. Original environment info from get_current_python_env()
#'
#' @return List with success status and messages
switch_to_python_env <- function(target_env = "auto", original_env = NULL) {
  messages <- character(0)
  
  # Auto-detect best environment for Apple Silicon
  if (target_env == "auto") {
    system_info <- Sys.info()
    
    # Check if we're on macOS and have the influence-py environment available
    use_uv_env <- FALSE
    if (system_info["sysname"] == "Darwin") {
      # Check if the influence-py environment exists (indicates Apple Silicon setup)
      uv_env_exists <- tryCatch({
        system("conda env list | grep -q influence-py", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
      }, error = function(e) FALSE)
      
      if (uv_env_exists) {
        use_uv_env <- TRUE
        # Check if we're running under Rosetta (x86_64 R on ARM hardware)
        if (system_info["machine"] == "x86_64") {
          messages <- c(messages, "[INFO] Apple Silicon setup detected (R running via Rosetta)")
        } else {
          messages <- c(messages, "[INFO] Apple Silicon environment detected")  
        }
      }
      
      # Use ARM environment if available
      if (use_uv_env) {
        target_env <- "influence-py"
        messages <- c(messages, "[INFO] Using dedicated ARM environment for optimal Python compatibility")
      }
    }
    
    # Fallback to current environment if auto-detection doesn't apply
    if (target_env == "auto") {
      if (!is.null(original_env)) {
        target_env <- original_env$name %||% original_env$path
      } else {
        target_env <- NULL
      }
    }
  }
  
  # Try to switch environment
  if (!is.null(target_env) && target_env != "") {
    success <- tryCatch({
      reticulate::use_condaenv(target_env, required = TRUE)
      messages <- c(messages, paste("[SUCCESS] Switched to Python environment:", target_env))
      TRUE
    }, error = function(e) {
      if (grepl("another version of Python.*has already been initialized", e$message)) {
        # Check if we're already in a compatible environment
        current_python <- tryCatch({
          reticulate::py_config()$python
        }, error = function(e) "unknown")
        
        if (grepl(target_env, current_python, fixed = TRUE)) {
          messages <<- c(messages, paste("[INFO] Already using", target_env, "environment"))
          target_env <<- basename(dirname(current_python))  # Update target_env to reflect reality
        } else {
          messages <<- c(messages, "[WARNING] Python already initialized - using current environment")
          messages <<- c(messages, "[INFO] To use different environment, restart R session")
          target_env <<- basename(dirname(current_python)) %||% "current"
        }
        TRUE
      } else if (grepl("incompatible architecture", e$message)) {
        messages <<- c(messages, "[WARNING] Architecture mismatch detected")
        messages <<- c(messages, "[INFO] This may indicate x86_64 R trying to use ARM64 Python")
        messages <<- c(messages, "[INFO] Using current Python environment instead")
        # Don't change target_env, let it fall back
        FALSE
      } else {
        messages <<- c(messages, paste("[WARNING] Failed to switch to environment", target_env, "- using current environment"))
        messages <<- c(messages, paste("  Error:", e$message))
        FALSE
      }
    })
    
    return(list(success = success, messages = messages, environment = target_env))
  } else {
    return(list(success = TRUE, messages = c("Using current Python environment"), environment = NULL))
  }
}

#' Restore Original Python Environment
#'
#' Internal helper function to restore the original Python environment.
#'
#' @param original_env List. Original environment info from get_current_python_env()
#' @param switched_env Character. Environment that was switched to
#'
#' @return Invisible TRUE
restore_python_env <- function(original_env = NULL, switched_env = NULL) {
  if (!is.null(switched_env) && switched_env == "influence-py" && !is.null(original_env)) {
    tryCatch({
      if (original_env$type == "conda" && !is.null(original_env$name)) {
        reticulate::use_condaenv(original_env$name)
        message("[INFO] Restored original Python environment: ", original_env$name)
      } else if (!is.null(original_env$path)) {
        reticulate::use_python(original_env$path)
        message("[INFO] Restored original Python environment")
      }
    }, error = function(e) {
      # Silently fail - environment restoration is best-effort
    })
  }
  invisible(TRUE)
}

#' Check Python Implementation Compatibility
#'
#' Internal function to assess whether Python implementation is likely to work
#' and provide guidance to users.
#'
#' @return List with compatibility status and user guidance
check_python_compatibility <- function() {
  system_info <- Sys.info()
  messages <- character(0)
  compatible <- TRUE
  
  # Check for common architecture issues
  if (system_info["sysname"] == "Darwin" && system_info["machine"] == "x86_64") {
    # Check if we're on Apple Silicon hardware
    hw_arch <- tryCatch({
      system("uname -m", intern = TRUE)
    }, error = function(e) "unknown")
    
    if (hw_arch == "arm64") {
      compatible <- FALSE
      messages <- c(messages,
        "Architecture compatibility issue detected:",
        "- You're running x86_64 R on Apple Silicon hardware",
        "- The Python wrapper may encounter architecture conflicts",
        "",
        "Recommendations:",
        "1. Use the optimized R implementation: influence_calculator_r()",
        "   (No Python dependencies, 4x faster through caching)",
        "2. Install native ARM R from CRAN for better Python compatibility",
        "3. If you need Python: restart R, then install_python_influence_calculator()")
    }
  }
  
  return(list(compatible = compatible, messages = messages))
}

#' Set Python Environment for InfluenceCalculator
#'
#' Configure the Python environment for use with the ConnectomeInfluenceCalculator
#' Python library.
#'
#' @param conda_env Character. Name of conda environment containing the required
#'   Python packages. If NULL, uses the default Python environment.
#' @param python_path Character. Path to specific Python executable. If NULL,
#'   uses conda_env or default.
#'
#' @return Invisible TRUE if successful
#' @export
#'
#' @examples
#' \dontrun{
#' set_python_env("ic-env")
#' }
set_python_env <- function(conda_env = NULL, python_path = NULL) {
  if (!is.null(conda_env)) {
    reticulate::use_condaenv(conda_env, required = TRUE)
  } else if (!is.null(python_path)) {
    reticulate::use_python(python_path, required = TRUE)
  }
  
  # Check if InfluenceCalculator is available
  tryCatch({
    ic <- reticulate::import("InfluenceCalculator")
    message("InfluenceCalculator python library loaded successfully")
    invisible(TRUE)
  }, error = function(e) {
    stop("Failed to import InfluenceCalculator. Please ensure it's installed in your Python environment.\n",
         "Error: ", e$message)
  })
}

#' Create InfluenceCalculator Python Object
#'
#' Create an InfluenceCalculator object using the Python library.
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
  
  # Store original Python environment for restoration
  original_env <- get_current_python_env()
  switched_env <- NULL
  temp_db_path <- NULL
  
  tryCatch({
    # Switch to appropriate Python environment (auto-detects Apple Silicon needs)
    env_switch_result <- switch_to_python_env("auto", original_env)
    switched_env <- env_switch_result$environment
    
    # Print environment switch messages
    if (length(env_switch_result$messages) > 0) {
      for (msg in env_switch_result$messages) {
        message(msg)
      }
    }
    
    # Import Python module with enhanced error handling
    ic_module <- tryCatch({
      reticulate::import("InfluenceCalculator")
    }, error = function(e) {
      # Handle architecture mismatch specifically
      if (grepl("incompatible architecture", e$message) || grepl("mach-o file.*have 'arm64'.*need 'x86_64'", e$message)) {
        stop("Architecture mismatch detected.\n",
             "Your x86_64 R cannot use ARM64 Python libraries.\n",
             "Solutions:\n",
             "  1. Use the optimized R implementation: influence_calculator_r()\n",
             "  2. Install native ARM R from CRAN for your Apple Silicon Mac\n",
             "  3. Restart R and run: install_python_influence_calculator()\n",
             "     (This will install in a compatible environment)\n",
             "Original error: ", e$message)
      }
      
      # If import fails, provide helpful guidance
      if (!env_switch_result$success || !grepl("influence-py", switched_env %||% "")) {
        # Check if influence-py environment exists
        env_exists <- tryCatch({
          system("conda env list | grep influence-py", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
        }, error = function(e) FALSE)
        
        if (env_exists) {
          stop("ConnectomeInfluenceCalculator not found in current environment.\n",
               "The influence-py environment exists but may not be accessible.\n",
               "Try restarting R and running: install_python_influence_calculator()\n",
               "Or use the R implementation: influence_calculator_r()\n",
               "Original error: ", e$message)
        } else {
          stop("ConnectomeInfluenceCalculator not found.\n",
               "Run: install_python_influence_calculator()\n",
               "Or use the R implementation: influence_calculator_r()\n",
               "Original error: ", e$message)
        }
      } else {
        stop("Failed to import ConnectomeInfluenceCalculator.\n",
             "The R implementation is available: influence_calculator_r()\n",
             "Original error: ", e$message)
      }
    })
    
    # Determine the filename to use for the Python object
    final_filename <- filename
    
    # If data frames provided, create temporary SQLite database
    if (is.null(filename)) {
      temp_db_path <- create_temp_sqlite(edgelist_simple, meta, count_thresh)
      final_filename <- temp_db_path
    }
    
    # Create InfluenceCalculator object with error handling for cleanup
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
    
    # Store environment and temp file info for later cleanup
    attr(ic, "temp_db_path") <- temp_db_path
    attr(ic, "original_env") <- original_env
    attr(ic, "switched_env") <- switched_env
    
    class(ic) <- c("InfluenceCalculatorPy", class(ic))
    return(ic)
    
  }, error = function(e) {
    # Clean up temporary file on any error
    if (!is.null(temp_db_path) && file.exists(temp_db_path)) {
      unlink(temp_db_path)
    }
    # Restore original environment on error
    restore_python_env(original_env, switched_env)
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
  
  # Get environment info for restoration
  original_env <- attr(ic, "original_env")
  switched_env <- attr(ic, "switched_env")
  temp_db_path <- attr(ic, "temp_db_path")
  
  tryCatch({
    # Convert R vectors to appropriate Python types
    if (length(silenced_neurons) == 0) {
      silenced_neurons <- list()
    }
    
    # Call Python method with error handling and cleanup
    result <- tryCatch({
      ic$calculate_influence(seed_ids = seed_ids, 
                             silenced_neurons = silenced_neurons)
    }, error = function(e) {
      stop("Failed to calculate influence: ", e$message)
    })
    
    # Convert pandas DataFrame to R data frame
    r_result <- reticulate::py_to_r(result)
    
    # Ensure id column is character type (prevent scientific notation for large IDs)
    # This handles cases where pandas int64 columns become numeric in R conversion
    if ("id" %in% names(r_result)) {
      r_result$id <- as.character(r_result$id)
    }
    
    # Add adjusted influence column: log(influence) + 24, with floor at 0
    # Find the influence score column (may have different names)
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
    
    # Restore original Python environment
    restore_python_env(original_env, switched_env)
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