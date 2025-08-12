#' Install Python ConnectomeInfluenceCalculator Library
#'
#' This function installs the Python ConnectomeInfluenceCalculator library
#' and its dependencies (PETSc and SLEPc) into the r-reticulate environment
#' using an optimized installation script.
#'
#' @param force_reinstall Logical. Whether to reinstall even if already present (default: FALSE).
#'
#' @return Invisible TRUE if successful, throws error otherwise.
#' @export
#'
#' @details
#' This function runs an installation script that:
#' 
#' 1. Automatically detects or installs PETSc and SLEPc (via Homebrew or source builds)
#' 2. Uses the r-reticulate conda environment exclusively
#' 3. Installs ConnectomeInfluenceCalculator with automatic issue resolution
#' 4. Tests the installation and provides configuration information
#' 
#' **Installation Features:**
#' - **Automatic PETSc/SLEPc Detection**: Searches PATH for existing installations
#' - **Homebrew Integration**: Uses Homebrew when available, with fallback to source builds
#' - **Local Builds**: Source builds install to `~/.local/influencer/` if needed
#' - **pyproject.toml Fixes**: Automatically resolves upstream license format issues
#' - **Environment Configuration**: Saves configuration to `~/.influencer_env` for reuse
#' 
#' The installation uses only the r-reticulate environment and does not create
#' additional conda environments, simplifying Python environment management.
#'
#' @examples
#' \dontrun{
#' # Install with default settings
#' install_python_influence_calculator()
#' 
#' # Force reinstallation
#' install_python_influence_calculator(force_reinstall = TRUE)
#' }
install_python_influence_calculator <- function(force_reinstall = FALSE) {
  # Check if already installed (unless force_reinstall)
  if (!force_reinstall) {
    already_installed <- tryCatch({
      reticulate::use_condaenv("r-reticulate", required = FALSE)
      reticulate::import("InfluenceCalculator")
      TRUE
    }, error = function(e) FALSE)
    
    if (already_installed) {
      message("[SUCCESS] ConnectomeInfluenceCalculator is already installed and importable")
      return(invisible(TRUE))
    }
  }
  
  # Find the installation script
  script_path <- system.file("install_python_deps.sh", package = "influencer")
  
  if (script_path == "" || !file.exists(script_path)) {
    stop("Installation script not found. Please reinstall the influencer package.")
  }
  
  # Make script executable
  system(paste("chmod +x", script_path))
  
  # Run the installation script
  message("Running Python dependencies installation script...")
  message("This will install into the r-reticulate environment")
  
  result <- system(script_path, wait = TRUE)
  
  if (result != 0) {
    stop("Installation script failed with exit code: ", result)
  }
  
  # Test installation
  test_result <- tryCatch({
    reticulate::use_condaenv("r-reticulate", required = TRUE)
    ic_module <- reticulate::import("InfluenceCalculator")
    message("[SUCCESS] ConnectomeInfluenceCalculator installed and imported successfully!")
    TRUE
  }, error = function(e) {
    stop("Installation completed but import failed: ", e$message,
         "\nTry restarting R and running: reticulate::py_config()")
  })
  
  invisible(TRUE)
}


#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

