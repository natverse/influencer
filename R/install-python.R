#' Install Python ConnectomeInfluenceCalculator Library
#'
#' This function helps install the Python ConnectomeInfluenceCalculator library
#' and its dependencies (PETSc and SLEPc) using conda. This method integrates
#' well with RStudio's r-miniconda environment system and includes automatic
#' fallback methods for common installation issues.
#'
#' @param method Character. Installation method: "conda" (recommended) or "pip".
#' @param conda_env Character. Name of conda environment to create/use. 
#'   If NULL, uses "ic-venv" (recommended by ConnectomeInfluenceCalculator).
#' @param python_version Character. Python version for new conda environment 
#'   (default: "3.12" for compatibility).
#' @param force_reinstall Logical. Whether to reinstall even if already present.
#'
#' @return Invisible TRUE if successful, throws error otherwise.
#' @export
#'
#' @details
#' The function installs the ConnectomeInfluenceCalculator using robust methods
#' with automatic fallbacks for common issues. For the conda method (recommended), it:
#' 
#' 1. Fixes common conda solver issues (switches to classic solver)
#' 2. Creates conda environment "ic-venv" with Python 3.13.1 (if not specified)
#' 3. Installs PETSc/SLEPc with automatic MPI conflict detection and fallback
#' 4. Installs ConnectomeInfluenceCalculator with automatic pyproject.toml fixing
#' 
#' **Automatic Issue Resolution:**
#' - **MPI Conflicts**: Falls back to conda-forge pre-compiled packages when
#'   mixed MPI implementations (e.g., Homebrew Open MPI + Conda MPICH) cause failures
#' - **pyproject.toml Issues**: Automatically fixes upstream license format validation
#'   errors by converting `license = "BSD-3-Clause"` to `license = {text = "BSD-3-Clause"}`
#' - **Solver Issues**: Switches to classic conda solver to avoid libmamba problems
#' 
#' For the pip method, it attempts direct installation but this may fail on some
#' systems due to PETSc/SLEPc compilation issues.
#' 
#' **Troubleshooting Common Issues:**
#' 
#' - **libmamba solver errors**: Function automatically switches to classic solver
#' - **Missing libarchive**: Usually resolved by switching conda solver
#' - **MPI compiler conflicts**: Function automatically falls back to conda-forge pre-compiled packages
#' - **pyproject.toml validation errors**: Function automatically fixes license format issues
#' - **Import failures**: Restart R session and try `reticulate::py_config()`
#' - **Environment conflicts**: Specify a dedicated `conda_env` name
#' - **Installation hangs**: Check internet connection and GitHub access
#' - **Version conflicts**: Try using the exact versions from ConnectomeInfluenceCalculator README
#' 
#' **MPI Conflict Resolution:**
#' If you have mixed MPI implementations (e.g., Homebrew Open MPI + Conda MPICH),
#' the function automatically detects compilation failures and switches to conda-forge
#' pre-compiled packages, avoiding the need to compile against conflicting MPI libraries.
#'
#' @examples
#' \dontrun{
#' # Recommended: install using conda (creates "ic-venv" environment)
#' install_python_influence_calculator()
#' 
#' # Install in specific conda environment
#' install_python_influence_calculator(conda_env = "my-custom-env")
#' 
#' # Use different Python version
#' install_python_influence_calculator(python_version = "3.12")
#' 
#' # Force reinstallation
#' install_python_influence_calculator(force_reinstall = TRUE)
#' }
install_python_influence_calculator <- function(method = "conda", 
                                                conda_env = NULL,
                                                python_version = "3.12",
                                                force_reinstall = FALSE) {
  method <- match.arg(method, choices = c("conda", "pip"))
  
  # Set default environment name following ConnectomeInfluenceCalculator README
  # but adapted for RStudio r-miniconda environment
  if (is.null(conda_env)) {
    # Check if we're in RStudio environment with r-miniconda (platform-aware)
    if (Sys.info()[["sysname"]] == "Darwin") {
      r_miniconda_path <- file.path(Sys.getenv("HOME"), "Library", "r-miniconda")
    } else {
      r_miniconda_path <- file.path(Sys.getenv("HOME"), ".local", "share", "r-miniconda")
    }
    
    if (dir.exists(r_miniconda_path)) {
      conda_env <- "ic-venv"  # Use recommended name but within r-miniconda
      message("[INFO] RStudio r-miniconda detected - using 'ic-venv' environment")
      message("[INFO] This follows ConnectomeInfluenceCalculator README recommendations")
    } else {
      conda_env <- "ic-venv"  # Use recommended environment name from ConnectomeInfluenceCalculator
      message("[INFO] Using recommended environment name 'ic-venv' (from ConnectomeInfluenceCalculator README)")
    }
  }
  
  # Check for Python initialization conflicts
  python_initialized <- tryCatch({
    py_config <- reticulate::py_config()
    !is.null(py_config$python)
  }, error = function(e) FALSE)
  
  if (python_initialized && !is.null(conda_env)) {
    current_python <- reticulate::py_config()$python
    
    # Check platform-appropriate miniconda paths
    if (Sys.info()[["sysname"]] == "Darwin") {
      r_miniconda_path <- file.path(Sys.getenv("HOME"), "Library", "r-miniconda", "envs", conda_env, "bin", "python")
    } else {
      r_miniconda_path <- file.path(Sys.getenv("HOME"), ".local", "share", "r-miniconda", "envs", conda_env, "bin", "python")
    }
    standard_miniconda_path <- file.path("/opt/miniconda3/envs", conda_env, "bin", "python")
    
    if (!grepl(conda_env, current_python, fixed = TRUE)) {
      current_env <- basename(dirname(dirname(current_python)))
      message("[INFO] Python already initialized with environment: ", current_env)
      message("[INFO] Desired environment: ", conda_env)
      message("[INFO] To use the ", conda_env, " environment, restart your R session and run:")
      message("       install_python_influence_calculator(conda_env = '", conda_env, "')")
      message("[INFO] Continuing with current Python environment...")
      conda_env <- NULL  # Use current environment
    }
  }
  
  # Check if already installed (unless force_reinstall)
  if (!force_reinstall) {
    already_installed <- tryCatch({
      reticulate::import("InfluenceCalculator")
      TRUE
    }, error = function(e) FALSE)
    
    if (already_installed) {
      message("[SUCCESS] ConnectomeInfluenceCalculator is already installed and importable")
      return(invisible(TRUE))
    }
  }
  
  if (method == "conda") {
    install_with_conda(conda_env, python_version, force_reinstall)
  } else {
    install_with_pip(conda_env, force_reinstall)
  }
}

#' Helper function to create conda/pip commands
#' @noRd
make_pip_command <- function(conda_env, python_path, pip_args) {
  if (!is.null(conda_env) && is.character(python_path) && !grepl("conda run", python_path) && file.exists(python_path)) {
    paste(python_path, "-m pip", pip_args)
  } else if (!is.null(conda_env)) {
    paste("conda run -n", conda_env, "pip", pip_args)
  } else {
    paste("pip", pip_args)
  }
}

#' Helper function to install PETSc/SLEPc with MPI conflict fallback
#' @noRd
install_petsc_slepc_with_fallback <- function(conda_env) {
  # Method 1: Try standard installation (petsc + petsc4py, slepc + slepc4py)
  message("Attempting standard PETSc/SLEPc installation...")
  
  # Step 1: Install PETSc and petsc4py
  cmd1 <- paste("conda install -n", conda_env, "-c conda-forge petsc petsc4py -y")
  message("Running: ", cmd1)
  result1 <- system(cmd1, ignore.stdout = FALSE, ignore.stderr = FALSE)
  
  if (result1 == 0) {
    # Step 2: Install SLEPc and slepc4py  
    cmd2 <- paste("conda install -n", conda_env, "-c conda-forge slepc slepc4py -y")
    message("Running: ", cmd2)
    result2 <- system(cmd2, ignore.stdout = FALSE, ignore.stderr = FALSE)
    
    if (result2 == 0) {
      message("[SUCCESS] Standard PETSc/SLEPc installation completed")
      return(TRUE)
    }
  }
  
  # Method 2: Fallback for MPI conflicts - use conda-forge pre-compiled packages only
  warning("Standard installation failed (likely MPI compiler conflicts). ",
          "Trying conda-forge pre-compiled packages fallback...")
  message("This method avoids MPI compiler conflicts by using pre-compiled binaries.")
  
  # Use conda environment-based installation (works across platforms)
  cmd_fallback <- paste("conda install -n", conda_env, "-c conda-forge petsc4py slepc4py -y")
  message("Running fallback: ", cmd_fallback)
  result_fallback <- system(cmd_fallback, ignore.stdout = FALSE, ignore.stderr = FALSE)
  
  if (result_fallback == 0) {
    message("[SUCCESS] MPI conflict fallback installation completed successfully!")
    message("[INFO] Used conda-forge pre-compiled packages to avoid MPI compiler issues")
    return(TRUE)
  } else {
    stop("Both standard and fallback installations failed. This may be due to:\n",
         "1. Network connectivity issues\n",
         "2. Conda configuration problems\n", 
         "3. Severe environment conflicts\n",
         "Try running 'conda config --set solver classic' manually and retry.")
  }
}

#' Helper function to install with pyproject.toml license format fix
#' @noRd
install_with_pyproject_fix <- function(conda_env, python_path) {
  temp_dir <- file.path(tempdir(), "ConnectomeInfluenceCalculator_fixed")
  if (dir.exists(temp_dir)) {
    unlink(temp_dir, recursive = TRUE)
  }
  
  # Clone repository
  message("Cloning repository to apply pyproject.toml fix...")
  clone_result <- system(paste("git clone https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator.git", temp_dir), 
                        ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  if (clone_result == 0) {
    # Fix pyproject.toml license issue
    pyproject_path <- file.path(temp_dir, "pyproject.toml")
    if (file.exists(pyproject_path)) {
      message("Applying automatic pyproject.toml license format fix...")
      # Read with warn = FALSE to suppress incomplete final line warning
      pyproject_content <- readLines(pyproject_path, warn = FALSE)
      
      # Fix the license format from string to object format
      pyproject_content <- gsub('^license = "BSD-3-Clause"', 'license = {text = "BSD-3-Clause"}', pyproject_content)
      
      # Ensure the file ends with a newline to avoid incomplete final line issues
      if (length(pyproject_content) > 0 && !grepl("\\n$", pyproject_content[length(pyproject_content)])) {
        pyproject_content[length(pyproject_content)] <- paste0(pyproject_content[length(pyproject_content)], "\\n")
      }
      
      # Write with a final newline
      writeLines(pyproject_content, pyproject_path)
      
      # Install from fixed local copy
      cmd_local <- make_pip_command(conda_env, python_path, paste("install", temp_dir))
      
      message("Installing ConnectomeInfluenceCalculator with pyproject.toml fix: ", cmd_local)
      result_local <- system(cmd_local)
      
      # Cleanup
      unlink(temp_dir, recursive = TRUE)
      
      if (result_local == 0) {
        message("[SUCCESS] ConnectomeInfluenceCalculator installed with automatic pyproject.toml fix!")
        message("[INFO] This fix resolves the upstream license format validation issue")
        return(TRUE)
      } else {
        warning("Installation failed even with pyproject.toml fix")
        return(FALSE)
      }
    } else {
      warning("Could not find pyproject.toml file in cloned repository")
      unlink(temp_dir, recursive = TRUE)
      return(FALSE)
    }
  } else {
    warning("Failed to clone repository for pyproject.toml fix")
    return(FALSE)
  }
}

#' Install using conda (recommended method)
#' @noRd
install_with_conda <- function(conda_env, python_version, force_reinstall) {
  # Check if conda is available
  conda_available <- tryCatch({
    system("conda --version", intern = TRUE)
    TRUE
  }, error = function(e) {
    stop("conda is not available. Please install conda or use method='pip'")
  })
  
  # Fix common conda solver issues
  message("Checking conda configuration...")
  tryCatch({
    # Switch to classic solver to avoid libmamba issues
    system("conda config --set solver classic", ignore.stdout = TRUE, ignore.stderr = TRUE)
    message("Set conda solver to 'classic' to avoid libmamba issues")
  }, error = function(e) {
    warning("Could not set conda solver - may encounter libmamba issues: ", e$message)
  })
  
  if (!is.null(conda_env)) {
    message("Creating/updating conda environment: ", conda_env)
    
    # Create environment if it doesn't exist
    env_exists <- tryCatch({
      result <- system(paste("conda env list | grep", conda_env), intern = TRUE)
      length(result) > 0
    }, error = function(e) FALSE)
    
    if (!env_exists || force_reinstall) {
      cmd <- paste("conda create -n", conda_env, "python=" %||% python_version, "-y")
      message("Running: ", cmd)
      result <- system(cmd)
      if (result != 0) {
        stop("Failed to create conda environment")
      }
    }
    
    # Install PETSc and SLEPc following ConnectomeInfluenceCalculator README recommendations
    message("Installing PETSc and SLEPc (following ConnectomeInfluenceCalculator README)...")
    
    # Try standard installation first, with fallback for MPI conflicts
    mpi_success <- install_petsc_slepc_with_fallback(conda_env)
    
    # Install other dependencies
    message("Installing other dependencies...")
    cmd <- paste("conda install -n", conda_env, "-c conda-forge pandas numpy bidict -y")
    result <- system(cmd)
    
    # Activate environment for pip install - check multiple possible locations
    python_path <- NULL
    
    # Try r-miniconda path first (common in RStudio on macOS)
    if (Sys.info()[["sysname"]] == "Darwin") {
      r_miniconda_python <- file.path(Sys.getenv("HOME"), "Library", "r-miniconda", "envs", conda_env, "bin", "python")
      if (file.exists(r_miniconda_python)) {
        python_path <- r_miniconda_python
        message("[INFO] Using r-miniconda Python: ", python_path)
      }
    }
    
    # Try standard conda locations
    if (is.null(python_path)) {
      standard_paths <- c(
        file.path(Sys.getenv("HOME"), ".conda", "envs", conda_env, "bin", "python"),
        file.path("/opt/miniconda3/envs", conda_env, "bin", "python"),
        file.path("/usr/local/miniconda3/envs", conda_env, "bin", "python")
      )
      
      for (path in standard_paths) {
        if (file.exists(path)) {
          python_path <- path
          break
        }
      }
    }
    
    # Fall back to conda run if direct path not found
    if (is.null(python_path)) {
      python_path <- paste("conda run -n", conda_env, "python")
      message("[INFO] Using conda run command: ", python_path)
    }
  } else {
    # Install in current environment
    message("Installing PETSc and SLEPc in current environment...")
    result <- system("conda install -c conda-forge petsc4py slepc4py -y")
    if (result != 0) {
      warning("Standard PETSc/SLEPc installation failed, trying alternative approach...")
      result_alt <- system("conda install -c conda-forge petsc4py slepc4py -y")
      if (result_alt != 0) {
        stop("Failed to install PETSc/SLEPc. This may be due to conda solver issues. ",
             "Try running 'conda config --set solver classic' manually and retry.")
      }
    }
    
    # Install other dependencies
    system("conda install -c conda-forge pandas numpy bidict -y")
    python_path <- "python"
  }
  
  # Install ConnectomeInfluenceCalculator from GitHub
  message("Installing ConnectomeInfluenceCalculator...")
  
  # Multiple installation approaches to handle various issues
  install_success <- FALSE
  
  # Method 1: Try git+https URL first, with smart pyproject.toml fix detection
  github_url <- "git+https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator.git"
  
  cmd <- make_pip_command(conda_env, python_path, paste("install", github_url))
  
  message("Trying method 1 (git): ", cmd)
  result <- system(cmd, intern = FALSE)
  
  # Capture stderr to check for specific pyproject.toml license error
  result_output <- system(cmd, intern = TRUE, ignore.stdout = FALSE, ignore.stderr = FALSE)
  
  if (result == 0) {
    install_success <- TRUE
    message("[SUCCESS] Standard git installation completed")
  } else if (any(grepl("project.license.*must be valid", result_output, ignore.case = TRUE)) || 
             any(grepl("license.*BSD-3-Clause", result_output, ignore.case = TRUE))) {
    # Detected pyproject.toml license format issue - jump directly to fix method
    warning("Detected pyproject.toml license format issue. Applying automatic fix...")
    install_success <- install_with_pyproject_fix(conda_env, python_path)
  } else {
    # Method 2: Try zip archive
    warning("Git installation failed (often due to pyproject.toml issues), trying zip archive...")
    github_url_zip <- "https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator/archive/refs/heads/main.zip"
    
    cmd_alt <- make_pip_command(conda_env, python_path, paste("install", github_url_zip))
    
    message("Trying method 2 (zip): ", cmd_alt)
    result_alt <- system(cmd_alt)
    if (result_alt == 0) {
      install_success <- TRUE
    } else {
      # Method 3: Try installing with --no-build-isolation to bypass pyproject.toml issues
      warning("Standard installations failed, trying method 3 (bypass build isolation)...")
      
      cmd_bypass <- make_pip_command(conda_env, python_path, paste("install --no-build-isolation", github_url))
      
      message("Trying method 3 (bypass): ", cmd_bypass)
      result_bypass <- system(cmd_bypass)
      if (result_bypass == 0) {
        install_success <- TRUE
      } else {
        # Method 4: Try with older pip build settings
        warning("Trying method 4 (legacy install)...")
        
        cmd_legacy <- make_pip_command(conda_env, python_path, paste("install --use-pep517=False", github_url_zip))
        
        message("Trying method 4 (legacy): ", cmd_legacy)
        result_legacy <- system(cmd_legacy)
        if (result_legacy == 0) {
          install_success <- TRUE
        } else {
          # Method 5: Clone, fix pyproject.toml, and install locally
          warning("All standard methods failed. Trying method 5 (local fix)...")
          
          temp_dir <- file.path(tempdir(), "ConnectomeInfluenceCalculator_fixed")
          if (dir.exists(temp_dir)) {
            unlink(temp_dir, recursive = TRUE)
          }
          
          # Clone repository
          message("Cloning repository to temporary location...")
          clone_result <- system(paste("git clone https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator.git", temp_dir))
          
          if (clone_result == 0) {
            # Fix pyproject.toml license issue
            pyproject_path <- file.path(temp_dir, "pyproject.toml")
            if (file.exists(pyproject_path)) {
              message("Fixing pyproject.toml license configuration...")
              # Read with warn = FALSE to suppress incomplete final line warning
              pyproject_content <- readLines(pyproject_path, warn = FALSE)
              pyproject_content <- gsub('^license = "BSD-3-Clause"', 'license = {text = "BSD-3-Clause"}', pyproject_content)
              
              # Ensure the file ends with a newline to avoid incomplete final line issues
              if (length(pyproject_content) > 0 && !grepl("\n$", pyproject_content[length(pyproject_content)])) {
                pyproject_content[length(pyproject_content)] <- paste0(pyproject_content[length(pyproject_content)], "\n")
              }
              
              # Write with a final newline
              writeLines(pyproject_content, pyproject_path)
              
              # Install from fixed local copy
              cmd_local <- make_pip_command(conda_env, python_path, paste("install", temp_dir))
              
              message("Trying method 5 (local fixed): ", cmd_local)
              result_local <- system(cmd_local)
              if (result_local == 0) {
                install_success <- TRUE
                message("[SUCCESS] Successfully installed using locally fixed pyproject.toml")
              }
            }
            
            # Cleanup
            unlink(temp_dir, recursive = TRUE)
          }
        }
      }
    }
  }
  
  if (!install_success) {
    stop("Failed to install ConnectomeInfluenceCalculator using all available methods. ",
         "This appears to be due to a pyproject.toml configuration issue in the upstream repository. ",
         "Please report this issue at: https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator/issues")
  }
  
  # Test installation
  message("Testing installation...")
  if (!is.null(conda_env)) {
    tryCatch({
      reticulate::use_condaenv(conda_env, required = TRUE)
    }, error = function(e) {
      if (grepl("another version of Python.*has already been initialized", e$message)) {
        warning("Python environment conflict - testing with current environment. ",
                "You may need to restart R to use the specified conda environment.")
      } else {
        warning("Could not activate conda environment: ", e$message, 
                "\nTesting with current environment.")
      }
    })
  }
  
  test_result <- tryCatch({
    ic_module <- reticulate::import("InfluenceCalculator")
    message("[SUCCESS] ConnectomeInfluenceCalculator installed and imported successfully!")
    message("[INFO] If you encountered Python environment conflicts, restart R session for clean environment.")
    TRUE
  }, error = function(e) {
    # Check if this is just an environment issue
    if (grepl("ModuleNotFoundError.*InfluenceCalculator", e$message)) {
      error_msg <- paste(
        "Installation completed but ConnectomeInfluenceCalculator module not found.",
        "\nThis is often due to Python environment conflicts.",
        "\nSolutions:",
        "1. Restart R session completely",
        "2. Run: reticulate::py_config() to check Python environment",
        "3. Manually run: pip install git+https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator.git",
        "4. Try installing in a fresh conda environment",
        "\nOriginal error:", e$message,
        sep = "\n"
      )
    } else {
      error_msg <- paste(
        "Installation completed but import failed:",
        e$message,
        "\nThis may be due to:",
        "1. Python environment conflicts (restart R session)",
        "2. Missing dependencies (check conda environment)",
        "3. Architecture mismatches (try different Python version)",
        "\nTry running: reticulate::py_config() to diagnose Python environment issues.",
        sep = "\n"
      )
    }
    stop(error_msg)
  })
  
  invisible(TRUE)
}

#' Install using pip (less reliable method)
#' @noRd
install_with_pip <- function(conda_env, force_reinstall) {
  warning("Installing with pip may fail due to PETSc/SLEPc compilation issues. ",
          "Consider using method='conda' instead.")
  
  if (!is.null(conda_env)) {
    tryCatch({
      reticulate::use_condaenv(conda_env, required = TRUE)
    }, error = function(e) {
      warning("Could not activate conda environment: ", e$message)
    })
  }
  
  # Try git URL first, then zip as fallback
  github_git_url <- "git+https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator.git"
  github_zip_url <- "https://github.com/DrugowitschLab/ConnectomeInfluenceCalculator/archive/refs/heads/main.zip"
  
  message("Installing ConnectomeInfluenceCalculator with pip (git method)...")
  result <- system(paste("pip install", github_git_url))
  
  if (result != 0) {
    warning("Git installation failed, trying zip archive...")
    result_zip <- system(paste("pip install", github_zip_url))
    if (result_zip != 0) {
      stop("pip installation failed with both git and zip methods. This is often due to PETSc/SLEPc compilation issues. ",
           "Please try method='conda' instead.")
    }
  }
  
  # Test installation
  test_result <- tryCatch({
    ic_module <- reticulate::import("InfluenceCalculator")
    message("[SUCCESS] ConnectomeInfluenceCalculator installed successfully!")
    TRUE
  }, error = function(e) {
    error_msg <- paste(
      "Installation completed but import failed:",
      e$message,
      "\nFor pip installations, this often indicates missing system dependencies.",
      "Consider using method='conda' for more reliable installation.",
      sep = "\n"
    )
    stop(error_msg)
  })
  
  invisible(TRUE)
}

#' Null-coalescing operator
#' @noRd
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

#' Install ConnectomeInfluenceCalculator using Homebrew + UV
#'
#' This function runs the installation script that uses Homebrew to install
#' PETSc and SLEPc core libraries, then UV to create a Python environment
#' and install ConnectomeInfluenceCalculator from GitHub.
#'
#' **RStudio Users**: This method creates environments outside of RStudio's r-miniconda
#' system. For better RStudio integration, consider using `install_python_influence_calculator()`
#' with conda instead, which works within RStudio's environment management system.
#'
#' @param env_name Character. Name of the UV environment to create (default: "influence-py").
#' @param check_dependencies Logical. Whether to check for Homebrew and UV first (default: TRUE).
#' @param force_reinstall Logical. Whether to remove existing environment first (default: FALSE).
#' @param warn_rstudio Logical. Whether to warn about RStudio compatibility (default: TRUE).
#'
#' @return Invisible TRUE if successful, throws error otherwise.
#' @export
#'
#' @details
#' This function runs the bash script located at `inst/install_python_deps.sh`
#' which performs the following steps with automatic error recovery:
#' 
#' 1. Checks for Homebrew and UV prerequisites
#' 2. Installs PETSc and SLEPc using Homebrew
#' 3. Creates a UV virtual environment with Python 3.13.1
#' 4. Installs petsc4py and slepc4py Python wrappers
#' 5. Installs ConnectomeInfluenceCalculator from GitHub (with automatic pyproject.toml fix)
#' 6. Tests the installation
#' 
#' **Automatic Error Recovery:**
#' The script includes automatic fixes for common installation issues:
#' - **pyproject.toml Issues**: Automatically fixes upstream license format validation errors
#' - **Xcode Command Line Tools**: Provides clear guidance and fallback compilation options
#' - **Network Issues**: Multiple download methods and clear error reporting
#' 
#' **Prerequisites:**
#' - Homebrew: `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`
#' - UV: `brew install uv`
#'
#' **RStudio Integration:**
#' UV environments are stored at `~/.local/share/uv/envs/` and may not integrate 
#' seamlessly with RStudio's Python environment detection. You may need to manually
#' specify the environment path using `reticulate::use_virtualenv()` in each session.
#'
#' @examples
#' \dontrun{
#' # Install with default settings (will warn if RStudio detected)
#' install_python_with_uv()
#' 
#' # Install without RStudio warning
#' install_python_with_uv(warn_rstudio = FALSE)
#' 
#' # Install with custom environment name
#' install_python_with_uv(env_name = "my-influence-env")
#' 
#' # Force reinstallation
#' install_python_with_uv(force_reinstall = TRUE)
#' 
#' # For RStudio users, consider conda instead:
#' # install_python_influence_calculator()
#' }
install_python_with_uv <- function(env_name = "influence-py", 
                                   check_dependencies = TRUE,
                                   force_reinstall = FALSE,
                                   warn_rstudio = TRUE) {
  
  # Check for RStudio and warn about integration
  if (warn_rstudio) {
    # Check platform-appropriate r-miniconda path
    if (Sys.info()[["sysname"]] == "Darwin") {
      r_miniconda_path <- file.path(Sys.getenv("HOME"), "Library", "r-miniconda")
    } else {
      r_miniconda_path <- file.path(Sys.getenv("HOME"), ".local", "share", "r-miniconda")
    }
    
    if (dir.exists(r_miniconda_path)) {
      message("[INFO] RStudio r-miniconda detected")
      message("[INFO] UV environments may not integrate seamlessly with RStudio")
      message("[INFO] Consider using install_python_influence_calculator() for better RStudio integration")
      message("[INFO] Or set warn_rstudio=FALSE to suppress this message")
      
      response <- readline(prompt = "Continue with UV installation? (y/N): ")
      if (tolower(response) != "y") {
        message("Installation cancelled. Try install_python_influence_calculator() instead.")
        return(invisible(FALSE))
      }
    }
  }
  
  # Find the script location
  script_path <- system.file("install_python_deps.sh", package = "influencer")
  
  if (script_path == "" || !file.exists(script_path)) {
    stop("Installation script not found. Please reinstall the influencer package.")
  }
  
  # Check prerequisites if requested
  if (check_dependencies) {
    # Check for Homebrew
    brew_available <- system("which brew", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
    if (!brew_available) {
      stop("Homebrew is not installed. Please install it first:\n",
           "/bin/bash -c \"$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)\"")
    }
    
    # Check for UV
    uv_available <- system("which uv", ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
    if (!uv_available) {
      stop("UV is not installed. Please install it first:\n",
           "brew install uv")
    }
  }
  
  # Make script executable (in case it's not)
  system(paste("chmod +x", script_path))
  
  # Prepare environment variables for the script
  env_vars <- paste0("ENV_NAME=", env_name)
  if (force_reinstall) {
    env_vars <- paste(env_vars, "FORCE_REINSTALL=1")
  }
  
  # Run the installation script
  message("Running installation script...")
  message("Script location: ", script_path)
  message("Environment name: ", env_name)
  
  result <- system(paste(env_vars, script_path), wait = TRUE)
  
  if (result != 0) {
    stop("Installation script failed with exit code: ", result)
  }
  
  # Provide usage instructions
  env_path <- file.path(Sys.getenv("HOME"), ".local/share/uv/envs", env_name)
  
  message("\n=== Installation Complete! ===")
  message("To use this environment in R:")
  message("  reticulate::use_virtualenv('", env_path, "')")
  
  # Check for RStudio and provide specific instructions
  if (Sys.info()[["sysname"]] == "Darwin") {
    r_miniconda_path <- file.path(Sys.getenv("HOME"), "Library", "r-miniconda")
  } else {
    r_miniconda_path <- file.path(Sys.getenv("HOME"), ".local", "share", "r-miniconda")
  }
  
  if (dir.exists(r_miniconda_path)) {
    message("\n[RStudio Users] Additional setup needed:")
    message("1. Add this to your .Rprofile or run in each session:")
    message("   reticulate::use_virtualenv('", env_path, "')")
    message("2. Or set RETICULATE_PYTHON environment variable:")
    message("   Sys.setenv(RETICULATE_PYTHON = '", file.path(env_path, "bin/python"), "')")
  }
  
  message("\nTo activate manually in terminal:")
  message("  source ", file.path(env_path, "bin/activate"))
  
  invisible(TRUE)
}