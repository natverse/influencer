#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbReadTable dbWriteTable
#' @importFrom Matrix sparseMatrix solve diag lu
#' @importFrom R6 R6Class
#' @importFrom RSpectra eigs
#' @importFrom RSQLite SQLite
#' @importFrom dplyr filter
#' @importFrom digest digest
#' @importFrom magrittr %>%
#' @importFrom reticulate import py_config py_to_r use_condaenv use_python
#' @importFrom tibble tibble
## usethis namespace: end
NULL

# Package startup message
.onAttach <- function(libname, pkgname) {
  # Check for potential architecture issues and provide guidance
  compatibility <- tryCatch({
    system_info <- Sys.info()
    if (system_info["sysname"] == "Darwin" && system_info["machine"] == "x86_64") {
      hw_arch <- tryCatch({
        system("uname -m", intern = TRUE)
      }, error = function(e) "unknown")
      
      if (hw_arch == "arm64") {
        packageStartupMessage("Use `influence_calculator_py()` for Python-based calculations")
        packageStartupMessage("Use `influence_calculator_r()` for native R calculations") 
        packageStartupMessage("Use `set_python_env()` to configure Python environment if needed")
        packageStartupMessage("")
        packageStartupMessage("Note: Architecture mismatch detected (x86_64 R on Apple Silicon).")
        packageStartupMessage("The R implementation is recommended and optimized for performance.")
      } else {
        packageStartupMessage("Use `influence_calculator_py()` for Python-based calculations")
        packageStartupMessage("Use `influence_calculator_r()` for native R calculations")
        packageStartupMessage("Use `set_python_env()` to configure Python environment if needed")
      }
    } else {
      packageStartupMessage("Use `influence_calculator_py()` for Python-based calculations")
      packageStartupMessage("Use `influence_calculator_r()` for native R calculations")
      packageStartupMessage("Use `set_python_env()` to configure Python environment if needed")
    }
  }, error = function(e) {
    packageStartupMessage("Use `influence_calculator_py()` for Python-based calculations")
    packageStartupMessage("Use `influence_calculator_r()` for native R calculations")
    packageStartupMessage("Use `set_python_env()` to configure Python environment if needed")
  })
}