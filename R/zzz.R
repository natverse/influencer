.onLoad <- function(libname, pkgname) {
  invisible()
}

.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    'Use `influence_calculator_py()` for Python-based calculations
',
    'Use `influence_calculator_r()` for native R calculations
',
    'Use `set_python_env()` to configure Python environment if needed')
}