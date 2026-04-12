# Suppress R CMD check NOTEs for data.table NSE variables
utils::globalVariables(c(
  ".", ".N", ":=",
  "adjusted_influence", "adjusted_influence_minmax_by_seed",
  "adjusted_influence_minmax_by_target",
  "adjusted_influence_norm_by_sources_and_targets",
  "id", "influence_original", "influence_summed",
  "is_seed", "no_sources", "no_targets", "seed", "target"
))

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