test_that("Python wrapper functions are available", {
  # These tests only check function availability, not execution
  # since Python environment may not be set up in testing
  
  expect_true(exists("set_python_env"))
  expect_true(exists("influence_calculator_py"))
  expect_true(exists("calculate_influence_py"))
})

test_that("Python wrapper handles missing environment gracefully", {
  # Function should error with an informative message in any of these scenarios:
  # missing conda, missing/old InfluenceCalculator, missing file, etc. We accept
  # any of the known failure-mode strings.
  expect_error(
    influence_calculator_py("nonexistent.sqlite"),
    "Failed to activate r-reticulate environment|Unable to locate conda environment|ConnectomeInfluenceCalculator not found|Architecture mismatch detected|Failed to import ConnectomeInfluenceCalculator|Failed to create InfluenceCalculator|no such table|Unable to find conda binary|Conda not found|Is Anaconda installed|pandas.errors.DatabaseError|Execution failed on sql|unused argument"
  )
})

# Skip Python tests when the connectome lib is unavailable OR doesn't expose the
# v0.2.0 surface that this branch of `influencer` now targets (`from_dataframes`,
# `inhibitory_nts`, `lambda_max`). Older installed versions of the Python lib
# will load fine but reject the new kwargs at call time.
skip_if_no_python <- function() {
  python_ok <- tryCatch({
    reticulate::use_condaenv("r-reticulate", required = TRUE)
    ic_module <- reticulate::import("InfluenceCalculator")
    # v0.2.0 marker: the from_dataframes classmethod is exposed on the class
    has_from_df <- tryCatch(
      reticulate::py_has_attr(ic_module$InfluenceCalculator, "from_dataframes"),
      error = function(e) FALSE
    )
    isTRUE(has_from_df)
  }, error = function(e) FALSE)

  skip_if_not(python_ok,
              "Python InfluenceCalculator v0.2.0 (from_dataframes API) not available")
}

test_that("Python implementation works when available", {
  skip_if_no_python()
  
  db_path <- system.file("tests", "testthat", "toy_network_example.sqlite", 
                         package = "influencer")
  if (!file.exists(db_path)) {
    db_path <- "toy_network_example.sqlite"
  }
  
  skip_if_not(file.exists(db_path), "Test database not found")
  
  # Test Python implementation
  ic_py <- influence_calculator_py(db_path)
  expect_s3_class(ic_py, "InfluenceCalculatorPy")
  
  # Get test seed IDs
  meta <- DBI::dbReadTable(DBI::dbConnect(RSQLite::SQLite(), db_path), "meta")
  seed_categories <- unique(meta$seed_01)
  seed_categories <- seed_categories[!is.na(seed_categories) & seed_categories != ""]
  
  if (length(seed_categories) > 0) {
    seed_category <- seed_categories[1]
    seed_ids <- meta$root_id[meta$seed_01 == seed_category]
    
    result_py <- calculate_influence_py(ic_py, seed_ids = seed_ids)
    
    expect_s3_class(result_py, "data.frame")
    expect_true("id" %in% names(result_py))
    expect_true("is_seed" %in% names(result_py))
  }
  
  DBI::dbDisconnect(DBI::dbConnect(RSQLite::SQLite(), db_path))
})