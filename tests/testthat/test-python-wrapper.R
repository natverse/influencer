test_that("Python wrapper functions are available", {
  # These tests only check function availability, not execution
  # since Python environment may not be set up in testing
  
  expect_true(exists("set_python_env"))
  expect_true(exists("influence_calculator_py"))
  expect_true(exists("calculate_influence_py"))
})

test_that("Python wrapper handles missing environment gracefully", {
  # Test that functions give informative errors when Python isn't available
  expect_error(
    influence_calculator_py("nonexistent.sqlite"),
    "ConnectomeInfluenceCalculator not found|Architecture mismatch detected|Failed to import ConnectomeInfluenceCalculator|Failed to create InfluenceCalculator|no such table"
  )
})

# Skip Python tests if environment is not available
skip_if_no_python <- function() {
  python_available <- tryCatch({
    # Use r-reticulate environment
    reticulate::use_condaenv("r-reticulate", required = TRUE)
    ic_module <- reticulate::import("InfluenceCalculator")
    TRUE
  }, error = function(e) {
    # Check if it's an architecture mismatch
    if (grepl("incompatible architecture", e$message) || 
        grepl("mach-o file.*have 'arm64'.*need 'x86_64'", e$message)) {
      # This is expected on Apple Silicon with x86_64 R
      FALSE
    } else {
      FALSE
    }
  })
  
  skip_if_not(python_available, "Python InfluenceCalculator not available (may be due to architecture mismatch)")
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