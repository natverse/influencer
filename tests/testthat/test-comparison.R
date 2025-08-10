test_that("R and Python implementations give similar results", {
  # Skip if Python environment is not available
  skip_if_no_python <- function() {
    python_available <- tryCatch({
      # Set up Python environment explicitly
      reticulate::use_python("/opt/miniconda3/bin/python", required = TRUE)
      ic_module <- reticulate::import("InfluenceCalculator")
      TRUE
    }, error = function(e) FALSE)
    
    skip_if_not(python_available, "Python InfluenceCalculator not available (may be due to architecture mismatch)")
  }
  
  skip_if_no_python()
  
  db_path <- system.file("tests", "testthat", "toy_network_example.sqlite", 
                         package = "influencer")
  if (!file.exists(db_path)) {
    db_path <- "toy_network_example.sqlite"
  }
  
  skip_if_not(file.exists(db_path), "Test database not found")
  
  # Create both implementations with same parameters
  ic_r <- influence_calculator_r(filename = db_path, sqlite = TRUE, signed = FALSE, count_thresh = 3)
  ic_py <- influence_calculator_py(db_path, signed = FALSE, count_thresh = 3)
  
  # Get test data
  seed_categories <- unique(ic_r$meta$seed_01)
  seed_categories <- seed_categories[!is.na(seed_categories) & seed_categories != ""]
  
  skip_if(length(seed_categories) == 0, "No seed categories found")
  
  # Test with first seed category
  seed_category <- seed_categories[1]
  seed_ids <- ic_r$meta$root_id[ic_r$meta$seed_01 == seed_category]
  
  # Get sensory neurons to silence
  sensory_neurons <- ic_r$meta$root_id[ic_r$meta$super_class %in% c("sensory", "ascending_sensory")]
  
  # Calculate influence with both implementations
  result_r <- ic_r$calculate_influence(seed_ids = seed_ids, 
                                       silenced_neurons = sensory_neurons)
  result_py <- calculate_influence_py(ic_py, seed_ids = seed_ids,
                                      silenced_neurons = sensory_neurons)
  
  # Check that both results have same structure
  expect_equal(nrow(result_r), nrow(result_py))
  expect_true("id" %in% names(result_r))
  expect_true("id" %in% names(result_py))
  expect_true("adjusted_influence" %in% names(result_r))
  expect_true("adjusted_influence" %in% names(result_py))
  
  # Sort both results by ID for comparison
  result_r <- result_r[order(result_r$id), ]
  result_py <- result_py[order(result_py$id), ]
  
  # Check that IDs match
  expect_equal(result_r$id, result_py$id)
  
  # Check that seed identification matches
  expect_equal(result_r$is_seed, result_py$is_seed)
  
  # Extract influence scores (column names might differ slightly)
  score_col_r <- grep("Influence_score", names(result_r), value = TRUE)[1]
  score_col_py <- grep("Influence_score", names(result_py), value = TRUE)[1]
  
  scores_r <- result_r[[score_col_r]]
  scores_py <- result_py[[score_col_py]]
  
  # Check that influence scores are reasonably similar
  # (allowing for numerical differences between implementations)
  correlation <- cor(scores_r, scores_py)
  expect_gt(correlation, 0.95, 
           info = paste("Correlation between R and Python results:", correlation))
  
  # Check that the relative ordering is similar for top influenced neurons
  top_n <- min(20, length(scores_r))
  top_r <- order(scores_r, decreasing = TRUE)[1:top_n]
  top_py <- order(scores_py, decreasing = TRUE)[1:top_n]
  
  # Check overlap in top neurons
  overlap <- length(intersect(top_r, top_py))
  overlap_fraction <- overlap / top_n
  expect_gt(overlap_fraction, 0.7,
           info = paste("Overlap in top", top_n, "neurons:", overlap_fraction))
})

test_that("Both R and Python implementations return character ID columns", {
  # Skip if Python environment is not available
  skip_if_no_python <- function() {
    python_available <- tryCatch({
      reticulate::use_python("/opt/miniconda3/bin/python", required = TRUE)
      ic_module <- reticulate::import("InfluenceCalculator")
      TRUE
    }, error = function(e) FALSE)
    
    skip_if_not(python_available, "Python InfluenceCalculator not available")
  }
  
  skip_if_no_python()
  
  # Create test data with large integer IDs that could cause precision issues
  large_ids <- c("720575968510201856", "720575968510201857", "720575968510201858")
  
  edgelist_simple <- data.frame(
    pre = large_ids[1:2],
    post = large_ids[2:3], 
    count = c(10, 8),
    norm = c(1, 1),
    stringsAsFactors = FALSE
  )
  
  meta <- data.frame(
    root_id = large_ids,
    cell_type = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )
  
  # Test R implementation
  ic_r <- influence_calculator_r(edgelist_simple = edgelist_simple, meta = meta)
  result_r <- ic_r$calculate_influence(seed_ids = large_ids[1])
  
  # Test that R implementation returns character IDs
  expect_type(result_r$id, "character")
  expect_false(is.numeric(result_r$id))
  expect_true(large_ids[1] %in% result_r$id)
  expect_false(any(grepl("e\\+", result_r$id)))  # No scientific notation
  
  # Test Python implementation (uses temporary SQLite database)
  ic_py <- influence_calculator_py(edgelist_simple = edgelist_simple, meta = meta)
  result_py <- calculate_influence_py(ic_py, seed_ids = large_ids[1])
  
  # Test that Python implementation also returns character IDs
  expect_type(result_py$id, "character") 
  expect_false(is.numeric(result_py$id))
  expect_true(large_ids[1] %in% result_py$id)
  expect_false(any(grepl("e\\+", result_py$id)))  # No scientific notation
  
  # Verify both implementations give the same IDs
  expect_equal(sort(result_r$id), sort(result_py$id))
})