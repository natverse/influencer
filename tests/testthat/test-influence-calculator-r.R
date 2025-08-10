test_that("R implementation loads and initialises correctly with SQLite", {
  db_path <- system.file("tests", "testthat", "toy_network_example.sqlite", 
                         package = "influencer")
  if (!file.exists(db_path)) {
    db_path <- "toy_network_example.sqlite"
  }
  
  skip_if_not(file.exists(db_path), "Test database not found")
  
  # Test basic initialisation with SQLite
  ic.sqlite <- influence_calculator_r(filename = db_path, sqlite = TRUE)
  expect_s3_class(ic.sqlite, "InfluenceCalculatorR")
  expect_true(ic.sqlite$n_neurons > 0)
  expect_false(is.null(ic.sqlite$meta))
  expect_false(is.null(ic.sqlite$W))
})

test_that("R implementation works with data frames", {
  # Create simple test data
  edgelist_simple <- data.frame(
    pre = c(1, 2, 3),
    post = c(2, 3, 1), 
    count = c(10, 8, 5),
    norm = c(1, 1, 1)
  )
  
  meta <- data.frame(
    root_id = c(1, 2, 3),
    cell_type = c("A", "B", "C")
  )
  
  # Test data frame initialisation
  ic.df <- influence_calculator_r(edgelist_simple = edgelist_simple, meta = meta)
  expect_s3_class(ic.df, "InfluenceCalculatorR")
  expect_equal(ic.df$n_neurons, 3)
  expect_false(is.null(ic.df$meta))
  expect_false(is.null(ic.df$W))
  
  # Test calculation
  result <- ic.df$calculate_influence(seed_ids = 1)
  expect_s3_class(result, "data.frame")
  expect_true("id" %in% names(result))
  expect_true("is_seed" %in% names(result))
  expect_true("adjusted_influence" %in% names(result))
  expect_equal(nrow(result), 3)
  
  # Test that id column is character type (not numeric)
  expect_type(result$id, "character")
  expect_false(is.numeric(result$id))
  
  # Test adjusted influence calculation
  influence_col <- grep("Influence_score", names(result), value = TRUE)[1]
  expected_adjusted <- log(result[[influence_col]]) + 24
  expected_adjusted[expected_adjusted < 0] <- 0
  expect_equal(result$adjusted_influence, expected_adjusted)
})

test_that("R implementation calculates influence scores with SQLite", {
  db_path <- system.file("tests", "testthat", "toy_network_example.sqlite", 
                         package = "influencer")
  if (!file.exists(db_path)) {
    db_path <- "toy_network_example.sqlite"
  }
  
  skip_if_not(file.exists(db_path), "Test database not found")
  
  ic <- influence_calculator_r(filename = db_path, sqlite = TRUE)
  
  # Get some seed categories from metadata
  seed_categories <- unique(ic$meta$seed_01)
  seed_categories <- seed_categories[!is.na(seed_categories) & seed_categories != ""]
  
  skip_if(length(seed_categories) == 0, "No seed categories found in test data")
  
  # Test with first seed category
  seed_category <- seed_categories[1]
  seed_ids <- ic$meta$root_id[ic$meta$seed_01 == seed_category]
  
  # Get sensory neurons to silence
  sensory_neurons <- ic$meta$root_id[ic$meta$super_class %in% c("sensory", "ascending_sensory")]
  
  # Calculate influence
  result <- ic$calculate_influence(seed_ids = seed_ids, 
                                   silenced_neurons = sensory_neurons)
  
  # Check result structure
  expect_s3_class(result, "data.frame")
  expect_true("id" %in% names(result))
  expect_true("is_seed" %in% names(result))
  expect_true("matrix_index" %in% names(result))
  expect_true("adjusted_influence" %in% names(result))
  expect_true(any(grepl("Influence_score", names(result))))
  
  # Check that we have results for all neurons
  expect_equal(nrow(result), ic$n_neurons)
  
  # Check that seed neurons are marked correctly
  seed_mask <- result$id %in% seed_ids
  expect_true(all(result$is_seed[seed_mask]))
  expect_false(any(result$is_seed[!seed_mask]))
  
  # Test adjusted influence calculation
  influence_col <- grep("Influence_score", names(result), value = TRUE)[1]
  expected_adjusted <- log(result[[influence_col]]) + 24
  expected_adjusted[expected_adjusted < 0] <- 0
  expect_equal(result$adjusted_influence, expected_adjusted)
})

test_that("R implementation handles signed connectivity", {
  db_path <- system.file("tests", "testthat", "toy_network_example.sqlite", 
                         package = "influencer")
  if (!file.exists(db_path)) {
    db_path <- "toy_network_example.sqlite"
  }
  
  skip_if_not(file.exists(db_path), "Test database not found")
  
  # Test signed version
  ic_signed <- influence_calculator_r(filename = db_path, sqlite = TRUE, signed = TRUE)
  expect_true(ic_signed$W_signed)
  
  # Test unsigned version
  ic_unsigned <- influence_calculator_r(filename = db_path, sqlite = TRUE, signed = FALSE)
  expect_false(ic_unsigned$W_signed)
})

test_that("R implementation handles different thresholds", {
  db_path <- system.file("tests", "testthat", "toy_network_example.sqlite", 
                         package = "influencer")
  if (!file.exists(db_path)) {
    db_path <- "toy_network_example.sqlite"
  }
  
  skip_if_not(file.exists(db_path), "Test database not found")
  
  # Test with different thresholds
  ic1 <- influence_calculator_r(filename = db_path, sqlite = TRUE, count_thresh = 1)
  ic3 <- influence_calculator_r(filename = db_path, sqlite = TRUE, count_thresh = 3)
  ic10 <- influence_calculator_r(filename = db_path, sqlite = TRUE, count_thresh = 10)
  
  # Higher thresholds should generally result in fewer connections
  expect_gte(Matrix::nnzero(ic1$W), Matrix::nnzero(ic3$W))
  expect_gte(Matrix::nnzero(ic3$W), Matrix::nnzero(ic10$W))
})

test_that("R implementation handles large integer IDs correctly", {
  # Create test data with large integer IDs that would be shown in scientific notation
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
  
  # Test data frame initialisation
  ic.df <- influence_calculator_r(edgelist_simple = edgelist_simple, meta = meta)
  expect_s3_class(ic.df, "InfluenceCalculatorR")
  expect_equal(ic.df$n_neurons, 3)
  
  # Test calculation with large ID as seed
  result <- ic.df$calculate_influence(seed_ids = large_ids[1])
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
  
  # Test that id column is character type and preserves large IDs correctly
  expect_type(result$id, "character")
  expect_false(is.numeric(result$id))
  expect_true(all(result$id %in% large_ids))
  expect_false(any(grepl("e\\+", result$id)))  # No scientific notation
  
  # Verify the exact large ID is preserved
  expect_true(large_ids[1] %in% result$id)
  expect_true(large_ids[2] %in% result$id)
  expect_true(large_ids[3] %in% result$id)
})