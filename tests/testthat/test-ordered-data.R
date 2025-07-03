# Unit tests for ordered data extraction functions
# Tests for get_ordered_data(), get_heatmap_order(), and get_ordered_matrix()

library(testthat)
library(tidyHeatmap)
library(dplyr)

# Helper function to create test heatmap
create_test_heatmap <- function() {
  tidyHeatmap::heatmap(
    dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
    .column = UBR, 
    .row = symbol_ct, 
    .value = `read count normalised log`,
    scale = "row"
  )
}

# Helper function to create grouped test heatmap
create_grouped_test_heatmap <- function() {
  tidyHeatmap::heatmap(
    dplyr::group_by(
      dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
      `Cell type`
    ),
    .column = UBR, 
    .row = symbol_ct, 
    .value = `read count normalised log`,
    scale = "row"
  )
}

# Tests for get_ordered_data()
test_that("get_ordered_data returns correct structure", {
  hm <- create_test_heatmap()
  result <- hm |> get_ordered_data()
  
  # Check that result is a list with correct elements
  expect_type(result, "list")
  expect_true(all(c("ordered_data", "row_order", "column_order", "abundance_matrix") %in% names(result)))
  expect_length(result, 4)
})

test_that("get_ordered_data ordered_data is correct type and structure", {
  hm <- create_test_heatmap()
  result <- hm |> get_ordered_data()
  
  # Check that ordered_data is a tibble
  expect_s3_class(result$ordered_data, "tbl_df")
  
  # Check that ordered_data has the expected columns
  original_cols <- colnames(hm@data)
  expect_true(all(original_cols %in% colnames(result$ordered_data)))
  
  # Check that data is properly ordered (factors with correct levels)
  row_col <- hm@arguments$.vertical
  col_col <- hm@arguments$.horizontal
  
  expect_s3_class(result$ordered_data[[rlang::quo_name(row_col)]], "factor")
  expect_s3_class(result$ordered_data[[rlang::quo_name(col_col)]], "factor")
  
  # Check that factor levels match the ordering
  expect_equal(levels(result$ordered_data[[rlang::quo_name(row_col)]]), result$row_order)
  expect_equal(levels(result$ordered_data[[rlang::quo_name(col_col)]]), result$column_order)
})

test_that("get_ordered_data ordering vectors are correct", {
  hm <- create_test_heatmap()
  result <- hm |> get_ordered_data()
  
  # Check that row_order and column_order are character vectors
  expect_type(result$row_order, "character")
  expect_type(result$column_order, "character")
  
  # Check that we have some ordering (not empty)
  expect_gt(length(result$row_order), 0)
  expect_gt(length(result$column_order), 0)
  
  # Check that ordering contains valid names
  original_mat <- hm@input[[1]]
  expect_true(all(result$row_order %in% rownames(original_mat)))
  expect_true(all(result$column_order %in% colnames(original_mat)))
})

test_that("get_ordered_data abundance matrix is correct", {
  hm <- create_test_heatmap()
  result <- hm |> get_ordered_data()
  
  # Check that abundance_matrix is a matrix
  expect_true(is.matrix(result$abundance_matrix))
  
  # Check that the matrix dimensions match the ordering vectors
  expect_equal(nrow(result$abundance_matrix), length(result$row_order))
  expect_equal(ncol(result$abundance_matrix), length(result$column_order))
  
  # Check that matrix row and column names match the ordering
  expect_equal(rownames(result$abundance_matrix), result$row_order)
  expect_equal(colnames(result$abundance_matrix), result$column_order)
  
  # Check that matrix contains numeric data
  expect_true(is.numeric(result$abundance_matrix))
})

# Tests for get_heatmap_order()
test_that("get_heatmap_order returns correct structure", {
  hm <- create_test_heatmap()
  order_info <- hm |> get_heatmap_order()
  
  # Check that result is a list with correct elements
  expect_type(order_info, "list")
  expect_true(all(c("rows", "columns") %in% names(order_info)))
  expect_length(order_info, 2)
})

test_that("get_heatmap_order returns correct data types", {
  hm <- create_test_heatmap()
  order_info <- hm |> get_heatmap_order()
  
  # Check that rows and columns are character vectors
  expect_type(order_info$rows, "character")
  expect_type(order_info$columns, "character")
  
  # Check that we have some ordering (not empty)
  expect_gt(length(order_info$rows), 0)
  expect_gt(length(order_info$columns), 0)
})

test_that("get_heatmap_order contains valid names", {
  hm <- create_test_heatmap()
  order_info <- hm |> get_heatmap_order()
  
  # Check that ordering contains valid names from original matrix
  original_mat <- hm@input[[1]]
  expect_true(all(order_info$rows %in% rownames(original_mat)))
  expect_true(all(order_info$columns %in% colnames(original_mat)))
  
  # Check that all original names are present (no missing data)
  expect_setequal(order_info$rows, rownames(original_mat))
  expect_setequal(order_info$columns, colnames(original_mat))
})

# Tests for get_ordered_matrix()
test_that("get_ordered_matrix returns correct structure", {
  hm <- create_test_heatmap()
  ordered_matrix <- hm |> get_ordered_matrix()
  
  # Check that result is a matrix
  expect_true(is.matrix(ordered_matrix))
  
  # Check that matrix has row and column names
  expect_true(!is.null(rownames(ordered_matrix)))
  expect_true(!is.null(colnames(ordered_matrix)))
  
  # Check that we have some data (not empty)
  expect_gt(nrow(ordered_matrix), 0)
  expect_gt(ncol(ordered_matrix), 0)
})

test_that("get_ordered_matrix contains correct data", {
  hm <- create_test_heatmap()
  ordered_matrix <- hm |> get_ordered_matrix()
  original_mat <- hm@input[[1]]
  
  # Check that matrix dimensions match original
  expect_equal(nrow(ordered_matrix), nrow(original_mat))
  expect_equal(ncol(ordered_matrix), ncol(original_mat))
  
  # Check that matrix contains the same data (just reordered)
  expect_equal(sort(as.vector(ordered_matrix)), sort(as.vector(original_mat)))
  
  # Check that row and column names are valid
  expect_true(all(rownames(ordered_matrix) %in% rownames(original_mat)))
  expect_true(all(colnames(ordered_matrix) %in% colnames(original_mat)))
})

# Cross-function consistency tests
test_that("all three functions return consistent results", {
  hm <- create_test_heatmap()
  
  # Get results from all three functions
  full_result <- hm |> get_ordered_data()
  order_info <- hm |> get_heatmap_order()
  ordered_matrix <- hm |> get_ordered_matrix()
  
  # Check consistency between functions
  expect_equal(full_result$row_order, order_info$rows)
  expect_equal(full_result$column_order, order_info$columns)
  expect_equal(full_result$abundance_matrix, ordered_matrix)
  
  # Check that matrix ordering matches order vectors
  expect_equal(rownames(ordered_matrix), order_info$rows)
  expect_equal(colnames(ordered_matrix), order_info$columns)
  expect_equal(rownames(ordered_matrix), full_result$row_order)
  expect_equal(colnames(ordered_matrix), full_result$column_order)
})

# Tests with grouped heatmaps
test_that("functions work with grouped heatmaps", {
  hm <- create_grouped_test_heatmap()
  
  # Test that functions work with grouped heatmaps
  result <- hm |> get_ordered_data()
  order_info <- hm |> get_heatmap_order()
  ordered_matrix <- hm |> get_ordered_matrix()
  
  # Basic structure checks
  expect_type(result, "list")
  expect_type(order_info, "list")
  expect_true(is.matrix(ordered_matrix))
  
  # Check consistency between functions
  expect_equal(result$row_order, order_info$rows)
  expect_equal(result$column_order, order_info$columns)
  expect_equal(result$abundance_matrix, ordered_matrix)
})

test_that("functions work with different scaling options", {
  # Test with different scale options
  scales_to_test <- c("none", "row", "column", "both")
  
  for (scale_option in scales_to_test) {
    hm <- tidyHeatmap::heatmap(
      dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
      .column = UBR, 
      .row = symbol_ct, 
      .value = `read count normalised log`,
      scale = scale_option
    )
    
    # Test that all functions work with different scaling
    expect_no_error(result <- hm |> get_ordered_data())
    expect_no_error(order_info <- hm |> get_heatmap_order())
    expect_no_error(ordered_matrix <- hm |> get_ordered_matrix())
    
    # Check basic consistency
    expect_equal(result$row_order, order_info$rows)
    expect_equal(result$column_order, order_info$columns)
  }
})

# Tests with annotations
test_that("functions work with annotated heatmaps", {
  hm <- create_test_heatmap() |>
    annotation_tile(CAPRA_TOTAL)
  
  # Test that functions work with annotations
  result <- hm |> get_ordered_data()
  order_info <- hm |> get_heatmap_order()
  ordered_matrix <- hm |> get_ordered_matrix()
  
  # Basic checks
  expect_type(result, "list")
  expect_type(order_info, "list")
  expect_true(is.matrix(ordered_matrix))
  
  # Check consistency
  expect_equal(result$row_order, order_info$rows)
  expect_equal(result$column_order, order_info$columns)
})

# Edge case tests
test_that("functions handle single row/column cases", {
  # Create a heatmap with minimal data
  minimal_data <- tidyHeatmap::N52 |>
    dplyr::filter(Category == "Angiogenesis") |>
    dplyr::slice(1:2) |>  # Only 2 rows
    dplyr::select(symbol_ct, UBR, `read count normalised log`) |>
    dplyr::filter(UBR %in% unique(UBR)[1:2])  # Only 2 columns
  
  hm <- tidyHeatmap::heatmap(
    minimal_data,
    .column = UBR, 
    .row = symbol_ct, 
    .value = `read count normalised log`,
    scale = "none"
  )
  
  # Test that functions work with minimal data
  expect_no_error(result <- hm |> get_ordered_data())
  expect_no_error(order_info <- hm |> get_heatmap_order())
  expect_no_error(ordered_matrix <- hm |> get_ordered_matrix())
  
  # Check that we get some results
  expect_gt(length(order_info$rows), 0)
  expect_gt(length(order_info$columns), 0)
})

test_that("functions return reproducible results", {
  # Test that functions return the same results when called multiple times
  hm <- create_test_heatmap()
  
  # Call functions multiple times
  result1 <- hm |> get_ordered_data()
  result2 <- hm |> get_ordered_data()
  
  order1 <- hm |> get_heatmap_order()
  order2 <- hm |> get_heatmap_order()
  
  matrix1 <- hm |> get_ordered_matrix()
  matrix2 <- hm |> get_ordered_matrix()
  
  # Check that results are identical
  expect_equal(result1$row_order, result2$row_order)
  expect_equal(result1$column_order, result2$column_order)
  expect_equal(order1$rows, order2$rows)
  expect_equal(order1$columns, order2$columns)
  expect_equal(matrix1, matrix2)
})

# Performance/memory tests
test_that("functions don't modify original heatmap object", {
  hm_original <- create_test_heatmap()
  hm_copy <- hm_original
  
  # Call functions
  result <- hm_copy |> get_ordered_data()
  order_info <- hm_copy |> get_heatmap_order()
  ordered_matrix <- hm_copy |> get_ordered_matrix()
  
  # Check that original object is unchanged
  expect_equal(hm_original@data, hm_copy@data)
  expect_equal(hm_original@input, hm_copy@input)
  expect_equal(hm_original@arguments, hm_copy@arguments)
})