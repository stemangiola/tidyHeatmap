# Unit tests for get_heatmap_data function

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

# Tests for get_heatmap_data()
test_that("get_heatmap_data returns correct structure", {
  hm <- create_test_heatmap()
  result <- hm |> get_heatmap_data()
  
  # Check that result is a list with correct elements
  expect_type(result, "list")
  expect_true(all(c("matrix", "row_dend", "column_dend") %in% names(result)))
  expect_length(result, 3)
})

test_that("get_heatmap_data matrix is correct", {
  hm <- create_test_heatmap()
  result <- hm |> get_heatmap_data()
  
  # Check that matrix is a matrix
  expect_true(is.matrix(result$matrix))
  
  # Check that matrix has row and column names
  expect_true(!is.null(rownames(result$matrix)))
  expect_true(!is.null(colnames(result$matrix)))
  
  # Check that matrix contains numeric data
  expect_true(is.numeric(result$matrix))
  
  # Check that we have some data (not empty)
  expect_gt(nrow(result$matrix), 0)
  expect_gt(ncol(result$matrix), 0)
  
  # Check that matrix contains the same data as original (just reordered)
  original_mat <- hm@input[[1]]
  expect_equal(nrow(result$matrix), nrow(original_mat))
  expect_equal(ncol(result$matrix), ncol(original_mat))
  expect_equal(sort(as.vector(result$matrix)), sort(as.vector(original_mat)))
})

test_that("get_heatmap_data dendrograms are correct", {
  hm <- create_test_heatmap()
  result <- hm |> get_heatmap_data()
  
  # Check that row_dend is a dendrogram
  expect_s3_class(result$row_dend, "dendrogram")
  
  # Check that column_dend is a dendrogram
  expect_s3_class(result$column_dend, "dendrogram")
  
  # Check that dendrograms have correct number of leaves
  expect_equal(length(labels(result$row_dend)), nrow(result$matrix))
  expect_equal(length(labels(result$column_dend)), ncol(result$matrix))
})

test_that("get_heatmap_data has consistent naming", {
  hm <- create_test_heatmap()
  result <- hm |> get_heatmap_data()
  
  # Check that dendrogram labels match matrix row/column names
  expect_setequal(labels(result$row_dend), rownames(result$matrix))
  expect_setequal(labels(result$column_dend), colnames(result$matrix))
  
  # Check that the order of dendrogram labels matches matrix order
  expect_equal(labels(result$row_dend)[order.dendrogram(result$row_dend)], rownames(result$matrix))
  expect_equal(labels(result$column_dend)[order.dendrogram(result$column_dend)], colnames(result$matrix))
})

test_that("get_heatmap_data works with different data types", {
  hm <- create_test_heatmap()
  result <- hm |> get_heatmap_data()
  
  # Check that matrix values are numeric
  expect_true(is.numeric(result$matrix))
  
  # Check that dendrograms are proper objects
  
  # Check that all data is finite (no NAs, Infs)
  expect_true(all(is.finite(result$matrix)))
})

# Tests with grouped heatmaps
test_that("get_heatmap_data works with grouped heatmaps", {
  hm <- create_grouped_test_heatmap()
  
  # Test that function works with grouped data
  expect_no_error(result <- hm |> get_heatmap_data())
  
  # Check that result has correct structure
  expect_type(result, "list")
  expect_true(all(c("matrix", "row_dend", "column_dend") %in% names(result)))
  
  # Check that all components are valid
  expect_true(is.matrix(result$matrix))
  expect_s3_class(result$row_dend, "dendrogram")
  expect_s3_class(result$column_dend, "dendrogram")
})

test_that("get_heatmap_data works with different scaling options", {
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
    
    # Test that function works with different scaling
    expect_no_error(result <- hm |> get_heatmap_data())
    
    # Check that result has correct structure
    expect_type(result, "list")
    expect_true(all(c("matrix", "row_dend", "column_dend") %in% names(result)))
    expect_true(is.matrix(result$matrix))
    expect_s3_class(result$row_dend, "dendrogram")
    expect_s3_class(result$column_dend, "dendrogram")
  }
})

# Tests with annotations
test_that("get_heatmap_data works with annotated heatmaps", {
  hm <- create_test_heatmap() |>
    annotation_tile(CAPRA_TOTAL)
  
  # Test that function works with annotations
  expect_no_error(result <- hm |> get_heatmap_data())
  
  # Basic checks
  expect_type(result, "list")
  expect_true(all(c("matrix", "row_dend", "column_dend") %in% names(result)))
  expect_true(is.matrix(result$matrix))
  expect_s3_class(result$row_dend, "dendrogram")
  expect_s3_class(result$column_dend, "dendrogram")
})

# Edge case tests
test_that("get_heatmap_data handles small datasets", {
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
  
  # Test that function works with minimal data
  expect_no_error(result <- hm |> get_heatmap_data())
  
  # Check that we get valid results
  expect_gt(nrow(result$matrix), 0)
  expect_gt(ncol(result$matrix), 0)
  expect_s3_class(result$row_dend, "dendrogram")
  expect_s3_class(result$column_dend, "dendrogram")
})

test_that("get_heatmap_data returns reproducible results", {
  # Test that function returns the same results when called multiple times
  hm <- create_test_heatmap()
  
  # Call function multiple times
  result1 <- hm |> get_heatmap_data()
  result2 <- hm |> get_heatmap_data()
  
  # Check that results are identical
  expect_equal(result1$matrix, result2$matrix)
  expect_equal(result1$row_dend, result2$row_dend)
  expect_equal(result1$column_dend, result2$column_dend)
})

# Performance/memory tests
test_that("get_heatmap_data doesn't modify original heatmap object", {
  hm_original <- create_test_heatmap()
  hm_copy <- hm_original
  
  # Call function
  result <- hm_copy |> get_heatmap_data()
  
  # Check that original object is unchanged
  expect_equal(hm_original@data, hm_copy@data)
  expect_equal(hm_original@input, hm_copy@input)
  expect_equal(hm_original@arguments, hm_copy@arguments)
})