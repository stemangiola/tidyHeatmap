context("get_x_y_annotation_columns tests")

# Load required packages
library(dplyr)
library(tidyr)
library(tibble)
library(rlang)

# Create test data sets for different scenarios
create_basic_test_data <- function() {
  tibble(
    sample = c("S1", "S2", "S3", "S4"),
    gene = c("G1", "G1", "G2", "G2"),
    count = c(10, 20, 30, 40),
    sample_type = c("A", "B", "A", "B"),  # annotation for samples
    gene_pathway = c("P1", "P1", "P2", "P2"),  # annotation for genes
    batch = c("B1", "B1", "B2", "B2"),  # could be either sample or gene annotation
    irrelevant_col = c("X", "Y", "Z", "W")  # not related to either dimension
  )
}

create_complex_test_data <- function() {
  tibble(
    patient_id = rep(c("P1", "P2", "P3"), each = 4),
    biomarker = rep(c("BM1", "BM2", "BM3", "BM4"), times = 3),
    expression = runif(12),
    age = rep(c(25, 35, 45), each = 4),  # patient annotation
    gender = rep(c("M", "F", "M"), each = 4),  # patient annotation
    pathway = rep(c("Path1", "Path2", "Path1", "Path2"), times = 3),  # biomarker annotation
    category = rep(c("Cat1", "Cat2", "Cat1", "Cat2"), times = 3),  # biomarker annotation
    treatment = rep(c("T1", "T2", "T3"), each = 4),  # patient annotation
    tissue_type = rep(c("Normal", "Tumor"), each = 6)  # could be either
  )
}

# Test basic functionality
test_that("get_x_y_annotation_columns works with basic data", {
  test_data <- create_basic_test_data()
  
  result <- get_x_y_annotation_columns(test_data, sample, gene, count)
  
  # Should be a tibble with orientation and col_name columns
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("orientation", "col_name"))
  
  # Should have entries for both orientations
  expect_true("column" %in% result$orientation)
  expect_true("row" %in% result$orientation)
  
  # Check that sample-related columns are marked as column orientation
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  expect_true("sample" %in% column_cols)
  expect_true("sample_type" %in% column_cols)
  
  # Check that gene-related columns are marked as row orientation
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  expect_true("gene" %in% row_cols)
  expect_true("gene_pathway" %in% row_cols)
  
  # Count should be in both orientations (as it's the abundance measure)
  expect_true("count" %in% column_cols)
  expect_true("count" %in% row_cols)
})

# Test with more complex data structure
test_that("get_x_y_annotation_columns handles complex data correctly", {
  test_data <- create_complex_test_data()
  
  result <- get_x_y_annotation_columns(test_data, patient_id, biomarker, expression)
  
  # Should identify patient-specific annotations
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  expect_true("patient_id" %in% column_cols)
  expect_true("age" %in% column_cols)
  expect_true("gender" %in% column_cols)
  expect_true("treatment" %in% column_cols)
  
  # Should identify biomarker-specific annotations
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  expect_true("biomarker" %in% row_cols)
  expect_true("pathway" %in% row_cols)
  expect_true("category" %in% row_cols)
})

# Test with empty data
test_that("get_x_y_annotation_columns handles empty data", {
  empty_data <- tibble(
    sample = character(0),
    gene = character(0),
    count = numeric(0)
  )
  
  result <- get_x_y_annotation_columns(empty_data, sample, gene, count)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("orientation", "col_name"))
  # With empty data, we should still get the basic structure
  expect_true(nrow(result) >= 0)
})

# Test with single row/column
test_that("get_x_y_annotation_columns handles single row/column data", {
  single_data <- tibble(
    sample = "S1",
    gene = "G1",
    count = 100,
    sample_annotation = "A",
    gene_annotation = "X"
  )
  
  result <- get_x_y_annotation_columns(single_data, sample, gene, count)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("orientation", "col_name"))
  
  # Should still categorize annotations correctly
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  
  expect_true("sample" %in% column_cols)
  expect_true("gene" %in% row_cols)
})

# Test with grouped data
test_that("get_x_y_annotation_columns works with grouped data", {
  test_data <- create_basic_test_data() %>%
    group_by(sample_type)
  
  result <- get_x_y_annotation_columns(test_data, sample, gene, count)
  
  # Should still work correctly and ungroup the data
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("orientation", "col_name"))
  
  # Should still categorize correctly
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  
  expect_true("sample" %in% column_cols)
  expect_true("gene" %in% row_cols)
})

# Test with list columns (should be filtered out)
test_that("get_x_y_annotation_columns filters out list columns", {
  test_data <- create_basic_test_data() %>%
    mutate(list_col = list(c(1, 2), c(3, 4), c(5, 6), c(7, 8)))
  
  result <- get_x_y_annotation_columns(test_data, sample, gene, count)
  
  # list_col should not appear in the result
  all_cols <- result %>% pull(col_name)
  expect_false("list_col" %in% all_cols)
  
  # Other columns should still be present
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  
  expect_true("sample" %in% column_cols)
  expect_true("gene" %in% row_cols)
})

# Test with factor columns
test_that("get_x_y_annotation_columns handles factor columns", {
  test_data <- create_basic_test_data() %>%
    mutate(
      sample_type = factor(sample_type),
      gene_pathway = factor(gene_pathway)
    )
  
  result <- get_x_y_annotation_columns(test_data, sample, gene, count)
  
  # Should handle factors correctly
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  
  expect_true("sample_type" %in% column_cols)
  expect_true("gene_pathway" %in% row_cols)
})

# Test with numeric annotation columns
test_that("get_x_y_annotation_columns handles numeric annotations", {
  test_data <- tibble(
    sample = c("S1", "S2", "S3", "S4"),
    gene = c("G1", "G1", "G2", "G2"),
    count = c(10, 20, 30, 40),
    sample_score = c(1.5, 2.5, 1.5, 2.5),  # numeric annotation for samples
    gene_weight = c(10.1, 10.1, 20.2, 20.2)  # numeric annotation for genes
  )
  
  result <- get_x_y_annotation_columns(test_data, sample, gene, count)
  
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  
  expect_true("sample_score" %in% column_cols)
  expect_true("gene_weight" %in% row_cols)
})

# Test with real-world-like data (using structure similar to N52)
test_that("get_x_y_annotation_columns works with N52-like structure", {
  # Create data similar to N52 structure
  n52_like_data <- tibble(
    symbol_ct = rep(c("G1", "G2", "G3"), each = 4),
    UBR = rep(c("S1", "S2", "S3", "S4"), times = 3),
    `read count normalised log` = runif(12),
    Category = rep(c("Cat1", "Cat2", "Cat1"), each = 4),  # gene annotation
    `Cell type` = rep(c("TypeA", "TypeB", "TypeA", "TypeB"), times = 3),  # sample annotation
    CAPRA_TOTAL = rep(c(1, 2, 3, 4), times = 3),  # sample annotation
    inflection = rep(c(10, 20, 30), each = 4)  # gene annotation
  )
  
  result <- get_x_y_annotation_columns(
    n52_like_data, 
    UBR, 
    symbol_ct, 
    `read count normalised log`
  )
  
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  
  # UBR should be in column orientation
  expect_true("UBR" %in% column_cols)
  # symbol_ct should be in row orientation  
  expect_true("symbol_ct" %in% row_cols)
  
  # Cell type and CAPRA_TOTAL should be sample (column) annotations
  expect_true("Cell type" %in% column_cols)
  expect_true("CAPRA_TOTAL" %in% column_cols)
  
  # Category and inflection should be gene (row) annotations
  expect_true("Category" %in% row_cols)
  expect_true("inflection" %in% row_cols)
})

# Test with missing abundance column specified
test_that("get_x_y_annotation_columns handles abundance column correctly", {
  test_data <- create_basic_test_data()
  
  # The abundance column should appear in both orientations in the result
  result <- get_x_y_annotation_columns(test_data, sample, gene, count)
  
  column_cols <- result %>% filter(orientation == "column") %>% pull(col_name)
  row_cols <- result %>% filter(orientation == "row") %>% pull(col_name)
  
  # count (abundance) should be in both orientations
  expect_true("count" %in% column_cols)
  expect_true("count" %in% row_cols)
})

# Test result structure and types
test_that("get_x_y_annotation_columns returns correct structure", {
  test_data <- create_basic_test_data()
  
  result <- get_x_y_annotation_columns(test_data, sample, gene, count)
  
  # Check return type and structure
  expect_s3_class(result, "tbl_df")
  expect_equal(ncol(result), 2)
  expect_equal(names(result), c("orientation", "col_name"))
  
  # Check column types
  expect_type(result$orientation, "character")
  expect_type(result$col_name, "character")
  
  # Check that orientation only contains expected values
  unique_orientations <- unique(result$orientation)
  expect_true(all(unique_orientations %in% c("column", "row")))
  
  # Check that col_name contains actual column names from the data
  expect_true(all(result$col_name %in% names(test_data)))
})

# Test with duplicate column names handling
test_that("get_x_y_annotation_columns handles edge cases", {
  # Test with minimal data
  minimal_data <- tibble(
    x = c("A", "B"),
    y = c("1", "2"), 
    z = c(100, 200)
  )
  
  result <- get_x_y_annotation_columns(minimal_data, x, y, z)
  
  expect_s3_class(result, "tbl_df")
  expect_equal(names(result), c("orientation", "col_name"))
  
  # All columns should appear in the result
  all_cols <- result %>% pull(col_name)
  expect_true("x" %in% all_cols)
  expect_true("y" %in% all_cols)
  expect_true("z" %in% all_cols)
})