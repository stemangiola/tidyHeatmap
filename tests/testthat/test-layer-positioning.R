# Unit test for layer_symbol and layer_text positioning fix
# Tests that symbols/text are positioned correctly in the matrix
# Addresses GitHub issue #162: layer_asterisk draws asterisks incorrectly

library(testthat)
library(tidyHeatmap)
library(dplyr)
library(forcats)
library(vdiffr)

test_that("layer_asterisk positions match matrix structure - using issue #162 dummy_df", {
  # Use the exact dummy_df from GitHub issue #162
  dummy_df <- tibble(
    row_id = c(
      "B cells", "Basophils", "Eosinophils", "Myeloid", "NK cells",  "Neutrophils", "T cells",
      "B cells", "Basophils", "Eosinophils", "Myeloid", "NK cells",  "Neutrophils", "T cells"
    ),
    col_id = c(
      rep("Control", 7),
      rep("Test", 7)
    ),
    value = c(
      0.2000, -0.0180,  0.0983,  0.1930,  0.0934,  0.0529, -0.2090,
      0.0831,  0.00515, 0.0413,  0.0808,  0.0123,  0.0619, -0.1550
    ),
    adj_p = c(
      5.81e-10, 2.72e-01, 4.46e-04, 1.44e-06, 6.43e-08, 1.08e-01, 7.65e-07,
      6.46e-02, 7.81e-01, 2.47e-01, 6.46e-02, 5.90e-01, 6.46e-02, 6.28e-03
    )
  ) %>% 
    mutate(col_id = fct_relevel(col_id, c("Control", "Test")))
  
  # Create heatmap (same as in the issue)
  hm <- heatmap(dummy_df, .row = row_id, .column = col_id, .value = value)
  
  # Get the matrix to see actual ordering
  mat <- hm@input[[1]]
  mat_rownames <- rownames(mat)
  mat_colnames <- colnames(mat)
  
  # Add asterisk with the same condition as in the issue
  hm_with_asterisk <- hm |> layer_asterisk(adj_p < 0.1)
  
  # Check the layer_symbol slot
  symbol_data <- hm_with_asterisk@layer_symbol
  
  # Verify positions are within matrix bounds
  expect_true(all(symbol_data$row >= 1 & symbol_data$row <= nrow(mat)))
  expect_true(all(symbol_data$column >= 1 & symbol_data$column <= ncol(mat)))
  
  # Verify that symbols are placed (we should have some for adj_p < 0.1)
  expect_gt(nrow(symbol_data), 0)
  
  # The key issue: verify that each symbol position corresponds to the correct row/column
  # by matching back to the original data
  # Expected cells with adj_p < 0.1
  expected <- dummy_df %>% filter(adj_p < 0.1)
  
  # Verify that each expected cell has a symbol at the correct position
  for (i in seq_len(nrow(expected))) {
    expected_row_name <- expected$row_id[i]
    expected_col_name <- as.character(expected$col_id[i])
    expected_row_pos <- which(mat_rownames == expected_row_name)
    expected_col_pos <- which(mat_colnames == expected_col_name)
    
    # There should be a symbol at this position
    expect_true(any(symbol_data$row == expected_row_pos & symbol_data$column == expected_col_pos),
                info = paste0("Expected symbol at (", expected_row_name, ", ", expected_col_name, 
                             ") = position (", expected_row_pos, ", ", expected_col_pos, ")"))
  }
  
  # Verify specific cells mentioned in the issue
  # The issue mentioned asterisks were switched between Control and Test for NK cells and Neutrophils
  # Verify NK cells Control (row 5, col 1) - adj_p = 6.43e-08 < 0.1, so should have asterisk
  nk_control_row_pos <- which(mat_rownames == "NK cells")
  nk_control_col_pos <- which(mat_colnames == "Control")
  expect_true(any(symbol_data$row == nk_control_row_pos & symbol_data$column == nk_control_col_pos),
              info = "NK cells Control should have asterisk (adj_p < 0.1)")
  
  # Verify Neutrophils Test (row 6, col 2) - adj_p = 6.46e-02 < 0.1, so should have asterisk
  neutrophils_test_row_pos <- which(mat_rownames == "Neutrophils")
  neutrophils_test_col_pos <- which(mat_colnames == "Test")
  expect_true(any(symbol_data$row == neutrophils_test_row_pos & symbol_data$column == neutrophils_test_col_pos),
              info = "Neutrophils Test should have asterisk (adj_p < 0.1)")
})

test_that("layer_asterisk visual output is correct - using issue #162 dummy_df", {
  # Use the exact dummy_df from GitHub issue #162
  dummy_df <- tibble(
    row_id = c(
      "B cells", "Basophils", "Eosinophils", "Myeloid", "NK cells",  "Neutrophils", "T cells",
      "B cells", "Basophils", "Eosinophils", "Myeloid", "NK cells",  "Neutrophils", "T cells"
    ),
    col_id = c(
      rep("Control", 7),
      rep("Test", 7)
    ),
    value = c(
      0.2000, -0.0180,  0.0983,  0.1930,  0.0934,  0.0529, -0.2090,
      0.0831,  0.00515, 0.0413,  0.0808,  0.0123,  0.0619, -0.1550
    ),
    adj_p = c(
      5.81e-10, 2.72e-01, 4.46e-04, 1.44e-06, 6.43e-08, 1.08e-01, 7.65e-07,
      6.46e-02, 7.81e-01, 2.47e-01, 6.46e-02, 5.90e-01, 6.46e-02, 6.28e-03
    )
  ) %>% 
    mutate(col_id = fct_relevel(col_id, c("Control", "Test")))
  
  # Create heatmap with asterisks (same as in the issue)
  p <- dummy_df |>
    heatmap(.row = row_id, .column = col_id, .value = value) |>
    layer_asterisk(adj_p < 0.1)
  
  # Visual test to ensure asterisks are positioned correctly
  vdiffr::expect_doppelganger("layer_asterisk_issue_162", p)
})

test_that("layer_symbol positions work correctly with randomly generated row/column names", {
  # Create a dataset with hash-like row and column names
  # This ensures the fix works generally, not just with specific name patterns
  # Note: Sequential names like R1, R2, R3 worked even before the fix
  set.seed(42)  # For reproducibility
  random_row_names <- replicate(5, paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
  random_col_names <- replicate(4, paste(sample(c(letters, 0:9), 8, replace = TRUE), collapse = ""))
  
  test_data <- expand.grid(
    row = random_row_names,
    col = random_col_names,
    stringsAsFactors = FALSE
  ) |>
    mutate(
      expression = runif(n()),
      significant = runif(n()) > 0.5
    ) |>
    as_tibble()
  
  # Create heatmap
  hm <- heatmap(test_data, .row = row, .column = col, .value = expression)
  
  # Get the matrix to see actual ordering
  mat <- hm@input[[1]]
  mat_rownames <- rownames(mat)
  mat_colnames <- colnames(mat)
  
  # Add asterisk for significant values
  hm_with_asterisk <- hm |> layer_asterisk(significant == TRUE)
  
  # Check the layer_symbol slot
  symbol_data <- hm_with_asterisk@layer_symbol
  
  # Verify positions are within matrix bounds
  expect_true(all(symbol_data$row >= 1 & symbol_data$row <= nrow(mat)))
  expect_true(all(symbol_data$column >= 1 & symbol_data$column <= ncol(mat)))
  
  # Get expected cells (those with significant == TRUE)
  expected <- test_data %>% filter(significant == TRUE)
  
  # Verify that each expected cell has a symbol at the correct position
  for (i in seq_len(nrow(expected))) {
    expected_row_name <- expected$row[i]
    expected_col_name <- expected$col[i]
    expected_row_pos <- which(mat_rownames == expected_row_name)
    expected_col_pos <- which(mat_colnames == expected_col_name)
    
    # There should be a symbol at this position
    expect_true(any(symbol_data$row == expected_row_pos & symbol_data$column == expected_col_pos),
                info = paste0("Expected symbol at (", expected_row_name, ", ", expected_col_name, 
                             ") = position (", expected_row_pos, ", ", expected_col_pos, ")"))
  }
  
  # Verify that cells with significant == FALSE do NOT have symbols
  not_expected <- test_data %>% filter(significant == FALSE)
  for (i in seq_len(nrow(not_expected))) {
    not_expected_row_name <- not_expected$row[i]
    not_expected_col_name <- not_expected$col[i]
    not_expected_row_pos <- which(mat_rownames == not_expected_row_name)
    not_expected_col_pos <- which(mat_colnames == not_expected_col_name)
    
    # There should NOT be a symbol at this position
    expect_false(any(symbol_data$row == not_expected_row_pos & symbol_data$column == not_expected_col_pos),
                 info = paste0("Should NOT have symbol at (", not_expected_row_name, ", ", not_expected_col_name, 
                              ") = position (", not_expected_row_pos, ", ", not_expected_col_pos, ")"))
  }
  
  # Verify the total number of symbols matches expected
  expect_equal(nrow(symbol_data), nrow(expected))
})
