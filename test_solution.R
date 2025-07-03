# Test Solution for tidyHeatmap Issue #134
# Demonstrating how to extract ordered data after creating a heatmap

# Load required libraries
library(tidyHeatmap)
library(ComplexHeatmap)
library(dplyr)

# Helper functions to extract ordered data from tidyHeatmap

#' Get ordered data from tidyHeatmap object
#' @param tidyheatmap_obj An InputHeatmap object from tidyHeatmap::heatmap()
#' @return List containing ordered data, row order, column order, and abundance matrix
get_ordered_data <- function(tidyheatmap_obj) {
  # Convert to ComplexHeatmap and draw it
  ch <- tidyheatmap_obj |> as_ComplexHeatmap()
  ch_drawn <- ComplexHeatmap::draw(ch)
  
  # Get row and column orders
  row_ord <- ComplexHeatmap::row_order(ch_drawn)
  col_ord <- ComplexHeatmap::column_order(ch_drawn)
  
  # Get the abundance matrix from the original object
  abundance_mat <- tidyheatmap_obj@input[[1]]
  
  # Extract row and column names in the heatmap order
  ordered_row_names <- rownames(abundance_mat)[row_ord]
  ordered_col_names <- colnames(abundance_mat)[col_ord]
  
  # Get the original data
  original_data <- tidyheatmap_obj@data
  
  # Get column names for merging
  row_col <- tidyheatmap_obj@arguments$.vertical
  col_col <- tidyheatmap_obj@arguments$.horizontal
  val_col <- tidyheatmap_obj@arguments$.abundance
  
  # Create ordered data
  ordered_data <- original_data |>
    # Filter to only include data that appears in the heatmap
    filter(
      !!row_col %in% ordered_row_names,
      !!col_col %in% ordered_col_names
    ) |>
    # Convert to factors with levels in heatmap order
    mutate(
      !!row_col := factor(!!row_col, levels = ordered_row_names),
      !!col_col := factor(!!col_col, levels = ordered_col_names)
    ) |>
    # Sort by the factor levels
    arrange(!!row_col, !!col_col)
  
  return(list(
    ordered_data = ordered_data,
    row_order = ordered_row_names,
    column_order = ordered_col_names,
    abundance_matrix = abundance_mat[row_ord, col_ord]
  ))
}

#' Get just the ordering information from tidyHeatmap object
#' @param tidyheatmap_obj An InputHeatmap object from tidyHeatmap::heatmap()
#' @return List with rows and columns in heatmap order
get_heatmap_order <- function(tidyheatmap_obj) {
  ch <- tidyheatmap_obj |> as_ComplexHeatmap()
  ch_drawn <- ComplexHeatmap::draw(ch)
  
  abundance_mat <- tidyheatmap_obj@input[[1]]
  
  list(
    rows = rownames(abundance_mat)[ComplexHeatmap::row_order(ch_drawn)],
    columns = colnames(abundance_mat)[ComplexHeatmap::column_order(ch_drawn)]
  )
}

#' Get the abundance matrix in heatmap order
#' @param tidyheatmap_obj An InputHeatmap object from tidyHeatmap::heatmap()
#' @return Matrix with rows and columns ordered as in the heatmap
get_ordered_matrix <- function(tidyheatmap_obj) {
  ch <- tidyheatmap_obj |> as_ComplexHeatmap()
  ch_drawn <- ComplexHeatmap::draw(ch)
  
  abundance_mat <- tidyheatmap_obj@input[[1]]
  row_ord <- ComplexHeatmap::row_order(ch_drawn)
  col_ord <- ComplexHeatmap::column_order(ch_drawn)
  
  abundance_mat[row_ord, col_ord]
}

# Example usage with tidyHeatmap sample data
cat("Creating heatmap with sample data...\n")

# Create a heatmap using the N52 sample data
hm <- tidyHeatmap::N52 |>
  tidyHeatmap::heatmap(
    .row = symbol_ct,
    .column = UBR,
    .value = `read count normalised log`
  )

cat("Heatmap created. Extracting ordered data...\n")

# Method 1: Get full ordered data
cat("\nMethod 1: Getting full ordered data\n")
ordered_result <- get_ordered_data(hm)

cat("Number of rows in ordered data:", nrow(ordered_result$ordered_data), "\n")
cat("Number of columns:", ncol(ordered_result$ordered_data), "\n")
cat("First 5 rows in heatmap order:", paste(head(ordered_result$row_order, 5), collapse = ", "), "\n")
cat("First 5 columns in heatmap order:", paste(head(ordered_result$column_order, 5), collapse = ", "), "\n")

# Method 2: Get just the ordering
cat("\nMethod 2: Getting just the ordering information\n")
order_info <- get_heatmap_order(hm)
cat("Total rows:", length(order_info$rows), "\n")
cat("Total columns:", length(order_info$columns), "\n")

# Method 3: Get ordered matrix
cat("\nMethod 3: Getting ordered abundance matrix\n")
ordered_matrix <- get_ordered_matrix(hm)
cat("Matrix dimensions:", paste(dim(ordered_matrix), collapse = " x "), "\n")
cat("First few values:", paste(ordered_matrix[1:3, 1:3], collapse = ", "), "\n")

# Verify that the ordering is consistent
cat("\nVerification:\n")
cat("Row names match:", identical(order_info$rows, ordered_result$row_order), "\n")
cat("Column names match:", identical(order_info$columns, ordered_result$column_order), "\n")
cat("Matrix row names match:", identical(rownames(ordered_matrix), ordered_result$row_order), "\n")
cat("Matrix column names match:", identical(colnames(ordered_matrix), ordered_result$column_order), "\n")

cat("\nExample completed successfully!\n")
cat("Users can now access the ordered data for downstream analysis.\n")

# Additional example: Working with grouped data
cat("\nExample with grouped data:\n")

# Create a grouped heatmap
hm_grouped <- tidyHeatmap::N52 |>
  dplyr::group_by(`Cell type`) |>
  tidyHeatmap::heatmap(
    .row = symbol_ct,
    .column = UBR,
    .value = `read count normalised log`
  )

# Get ordered data from grouped heatmap
grouped_result <- get_ordered_data(hm_grouped)
cat("Grouped heatmap rows:", length(grouped_result$row_order), "\n")
cat("Grouped heatmap columns:", length(grouped_result$column_order), "\n")

cat("\nAll examples completed!\n")