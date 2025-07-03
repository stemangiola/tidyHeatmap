# Solution for tidyHeatmap Issue #134: Retrieve Ordered Data After Producing Heatmap

## Problem Description

[Issue #134](https://github.com/stemangiola/tidyHeatmap/issues/134) requests a way to retrieve the ordered data after generating a heatmap with tidyHeatmap. Currently, users can create beautiful heatmaps but cannot easily access the row and column ordering that results from clustering, which is crucial for downstream analysis.

## Current tidyHeatmap Architecture

After analyzing the codebase, here's how tidyHeatmap works:

1. **InputHeatmap Object**: Created by `heatmap()` function
   - `@data`: stores original tibble data
   - `@input`: stores list of parameters for ComplexHeatmap (including abundance matrix)
   - `@arguments`: stores original function arguments

2. **ComplexHeatmap Conversion**: `as_ComplexHeatmap()` converts InputHeatmap to ComplexHeatmap object

3. **Display**: The heatmap is displayed when the object is printed/shown

## Solution: Helper Functions for Data Extraction

Here are several approaches to extract ordered data:

### Approach 1: Extract from ComplexHeatmap Object

```r
# Function to get ordered data from tidyHeatmap
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
```

### Approach 2: Extract Row/Column Names Only

```r
# Simpler function to just get the ordered names
get_heatmap_order <- function(tidyheatmap_obj) {
  ch <- tidyheatmap_obj |> as_ComplexHeatmap()
  ch_drawn <- ComplexHeatmap::draw(ch)
  
  abundance_mat <- tidyheatmap_obj@input[[1]]
  
  list(
    rows = rownames(abundance_mat)[ComplexHeatmap::row_order(ch_drawn)],
    columns = colnames(abundance_mat)[ComplexHeatmap::column_order(ch_drawn)]
  )
}
```

### Approach 3: Get Ordered Matrix Directly

```r
# Function to get the abundance matrix in heatmap order
get_ordered_matrix <- function(tidyheatmap_obj) {
  ch <- tidyheatmap_obj |> as_ComplexHeatmap()
  ch_drawn <- ComplexHeatmap::draw(ch)
  
  abundance_mat <- tidyheatmap_obj@input[[1]]
  row_ord <- ComplexHeatmap::row_order(ch_drawn)
  col_ord <- ComplexHeatmap::column_order(ch_drawn)
  
  abundance_mat[row_ord, col_ord]
}
```

## Example Usage

```r
library(tidyHeatmap)
library(ComplexHeatmap)

# Create a heatmap
hm <- tidyHeatmap::N52 |>
  tidyHeatmap::heatmap(
    .row = symbol_ct,
    .column = UBR,
    .value = `read count normalised log`
  )

# Display the heatmap (important - this actually draws it)
print(hm)

# Get ordered data
ordered_result <- get_ordered_data(hm)

# Access the ordered data
ordered_tibble <- ordered_result$ordered_data
row_order <- ordered_result$row_order  
column_order <- ordered_result$column_order
ordered_matrix <- ordered_result$abundance_matrix

# Or get just the order
order_info <- get_heatmap_order(hm)
print(order_info$rows)     # Row names in heatmap order
print(order_info$columns)  # Column names in heatmap order
```

## Implementation Recommendations

1. **Add to tidyHeatmap Package**: These functions could be added as methods to the InputHeatmap class:
   - `get_order()` method
   - `get_ordered_data()` method
   - `get_ordered_matrix()` method

2. **Documentation**: Include examples in the package documentation showing how to extract ordered data

3. **Integration**: Could add an option to `heatmap()` function to automatically return ordered data alongside the plot

## Limitations and Considerations

1. **Drawing Required**: The heatmap must be drawn (displayed) before the ordering can be extracted
2. **Complex Clustering**: With grouped heatmaps or split dendrograms, the ordering becomes more complex
3. **Annotations**: The solution focuses on the main data matrix; annotations would need separate handling

## ComplexHeatmap Reference

This solution leverages ComplexHeatmap's built-in functions:
- `row_order()`: extracts row ordering after drawing
- `column_order()`: extracts column ordering after drawing
- `draw()`: actually performs the layout and clustering

For more details, see the [ComplexHeatmap documentation on retrieving orders and dendrograms](https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#get-orders-and-dendrograms-from-heatmap).

## Conclusion

While tidyHeatmap doesn't currently provide built-in methods for extracting ordered data, the solution above provides a robust way to retrieve this information using ComplexHeatmap's underlying functionality. This addresses the core need expressed in issue #134 and maintains compatibility with tidyHeatmap's design philosophy.