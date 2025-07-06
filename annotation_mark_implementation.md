# Implementation of `annotation_mark` for tidyHeatmap

## Overview

This document describes the implementation of the `annotation_mark` feature for tidyHeatmap, as requested in [GitHub issue #17](https://github.com/stemangiola/tidyHeatmap/issues/17).

The `annotation_mark` function adds the ability to mark specific rows or columns in a heatmap based on a boolean column and connect them to text labels with lines, providing a powerful way to highlight important features in large heatmaps.

## Key Design Decision

**Boolean Column Approach**: Unlike the original ComplexHeatmap `anno_mark` which requires explicit `at` indices and `labels`, the tidyHeatmap implementation follows the framework's pattern of using data columns. Users provide a boolean column, and the function automatically:

1. Identifies TRUE values in the boolean column
2. Determines whether it's a row or column annotation using existing tidyHeatmap machinery
3. Extracts the corresponding row/column names as labels
4. Calculates the appropriate indices for the `at` parameter

## Implementation Details

### Files Modified

1. **R/utilities.R**
   - Added `@importFrom ComplexHeatmap anno_mark`
   - Added `"mark" = anno_mark` to `type_to_annot_function` mapping
   - Added `"mark"` case to the switch statement in `get_top_left_annotation`
   - Added special handling for `anno_mark` parameters:
     - Processes boolean data to extract TRUE positions
     - Automatically determines row vs column orientation
     - Extracts row/column names as labels
     - Calculates indices for the `at` parameter

2. **R/methods.R**
   - Added `annotation_mark` generic function and method
   - Comprehensive documentation with examples
   - Follows tidyHeatmap pattern: takes `.data` and `.column` parameters
   - Integrates with existing `add_annotation()` workflow

3. **R/functions.R**
   - Updated `add_annotation` documentation to include "mark" type

4. **NAMESPACE**
   - Added `export(annotation_mark)`
   - Added `importFrom(ComplexHeatmap,anno_mark)`

## Usage Pattern

```r
library(dplyr)
library(tidyHeatmap)

# Create a boolean column to mark specific rows
N52_marked <- N52 |>
  group_by(symbol_ct) |>
  summarise(inflection_high = max(inflection) > 5, .groups = "drop") |>
  right_join(N52, by = "symbol_ct")

# Create heatmap
hm <- N52_marked |>
  tidyHeatmap::heatmap(
    .row = symbol_ct,
    .column = UBR,
    .value = `read count normalised log`
  )

# Add mark annotation - automatically detects row annotation
# and marks rows where inflection_high is TRUE
hm |> annotation_mark(inflection_high)
```

## Technical Implementation

### Boolean Data Processing

The key innovation is in the `get_top_left_annotation` function where mark annotations are handled specially:

```r
if(..2 == "mark") {
  # Extract TRUE values from boolean column to get at and labels
  mark_data <- ..1
  
  # Get row/column names for the orientation
  if(..3 == "row") {
    row_names <- .data_ %>% ungroup() %>% distinct(!!.row) %>% arrange(!!.row) %>% pull(!!.row)
    mark_indices <- which(mark_data)
    mark_labels <- row_names[mark_indices]
  } else {
    col_names <- .data_ %>% ungroup() %>% distinct(!!.column) %>% arrange(!!.column) %>% pull(!!.column)
    mark_indices <- which(mark_data)
    mark_labels <- col_names[mark_indices]
  }
  
  call_args <- list(
    at = mark_indices,
    labels = mark_labels,
    which = ..3
  )
}
```

### Automatic Orientation Detection

The function leverages tidyHeatmap's existing `get_x_y_annotation_columns` machinery to automatically determine whether the boolean column corresponds to rows or columns, making the interface consistent with other annotation functions.

## Benefits

1. **Consistent Interface**: Follows tidyHeatmap's pattern of using data columns
2. **Automatic Label Generation**: Uses actual row/column names as labels
3. **Flexible**: Works with any boolean column in the data
4. **Intuitive**: TRUE values get marked, FALSE values don't
5. **Integrated**: Uses existing tidyHeatmap annotation infrastructure

## Example Use Cases

1. **Significant Genes**: Mark genes with p-value < 0.05
2. **High Expression**: Mark samples with expression > threshold
3. **Interesting Features**: Mark based on any boolean condition
4. **Quality Control**: Mark samples/genes that pass QC criteria

## Future Enhancements

- Support for custom label columns (use a different column for labels than the boolean column)
- Support for custom positioning parameters
- Integration with other tidyHeatmap features like grouping

## Verification

The implementation has been verified to:
- Parse correctly (no syntax errors)
- Follow tidyHeatmap patterns
- Integrate with existing annotation system
- Include proper documentation and examples
- Export correctly in NAMESPACE

This implementation provides a clean, intuitive interface for marking specific rows or columns in heatmaps while maintaining consistency with the tidyHeatmap framework.