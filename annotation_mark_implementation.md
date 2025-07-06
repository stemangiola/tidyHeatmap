# Implementation of `annotation_mark` for tidyHeatmap

## Overview

This document describes the implementation of the `annotation_mark` feature for tidyHeatmap, as requested in [GitHub issue #17](https://github.com/stemangiola/tidyHeatmap/issues/17).

The `annotation_mark` function adds the ability to mark specific rows or columns in a heatmap and connect them to text labels with lines, providing a powerful way to highlight important features in large heatmaps.

## Implementation Details

### Files Modified

1. **R/utilities.R**
   - Added `@importFrom ComplexHeatmap anno_mark`
   - Added `"mark" = anno_mark` to `type_to_annot_function` mapping
   - Added `"mark"` case to the switch statement in `get_top_left_annotation`
   - Added special handling for `anno_mark` parameters (doesn't use `x` parameter like other annotations)

2. **R/methods.R**
   - Added `annotation_mark` generic function
   - Added `annotation_mark` method for `InputHeatmap` class
   - Comprehensive documentation with examples

3. **R/functions.R**
   - Updated `add_annotation` documentation to include "mark" in type parameter

4. **NAMESPACE**
   - Added `export(annotation_mark)`
   - Added `importFrom(ComplexHeatmap,anno_mark)`

### Key Design Decisions

#### Special Parameter Handling
Unlike other annotation types that use data from a column in the dataset, `anno_mark` requires specific parameters:
- `at`: Indices indicating which rows/columns to mark
- `labels`: Text labels corresponding to the marked positions

The implementation handles this by:
- Detecting when `type == "mark"` in the annotation processing logic
- Building a different argument list that doesn't include the `x` parameter
- Passing `at` and `labels` directly from the `...` arguments

#### Size Parameter Handling
For `anno_mark`, the size parameter is handled as `width` rather than `height`, following ComplexHeatmap's conventions.

#### Validation
The implementation includes validation to ensure:
- `at` parameter is provided (required)
- `labels` parameter is provided (required)  
- `at` and `labels` have the same length

## Usage Examples

### Basic Usage

```r
library(tidyHeatmap)

# Create a heatmap
hm <- tidyHeatmap::N52 |>
  tidyHeatmap::heatmap(
    .row = symbol_ct,
    .column = UBR,
    .value = `read count normalised log`
  )

# Add mark annotations to specific rows
hm |> annotation_mark(
  symbol_ct, 
  at = c(1, 5, 10), 
  labels = c("Gene A", "Gene B", "Gene C"),
  side = "right"
)
```

### Advanced Usage with Styling

```r
# Add mark annotations with custom styling
hm |> annotation_mark(
  symbol_ct,
  at = c(2, 4, 6, 8),
  labels = c("Important Gene 1", "Important Gene 2", "Important Gene 3", "Important Gene 4"),
  side = "left",
  size = unit(3, "cm"),
  link_gp = gpar(col = "red", lwd = 2),
  labels_gp = gpar(fontsize = 10, col = "blue")
)
```

### Column Annotations

```r
# Mark specific columns
hm |> annotation_mark(
  UBR,
  at = c(1, 3, 5),
  labels = c("Sample A", "Sample B", "Sample C"),
  side = "top"
)
```

## Function Signature

```r
annotation_mark(
  .data,
  .column,
  at = NULL, 
  labels = NULL,
  side = NULL,
  size = NULL, 
  ...
)
```

### Parameters

- `.data`: A `InputHeatmap` object created calling `tidyHeatmap::heatmap()`
- `.column`: Vector of quotes specifying the column to use for determining row/column orientation
- `at`: A vector of indices indicating which rows/columns to mark (required)
- `labels`: A character vector of labels corresponding to the marked positions (required)
- `side`: The side on which to place the labels ("left", "right" for row annotations; "top", "bottom" for column annotations)
- `size`: A grid::unit object for the width (row annotations) or height (column annotations)
- `...`: Additional arguments passed to `ComplexHeatmap::anno_mark()` and `HeatmapAnnotation()`

## Technical Architecture

### Integration with tidyHeatmap Framework

The implementation follows tidyHeatmap's established patterns:

1. **Generic/Method Pattern**: Uses S4 generic and method dispatch like other annotation functions
2. **Pipe-Friendly**: Returns `InputHeatmap` object for chaining with `|>`
3. **Consistent API**: Follows same parameter conventions as other `annotation_*` functions
4. **Type System**: Integrates with the existing annotation type system in `utilities.R`

### ComplexHeatmap Integration

The implementation leverages ComplexHeatmap's `anno_mark` function while providing a tidyHeatmap-style interface:

- Automatically detects row vs column orientation
- Handles size parameters appropriately  
- Passes through all ComplexHeatmap customization options via `...`
- Integrates with tidyHeatmap's annotation pipeline

## Testing

The implementation has been tested for:

1. **Syntax Validation**: All R files parse correctly
2. **Type System Integration**: Mark type properly integrated into annotation system
3. **Parameter Validation**: Required parameters are validated
4. **API Consistency**: Follows established tidyHeatmap patterns

## Future Enhancements

Potential future improvements could include:

1. **Smart Positioning**: Automatic calculation of optimal label positions
2. **Collision Detection**: Automatic adjustment when labels overlap
3. **Data-Driven Marking**: Helper functions to automatically identify rows/columns to mark based on data criteria
4. **Enhanced Styling**: Additional styling options specific to mark annotations

## Compatibility

This implementation:
- Is fully backward compatible with existing tidyHeatmap code
- Follows tidyHeatmap's lifecycle and deprecation policies
- Maintains consistency with existing annotation functions
- Integrates seamlessly with the existing codebase

## References

- [GitHub Issue #17](https://github.com/stemangiola/tidyHeatmap/issues/17)
- [ComplexHeatmap anno_mark documentation](https://jokergoo.github.io/ComplexHeatmap/reference/anno_mark.html)
- [tidyHeatmap paper](https://joss.theoj.org/papers/10.21105/joss.02472)