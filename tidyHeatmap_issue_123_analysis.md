# Analysis of tidyHeatmap Issue #123: Adding More Than 2 Heatmaps Side-by-Side Not Working

## Issue Summary

**GitHub Issue**: [#123](https://github.com/stemangiola/tidyHeatmap/issues/123)
**Problem**: Adding more than 2 heatmaps side-by-side using the `+` operator fails with an error.

## Problem Description

When users try to combine more than 2 heatmaps using the `+` operator:

```r
p_heatmap = heatmap(mtcars_tidy, `Car name`, Property, Value, scale = "row") 

# This works with a warning
p_heatmap + p_heatmap
#> Warning: Heatmap/annotation names are duplicated: Value

# This fails with an error
p_heatmap + p_heatmap + p_heatmap
#> Warning: Heatmap/annotation names are duplicated: Value
#> Warning: Incompatible methods ("+.AdditiveUnit", "+.InputHeatmap") for "+"
#> Error in p_heatmap + p_heatmap + p_heatmap : non-numeric argument to binary operator
```

## Root Cause Analysis

### Current Implementation

The current `+.InputHeatmap` method in `R/methods.R` (line 207):

```r
"+.InputHeatmap" <- function(e1, e2) {
  add_heatmap(as_ComplexHeatmap(e1), as_ComplexHeatmap(e2)) 
}
```

### The Problem

When evaluating `p_heatmap + p_heatmap + p_heatmap`, R processes this left-to-right:

1. **First operation**: `(p_heatmap + p_heatmap)` 
   - Calls `+.InputHeatmap` method
   - Converts both `InputHeatmap` objects to `ComplexHeatmap` objects
   - Calls `add_heatmap()` which returns a `HeatmapList` object

2. **Second operation**: `HeatmapList + p_heatmap`
   - Tries to find a method for `HeatmapList + InputHeatmap`
   - No such method exists in tidyHeatmap
   - R falls back to the default `+` operator
   - Default `+` expects numeric arguments → **Error**: "non-numeric argument to binary operator"

### ComplexHeatmap's Expected Behavior

According to ComplexHeatmap documentation, `add_heatmap` can handle:
- `Heatmap + Heatmap` → `HeatmapList`
- `HeatmapList + Heatmap` → `HeatmapList`  
- `HeatmapAnnotation + Heatmap` → `HeatmapList`

The `+` operator in ComplexHeatmap (via `+.AdditiveUnit`) works with these combinations seamlessly.

## Proposed Solution

### Option 1: Define Additional Methods (Recommended)

Add methods to handle combinations where the first argument is not an `InputHeatmap`:

```r
# Method for HeatmapList + InputHeatmap
"+.HeatmapList" <- function(e1, e2) {
  if (inherits(e2, "InputHeatmap")) {
    add_heatmap(e1, as_ComplexHeatmap(e2))
  } else {
    # Fall back to ComplexHeatmap's method
    NextMethod("+")
  }
}

# Method for AdditiveUnit + InputHeatmap (more general)
"+.AdditiveUnit" <- function(e1, e2) {
  if (inherits(e2, "InputHeatmap")) {
    add_heatmap(e1, as_ComplexHeatmap(e2))
  } else {
    # Fall back to ComplexHeatmap's method
    NextMethod("+")
  }
}
```

### Option 2: Modify Existing Method

Alternatively, modify the existing `+.InputHeatmap` method to be more robust:

```r
"+.InputHeatmap" <- function(e1, e2) {
  # Convert e1 to ComplexHeatmap if it's InputHeatmap
  if (inherits(e1, "InputHeatmap")) {
    e1 <- as_ComplexHeatmap(e1)
  }
  
  # Convert e2 to ComplexHeatmap if it's InputHeatmap
  if (inherits(e2, "InputHeatmap")) {
    e2 <- as_ComplexHeatmap(e2)
  }
  
  add_heatmap(e1, e2)
}
```

## Implementation Considerations

1. **Backward Compatibility**: The solution should not break existing code that works with 2 heatmaps.

2. **Method Dispatch**: Need to ensure proper method dispatch order. R's S3 method dispatch looks for methods based on the class of the first argument.

3. **ComplexHeatmap Integration**: The solution should work seamlessly with ComplexHeatmap's existing `+` operator functionality.

4. **Testing**: Need to test various combinations:
   - `InputHeatmap + InputHeatmap + InputHeatmap`
   - `InputHeatmap + InputHeatmap + InputHeatmap + InputHeatmap`
   - Mixed combinations with annotations

## Files to Modify

- **`R/methods.R`**: Add the new method definitions
- **`tests/testthat/tests.R`**: Add test cases for multiple heatmap addition
- **Documentation**: Update relevant man pages if needed

## Expected Outcome

After implementing the solution, users should be able to chain multiple heatmaps:

```r
# This should work without errors
p_heatmap + p_heatmap + p_heatmap
p_heatmap + p_heatmap + p_heatmap + p_heatmap

# Mixed with annotations should also work  
p_heatmap + p_heatmap + rowAnnotation(...)
```

## Benefits

1. **Enhanced Usability**: Users can create complex multi-heatmap visualizations more easily
2. **Consistency**: Behavior aligns with ComplexHeatmap's capabilities
3. **Flexibility**: Enables more sophisticated heatmap compositions

This solution addresses the core issue while maintaining compatibility with existing functionality and following R's S3 method dispatch patterns.