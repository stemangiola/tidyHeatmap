# Research Summary: tidyHeatmap Issue #134

## Issue Overview
**Title**: Retrieve data after producing heatmap  
**Reporter**: @Ollipolli1909  
**Date**: Oct 29, 2024  
**Status**: Open  

**Problem**: Users want to access the ordered data after creating a heatmap with tidyHeatmap. Currently, there's no built-in way to retrieve the row and column ordering that results from clustering, which is essential for downstream analysis.

## Key Findings

### Current Architecture Analysis
- tidyHeatmap creates `InputHeatmap` objects that store original data and parameters
- Conversion to `ComplexHeatmap` happens via `as_ComplexHeatmap()` method
- Ordering information is only available after the heatmap is actually drawn
- No existing methods in tidyHeatmap to extract ordered data

### Solution Approach
The solution leverages ComplexHeatmap's built-in functions:
- `row_order()` and `column_order()` to get ordering after drawing
- `draw()` to perform the actual layout and clustering
- Access to stored data in `InputHeatmap@data` and `InputHeatmap@input`

## Proposed Solution

### Helper Functions
Three main helper functions to extract different types of ordered information:

1. **`get_ordered_data()`** - Returns complete ordered tibble with original data
2. **`get_heatmap_order()`** - Returns just row/column names in order
3. **`get_ordered_matrix()`** - Returns abundance matrix in heatmap order

### Example Usage
```r
# Create heatmap
hm <- tidyHeatmap::N52 |>
  tidyHeatmap::heatmap(.row = symbol_ct, .column = UBR, .value = `read count normalised log`)

# Extract ordered data
result <- get_ordered_data(hm)
ordered_tibble <- result$ordered_data
row_order <- result$row_order
column_order <- result$column_order
```

## Implementation Recommendations

### Short-term (User Solution)
- Provide helper functions as demonstrated in research files
- Document the approach in tidyHeatmap issues/wiki
- Create examples for common use cases

### Long-term (Package Integration)
1. **Add methods to InputHeatmap class**:
   - `get_order()` method
   - `get_ordered_data()` method  
   - `get_ordered_matrix()` method

2. **Enhance documentation** with ordering examples

3. **Consider API design** for automatic return of ordered data

## Technical Considerations

### Limitations
- Heatmap must be drawn before ordering can be extracted
- Complex with grouped heatmaps or split dendrograms
- Annotations require separate handling
- Performance impact of re-drawing heatmaps

### Compatibility
- Solution maintains compatibility with existing tidyHeatmap design
- Leverages established ComplexHeatmap functionality
- No breaking changes to current API

## Files Created
1. `research_findings.md` - Detailed technical solution
2. `test_solution.R` - Working example implementation
3. `issue_134_summary.md` - This summary document

## Next Steps
1. Share solution with @Ollipolli1909 for feedback
2. Consider implementing as formal package methods
3. Add to tidyHeatmap documentation/examples
4. Test with various heatmap configurations

## References
- [tidyHeatmap Issue #134](https://github.com/stemangiola/tidyHeatmap/issues/134)
- [ComplexHeatmap documentation on orders](https://jokergoo.github.io/ComplexHeatmap-reference/book/a-single-heatmap.html#get-orders-and-dendrograms-from-heatmap)
- [Biostars discussions on heatmap ordering](https://www.biostars.org/p/465304/)