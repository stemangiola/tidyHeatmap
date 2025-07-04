# Solution: Fix Legend Text Size in tidyHeatmap

## Problem
Users were unable to control legend text size in tidyHeatmap annotations using parameters like `legend_labels_gp` and `annotation_name_gp`. These parameters were being filtered out or not properly passed to ComplexHeatmap's `HeatmapAnnotation` function.

## Root Cause
The issue was in the `filter_args()` function in `R/utilities.R` and how `further_arguments` were handled for tile annotations in `get_top_left_annotation()`. Legend-related parameters were not being recognized as valid arguments for `HeatmapAnnotation`.

## Solution
1. **Updated `filter_args()` function** to always include common legend and annotation parameters:
   - `legend_labels_gp` - Controls legend label text appearance
   - `legend_title_gp` - Controls legend title text appearance  
   - `annotation_name_gp` - Controls annotation name appearance
   - `legend_grid_height`, `legend_grid_width` - Controls legend square sizes
   - `show_legend`, `legend_border`, etc. - Other legend controls

2. **Improved argument filtering** in `get_top_left_annotation()` for tile annotations to ensure proper parameter passing to `HeatmapAnnotation`.

## Usage Examples

### Basic Legend Text Size Control
```r
# Increase legend label text size
your_data |>
  tidyHeatmap::heatmap(
    .row = gene,
    .column = sample, 
    .value = expression
  ) |>
  annotation_tile(Metabolic_Category,
                  legend_labels_gp = grid::gpar(fontsize = 20, fontface = 'bold'))
```

### Comprehensive Legend Styling
```r
# Full legend customization
your_data |>
  tidyHeatmap::heatmap(
    .row = gene,
    .column = sample, 
    .value = expression
  ) |>
  annotation_tile(Metabolic_Category,
                  # Legend label text
                  legend_labels_gp = grid::gpar(fontsize = 18, fontface = 'bold', col = "darkblue"),
                  # Legend title text  
                  legend_title_gp = grid::gpar(fontsize = 20, fontface = 'bold'),
                  # Legend square sizes
                  legend_grid_height = grid::unit(8, "mm"),
                  legend_grid_width = grid::unit(8, "mm"),
                  # Annotation name (appears next to annotation)
                  annotation_name_gp = grid::gpar(fontsize = 16))
```

### Multiple Annotations with Different Legend Styles
```r
your_data |>
  tidyHeatmap::heatmap(.row = gene, .column = sample, .value = expression) |>
  annotation_tile(category1, 
                  legend_labels_gp = grid::gpar(fontsize = 16),
                  legend_title_gp = grid::gpar(fontsize = 18)) |>
  annotation_tile(category2,
                  legend_labels_gp = grid::gpar(fontsize = 14, fontface = 'italic'),
                  show_legend = TRUE)
```

## Key Parameters Fixed
- `legend_labels_gp` - **Primary parameter for legend text size**
- `legend_title_gp` - Legend title styling
- `annotation_name_gp` - Annotation name styling
- `legend_grid_height` / `legend_grid_width` - Legend square dimensions
- `show_legend` - Show/hide legends
- `legend_border` - Legend border styling

## Files Modified
- `R/utilities.R` - Updated `filter_args()` and `get_top_left_annotation()` functions

## Backward Compatibility
This fix is fully backward compatible and doesn't change existing functionality - it only ensures that previously ignored parameters now work as expected.