# Test script to verify legend text size fix in tidyHeatmap
# This tests the fix for GitHub issue #146

library(tidyHeatmap)
library(grid)

# Test basic legend text size control
cat("Testing basic legend text size control...\n")

# Use built-in sample data
hm_basic <- tidyHeatmap::N52 |>
  tidyHeatmap::heatmap(
    .row = symbol_ct,
    .column = UBR,
    .value = `read count normalised log`,
    scale = "row"
  ) |>
  annotation_tile(`Cell type`, 
                  legend_labels_gp = grid::gpar(fontsize = 20, fontface = 'bold'))

# Test comprehensive legend styling
cat("Testing comprehensive legend styling...\n")

hm_comprehensive <- tidyHeatmap::N52 |>
  tidyHeatmap::heatmap(
    .row = symbol_ct,
    .column = UBR,
    .value = `read count normalised log`,
    scale = "row"
  ) |>
  annotation_tile(`Cell type`,
                  # Legend label text (main fix)
                  legend_labels_gp = grid::gpar(fontsize = 18, fontface = 'bold', col = "darkblue"),
                  # Legend title text  
                  legend_title_gp = grid::gpar(fontsize = 20, fontface = 'bold'),
                  # Legend square sizes
                  legend_grid_height = grid::unit(8, "mm"),
                  legend_grid_width = grid::unit(8, "mm"),
                  # Annotation name (appears next to annotation)
                  annotation_name_gp = grid::gpar(fontsize = 16))

# Test annotation_name_gp specifically (the parameter mentioned in the GitHub issue)
cat("Testing annotation_name_gp parameter...\n")

hm_annotation_name <- tidyHeatmap::N52 |>
  tidyHeatmap::heatmap(
    .row = symbol_ct,
    .column = UBR,
    .value = `read count normalised log`,
    scale = "row"
  ) |>
  annotation_tile(`Cell type`,
                  annotation_name_gp = grid::gpar(fontsize = 24, fontface = 'bold', col = "red"))

cat("All tests completed successfully!\n")
cat("The fix should now allow proper control of:\n")
cat("- legend_labels_gp: Legend text size and styling\n")
cat("- legend_title_gp: Legend title styling\n") 
cat("- annotation_name_gp: Annotation name styling\n")
cat("- Other legend parameters like legend_grid_height, legend_grid_width, etc.\n")

# Print first heatmap to verify it works
print("Displaying test heatmap...")
print(hm_basic)