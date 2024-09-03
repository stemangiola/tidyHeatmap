#' Wrap tidyHeatmap (ComplexHeatmap) in a patchwork-compliant patch
#'
#' In order to add tidyHeatmap (ComplexHeatmap) element to a patchwork they can be
#' converted to a compliant representation using the `wrap_heatmap()` function.
#' This allows you to position either grobs, ggplot objects, patchwork
#' objects, or even base graphics (if passed as a formula) in either the full
#' area, the full plotting area (anything between and
#' including the axis label), or the panel area (only the actual area where data
#' is drawn). 
#'
#' @importFrom patchwork wrap_elements
#' @importFrom grid grid.grabExpr
#' @importFrom ComplexHeatmap draw
#' @importFrom methods show
#'
#' @param panel,plot,full A grob, ggplot, patchwork, formula, raster, or
#' nativeRaster object to add to the respective area.
#'
#' @param clip Should the grobs be clipped if expanding outside its area
#'
#' @param ignore_tag Should tags be ignored for this patch. This is relevant
#' when using automatic tagging of plots and the content of the patch does not
#' qualify for a tag.
#' 
#' @param padding A grid::unit object. It defined the padding distance for the plot. It is helpful when the heatmap is assembled with other ggplots through patchwork.
#'
#' @docType methods
#' @rdname wrap_heatmap-method
#' 
#' @return A wrapped_patch object
#'
#' @export
#'
#' @examples
#' 
#' 
#' tidyHeatmap::N52 |>
#' tidyHeatmap::heatmap(
#'  .row = symbol_ct,
#'  .column = UBR,
#'  .value = `read count normalised log`,
#' ) |> 
#' wrap_heatmap()
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric(
	"wrap_heatmap", 
	function(panel = NULL, plot = NULL, full = NULL, clip = TRUE, ignore_tag = FALSE, padding = NULL) standardGeneric("wrap_heatmap")
)

#' Wrap tidyHeatmap (ComplexHeatmap) in a patchwork-compliant patch
#' 
#' @docType methods
#' @rdname wrap_heatmap-method
#' 
#' @return A wrapped_patch object
setMethod("wrap_heatmap", "InputHeatmap", function(panel = NULL, plot = NULL, full = NULL, clip = TRUE, ignore_tag = FALSE, padding = NULL){
	

	# Update panel with padding if set
	if(!is.null(padding)){
		# Use the trick for avoiding plotting twice with `draw`
		t = tempfile()
		pdf(file=t)
		
		panel = 
			as_ComplexHeatmap(panel) |> 
			draw( padding = padding)
		
		invisible(dev.off())
		invisible(file.remove(t))
		
	}
		
	
	panel |> 
		methods::show() |> 
		ComplexHeatmap::draw() |> 
		ComplexHeatmap::draw() |> 
		grid::grid.grabExpr() |> 
		patchwork::wrap_elements(plot = plot, full = full, clip = clip)
	
})

# #' Wrap tidyHeatmap (ComplexHeatmap) in a patchwork-compliant patch
# #' 
# #' @docType methods
# #' @rdname wrap_heatmap-method
# #' 
# #' @return A wrapped_patch object
# setMethod("wrap_heatmap", "HeatmapList", function(panel = NULL, plot = NULL, full = NULL, clip = TRUE, ignore_tag = FALSE, padding = NULL){
# 	patchwork::wrap_elements(grid::grid.grabExpr(ComplexHeatmap::draw(ComplexHeatmap::draw(methods::show(panel)))))
# })
