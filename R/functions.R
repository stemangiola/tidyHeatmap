#' plot_heatmap
#'
#' @description plot_heatmap() creates a `tt` object from a `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#'
#' @import dplyr
#' @import tidyr
#' @import ComplexHeatmap
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom circlize colorRamp2
#' @importFrom viridis viridis
#' @importFrom grid unit
#' @importFrom grid gpar
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom magrittr equals
#' @importFrom rlang quo_is_symbolic
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @importFrom viridis magma
#' 
#' @name plot_heatmap
#' @rdname plot_heatmap
#'
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .horizontal The name of the column horizontally presented in the heatmap
#' @param .vertical The name of the column vertically presented in the heatmap
#' @param .abundance The name of the transcript/gene abundance column
#' @param annotation Vector of quotes
#'
#' @details To be added.
#'
#' @return A `ComplexHeatmap` object
#'
#'
#' @examples
#'
#' library(tidyverse)
#' tidyHeatmap::N52 %>%
#' group_by( `Cell type`) %>%
#' tidyHeatmap::plot_heatmap(
#'  .horizontal = UBR, 
#'  .vertical = symbol_ct, 
#'  .abundance = `read count normalised log`,
#'  annotation = CAPRA_TOTAL
#' )
#'
#'
#' @export
#' 
#'
#' 
plot_heatmap = function(.data, .horizontal, .vertical, .abundance, annotation = NULL){
	
	# Make col names
	.horizontal = enquo(.horizontal)
	.vertical = enquo(.vertical)
	.abundance = enquo(.abundance)
	annotation = enquo(annotation)
	
	# Get abundance matrix
	abundance_tbl = 
		.data %>% 
		ungroup() %>%
		distinct(!!.vertical, !!.horizontal, !!.abundance) %>% 
		spread(!!.horizontal, !!.abundance) 
	
	abundance_mat =
		abundance_tbl %>%
		as_matrix(rownames=quo_name(.vertical)) %>%
		t() %>%
		apply(2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y))) %>%
		t() 
	
	# Colors tiles
	palette_abundance = viridis(3)[1:2] %>% c("#fefada")
	colors = colorRamp2(c(-2, 0, 2), palette_abundance )
	
	# Colors annotations
	palette_annotation = list(
		discrete = list( brewer.pal(9,"Set1"), brewer.pal(8,"Dark2"), brewer.pal(8,"Accent"), brewer.pal(12,"Set3"), brewer.pal(8,"Set2"), brewer.pal(8,"Pastel2") ),
		continuous = list(colorRampPalette(brewer.pal(11,"Spectral") %>% rev), colorRampPalette(viridis(n=5)), colorRampPalette(magma(n=5)),   colorRampPalette(brewer.pal(11,"PRGn")), colorRampPalette(brewer.pal(11,"BrBG") )  )
	)
	
	# Get x and y anntation columns
	x_y_annot_cols =
		.data %>% 
		ungroup() %>%
		get_x_y_annotation_columns(!!.horizontal, !!.vertical, !!.abundance)
	
	# Check if annotation is compatible with your dataset
	.data %>% 
		select(!!annotation) %>% 
		colnames %>% 
		setdiff(x_y_annot_cols %>% unlist) %>%
		ifelse_pipe(
			length(.) > 0, 
			~ stop(sprintf(
				"Your annotation \"%s\" is not unique to vertical nor horizontal dimentions", 
				.x %>% paste(collapse=", ")
			))
		)
	
	# See if I have grouping and setup framework
	group_annotation = get_group_annotation(.data, !!.horizontal, !!.vertical, !!.abundance, !!annotation, x_y_annot_cols)
	
	# See if there is annotation
	top_annotation = get_top_annotation(.data, !!.horizontal, !!.vertical, !!.abundance, !!annotation, x_y_annot_cols, palette_annotation)
	
	abundance_mat %>%
		Heatmap(
			column_title = quo_name(.horizontal),
			row_title = quo_name(.vertical),
			# width = unit(0.5 * 13, "cm"),
			# height = unit(0.5 * .data %>% distinct(!!.vertical) %>% nrow, "cm"),
			col = colors,
			row_split = group_annotation$row_split,
			left_annotation =	group_annotation$left_annotation,
			cluster_row_slices = F,
			row_names_gp = gpar(fontsize = 320 / dim(abundance_mat)[1]),
			#,
			#	clustering_distance_columns = robust_dist,
			# ,
			# 
			# inflection =  anno_points( << THIS CAN ALSO BE AUTOMATIC GIVING COLUMN DISTINCT WITH .vertical AND TYPE anno_POINTS
			# 	tbl %>% distinct(symbol_ct, inflection) %>%
			# 		arrange(symbol_ct) %>% pull(inflection)
			# )

			top_annotation  = top_annotation
		)
	
	
} 
