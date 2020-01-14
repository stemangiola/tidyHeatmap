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
#' @importFrom magrittr equals
#' @importFrom rlang quo_is_symbol
#' @importFrom RColorBrewer brewer.pal
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
#'
#' print(1)
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
	
	# Colors
	palette_abundance = viridis(3)[1:2] %>% c("#fefada")
	palette_annotation_continuous = colorRampPalette(brewer.pal(11,"Spectral") %>% rev)
	palette_annotation_discrete = brewer.pal(8,"Dark2")
	
	colors = colorRamp2(c(-2, 0, 2), palette_abundance )
	
	# Get x and y anntation columns
	x_y_annot_cols =
		.data %>% 
		ungroup() %>%
		get_x_y_annotation_columns(!!.horizontal, !!.vertical, !!.abundance)
	
	# See if I have grouping and setup framework
	if("groups" %in%  (.data %>% attributes %>% names)) {
		x_y_annotation_cols = 
			x_y_annot_cols %>%
			map(
				~ .x %>% intersect( .data %>% attr("groups") %>% select(-.rows) %>% colnames())
			)
		
		# Row split
		row_split = 
			.data %>%
			ungroup() %>%
			distinct(!!.vertical, !!as.symbol(x_y_annotation_cols$vertical)) %>%
			arrange(!!.vertical) %>%
			pull(`Cell type`)
		
		left_annotation_args = 
			list(
				ct = anno_block(  #<<< IF i HAVE GROUPING THIS IS AUTOMATIC
					gp = gpar(fill = ct_colors(	row_split %>% unique %>% sort	)),
					labels = row_split %>% unique %>% sort,
					labels_gp = gpar(col = "white"),
					which = "row"
				)
			)
		
		left_annotation = do.call("rowAnnotation", as.list(left_annotation_args))
		
	} else {
		row_split = NULL
		left_annotation =	NULL
	}
	
	# See if there is annotation
	if(annotation %>% quo_is_symbol()) {
		x_y_annotation_cols = 
			x_y_annot_cols %>%
			map(
				~ .x %>% intersect( quo_name(annotation))
			)
		
		# Col annot
		col_annot = 
			.data %>%
			ungroup() %>%
			distinct(!!.horizontal, !!as.symbol(x_y_annotation_cols$horizontal)) %>%
			arrange(!!.horizontal) %>%
			pull(!!as.symbol(x_y_annotation_cols$horizontal))
		
		col_annot_cont = 
			.data %>% 
			ungroup() %>%
			select(!!as.symbol(x_y_annotation_cols$horizontal)) %>%
			ifelse_pipe(
				(.) %>%
					pull(1) %>% 
					class %in% c("factor", "character"),
				~ palette_annotation_discrete[1:length(unique(col_annot))] %>% setNames(unique(col_annot)),
				~ colorRamp2(0:7, palette_annotation_continuous(8))
			)
		
		left_annotation_args = 
			list(
				df = col_annot,
				col = list(	df  = col_annot_cont )
			)
		
		top_annotation = do.call("HeatmapAnnotation", as.list(left_annotation_args))
		
	} else {
		top_annotation = NULL
	}
	
	abundance_mat %>%
		Heatmap(
		column_title = quo_name(.horizontal),
		row_title = quo_name(.vertical),
		# width = unit(0.5 * 13, "cm"),
		# height = unit(0.5 * .data %>% distinct(!!.vertical) %>% nrow, "cm"),
		col = colors,
		row_split = row_split,
		left_annotation =	left_annotation,
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
