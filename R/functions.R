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
#' @importFrom ggplot2 unit
#'
#' @name plot_heatmap
#' @rdname plot_heatmap
#'
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .x The name of the column horizontally presented in the heatmap
#' @param .y The name of the column vertically presented in the heatmap
#' @param .abundance The name of the transcript/gene abundance column
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
plot_heatmap = function(.data, .x, .y, .abundance){
	
	# Make col names
	.x = enquo(.x)
	.y = enquo(.y)
	.abundance = enquo(.abundance)
	

	abundance_tbl = 
		.data %>% 
		distinct(!!.y, !!.x, !!.abundance) %>% 
		spread(!!.x, !!.abundance) 
	
	abundance_mat =
		abundance_tbl %>%
		ttBulk::as_matrix(rownames=quo_name(.y)) %>%
		t() %>%
		apply(2, function(y) (y - mean(y)) / sd(y) ^ as.logical(sd(y))) %>%
		t() 
	
	palette_abundance = viridis(3)[1:2] %>% c("#fefada")
	colors = colorRamp2(c(-2, 0, 2), palette_abundance )
	
	# # If data grouped
	# if()
	# group_columns_x
	# group_columns_y
	
	abundance_mat %>%
		Heatmap(
		column_title = quo_name(.x),
		row_title = quo_name(.y),
		width = unit(0.5 * 13, "cm"),
		height = unit(0.5 * .data %>% distinct(!!.y) %>% nrow, "cm"),
		col = colors,
		# row_split = 
		# 	.data %>%
		# 	ungroup()
		# 	distinct(!!.y) %>%
		# 	left_join(
		# 		.data %>%
		# 	)
		# 	attr("groups") %>%
		# 	
		# distinct(symbol_ct, `Cell type`) %>% 
		# 	arrange(symbol_ct) %>% pull(`Cell type`), 
		cluster_row_slices = FALSE
		#,
		#	clustering_distance_columns = robust_dist,
		# left_annotation =
		# 	rowAnnotation(
		# 		ct = anno_block(  <<< IF i HAVE GROUPING THIS IS AUTOMATIC
		# 			gp = gpar(fill = ct_colors(
		# 				tbl %>% distinct(`Cell type`) %>% arrange(`Cell type`) %>% pull(`Cell type`) %>% as.character
		# 			)), 
		# 			labels = ct_names(
		# 				tbl %>% distinct(`Cell type`) %>% arrange(`Cell type`) %>% pull(`Cell type`) %>% as.character
		# 			), 
		# 			labels_gp = gpar(col = "white")
		# 		),
		# 		
		# 		inflection =  anno_points( << THIS CAN ALSO BE AUTOMATIC GIVING COLUMN DISTINCT WITH .Y AND TYPE anno_POINTS
		# 			tbl %>% distinct(symbol_ct, inflection) %>% 
		# 				arrange(symbol_ct) %>% pull(inflection)
		# 		)
		# 		
		# 	),
		# top_annotation  =
		# 	HeatmapAnnotation(  <<< IF i HAVE GROUPING THIS IS AUTOMATIC
		# 		`CAPRA-S` =  tbl %>% distinct(UBR, CAPRA_TOTAL) %>% 
		# 			arrange(UBR) %>% pull(CAPRA_TOTAL) ,
		# 		col = list(	`CAPRA-S`  = circlize::colorRamp2(0:7, colorRampPalette(RColorBrewer ::brewer.pal(11,"Spectral") %>% rev)(8)))
		# 	)
	)
	
	
} 
