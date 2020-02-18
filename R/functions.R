#' plot_heatmap
#'
#' @description plot_heatmap() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
#'
#' @import dplyr
#' @import tidyr
#' @import ComplexHeatmap
#' @importFrom magrittr "%>%"
#' @importFrom rlang enquo
#' @importFrom rlang quo_name
#' @importFrom circlize colorRamp2
#' @importFrom grDevices colorRampPalette
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
#' @param log_transform A boolean, whether the value should be log-transformed (e.g., TRUE for RNA sequencing data)
#' @param palette_abundance A character vector This is the palette that will be used as gradient for abundance.
#' @param palette_discrete A list of character vectors. This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#' @param palette_continuous A list of character vectors. This is the list of palettes that will be used for horizontal and vertical continuous annotations. The continuous classification of annotations depends on the column type of your input tibble (e.g., integer, numerical, double).
#'
#' @details To be added.
#'
#' @return A `ComplexHeatmap` object
#'
#'
#'
#'
#'
#'
plot_heatmap = function(.data,
												.horizontal,
												.vertical,
												.abundance,
												annotation = NULL,
												log_transform = FALSE,
												palette_abundance = c("#440154FF", "#21908CFF", "#fefada" ),
												palette_discrete = list(),
												palette_continuous = list()) {
	# Comply with CRAN NOTES
	. = NULL
	
	# Make col names
	.horizontal = enquo(.horizontal)
	.vertical = enquo(.vertical)
	.abundance = enquo(.abundance)
	annotation = enquo(annotation)
	
	# Check if you have more than one grouping, at the moment just one is accepted
	if (length(get_grouping_columns(.data)) > 1)
		stop("At the moment just one grouping is supported")
	
	# Check if palettes variables are correct
	if( class(palette_discrete) != "list" | class(palette_continuous) != "list")
		stop("tidyHeatmap says: palette discrete and continuous must be lists of character vectors.")
	
	# Get abundance matrix
	abundance_tbl =
		.data %>%
		ungroup() %>%
		
		# Check if log tranfrom is needed
		ifelse_pipe(log_transform,
								~ .x %>% mutate(!!.abundance := !!.abundance %>%  `+`(1) %>%  log())) %>%
		
		distinct(!!.vertical,!!.horizontal,!!.abundance) %>%
		spread(!!.horizontal,!!.abundance)
	
	abundance_mat =
		abundance_tbl %>%
		as_matrix(rownames = quo_name(.vertical)) %>%
		t() %>%
		apply(2, function(y)
			(y - mean(y)) / sd(y) ^ as.logical(sd(y))) %>%
		t()
	
	# Colors tiles
	#palette_abundance = c(viridis(3)[1:2],"#fefada")
	colors = colorRamp2(c(-2, 0, 2), palette_abundance)
	
	# Colors annotations
	palette_annotation = list(
		# Discrete pellets
		discrete = 
			palette_discrete %>%
			c( list(
				brewer.pal(9, "Set1"),
				brewer.pal(8, "Set2"),
				brewer.pal(12, "Set3"),
				brewer.pal(8, "Dark2"),
				brewer.pal(8, "Accent"),
				brewer.pal(8, "Pastel2")
			)),
		
		# Continuous pellets
		# continuous = list(
		# 	colorRampPalette(brewer.pal(11, "Spectral") %>% rev),
		# 	colorRampPalette(viridis(n = 5)),
		# 	colorRampPalette(magma(n = 5)),
		# 	colorRampPalette(brewer.pal(11, "PRGn")),
		# 	colorRampPalette(brewer.pal(11, "BrBG"))
		# )
		
		continuous = 
			palette_continuous %>%
			c(list(
				brewer.pal(11, "Spectral") %>% rev,
				viridis(n = 5),
				magma(n = 5),
				brewer.pal(11, "PRGn"),
				brewer.pal(11, "BrBG")
			))
	)
	
	# Get x and y anntation columns
	x_y_annot_cols =
		.data %>%
		ungroup() %>%
		get_x_y_annotation_columns(!!.horizontal,!!.vertical,!!.abundance)
	
	# Check if annotation is compatible with your dataset
	.data %>%
		select(!!annotation) %>%
		colnames %>%
		setdiff(x_y_annot_cols %>% unlist) %>%
		ifelse_pipe(length(.) > 0,
								~ stop(
									sprintf(
										"Your annotation \"%s\" is not unique to vertical nor horizontal dimentions",
										.x %>% paste(collapse = ", ")
									)
								))
	
	# See if I have grouping and setup framework
	group_annotation = get_group_annotation(
		.data,
		!!.horizontal,
		!!.vertical,
		!!.abundance,
		!!annotation,
		x_y_annot_cols,
		palette_annotation
	)
	
	# If I have grouping, eliminate the first discrete palette
	palette_annotation$discrete =
		palette_annotation$discrete %>%
		ifelse_pipe(length(get_grouping_columns(.data)) > 0, ~ .x[-1])
	
	# See if there is annotation
	top_left_annot = get_top_left_annotation(
		.data,
		!!.horizontal,
		!!.vertical,
		!!.abundance,
		!!annotation,
		x_y_annot_cols,
		palette_annotation
	)
	

	top_annot =  
		top_left_annot$top_annotation %>%
		ifelse_pipe(
			(.) %>% is.null %>% `!`,
			~ do.call("HeatmapAnnotation", .x),
			~ NULL
		)
	
	left_annot = 
		c(group_annotation$left_annotation, top_left_annot$left_annotation) %>%
		list_drop_null() %>%
		ifelse_pipe(
			(.) %>% length %>% `>` (0),
			~ do.call("rowAnnotation", .x),
			~ NULL
		)
	

	abundance_mat %>%
		Heatmap(
			name = "Abundance",
			column_title = quo_name(.horizontal),
			row_title = quo_name(.vertical),
			# width = unit(0.5 * 13, "cm"),
			# height = unit(0.5 * .data %>% distinct(!!.vertical) %>% nrow, "cm"),
			col = colors,
			row_split = group_annotation$row_split,
			left_annotation = left_annot,
			cluster_row_slices = FALSE,
			row_names_gp = gpar(fontsize = 320 / dim(abundance_mat)[1]),
			#,
			#	clustering_distance_columns = robust_dist,
			# ,
			#
			# inflection =  anno_points( << THIS CAN ALSO BE AUTOMATIC GIVING COLUMN DISTINCT WITH .vertical AND TYPE anno_POINTS
			# 	tbl %>% distinct(symbol_ct, inflection) %>%
			# 		arrange(symbol_ct) %>% pull(inflection)
			# )
			
			top_annotation  = top_annot
		)
	
	
}
