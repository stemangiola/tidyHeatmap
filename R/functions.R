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
#' @importFrom purrr negate
#' @importFrom magrittr equals
#' @importFrom rlang quo_is_symbolic
#' @importFrom RColorBrewer brewer.pal
#' @importFrom viridis viridis
#' @importFrom viridis magma
#' @importFrom rlang is_function
#' @importFrom purrr when
#'
#' @name plot_heatmap
#' @rdname plot_heatmap
#'
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .horizontal The name of the column horizontally presented in the heatmap
#' @param .vertical The name of the column vertically presented in the heatmap
#' @param .abundance The name of the transcript/gene abundance column
#' @param annotation Vector of quotes
#' @param transform A function, used to tranform .value, for example log1p
#' @param .scale A character string. Possible values are c(\"none\", \"row\", \"column\", \"both\")
#' @param palette_abundance A character vector, or a function for higher customisation (colorRamp2). This is the palette that will be used as gradient for abundance. If palette_abundance is a vector of hexadecimal colous, it should have 3 values. If you want more customisation, you can pass to palette_abundance a function, that is derived as for example `colorRamp2(c(-2, 0, 2), palette_abundance)`
#' @param palette_discrete A list of character vectors. This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#' @param palette_continuous A list of character vectors. This is the list of palettes that will be used for horizontal and vertical continuous annotations. The continuous classification of annotations depends on the column type of your input tibble (e.g., integer, numerical, double).
#' @param ... Further arguments to be passed to ComplexHeatmap::Heatmap
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
												transform = NULL,
												.scale = "row",
												palette_abundance = c("#440154FF", "#21908CFF", "#fefada" ), #c(viridis(3)[1:2],"#fefada")
												palette_discrete = list(),
												palette_continuous = list(),
												...) {
	# Comply with CRAN NOTES
	. = NULL
	
	# Make col names
	.horizontal = enquo(.horizontal)
	.vertical = enquo(.vertical)
	.abundance = enquo(.abundance)
	annotation = enquo(annotation)
	
	# Check if palette discrete and continuous are lists
	if(!is.list(palette_discrete) | !is.list(palette_continuous))
		stop("tidyHeatmap says: the arguments palette_discrete and palette_continuous must be lists. E.g., list(rep(\"#000000\", 20))")
	
	# Get abundance matrix
	abundance_tbl =
		.data %>%
		ungroup() %>%
		
		# Check if tranfrom is needed
		ifelse_pipe(
			is_function(transform),
			
			# Transform rowwise
			~ .x %>% 
				mutate(!!.abundance := !!.abundance %>% transform()) %>%
				
				# Check if log introduced -Inf
				ifelse_pipe(
					pull(., !!.abundance) %>% min %>% equals(-Inf), 
					~ stop("tidyHeatmap says: you applied a transformation that introduced negative infinite .value, was it log? If so please use log1p.")
				)
		) %>%
		
		# If .scale row
		when(
			.scale %in% c("row", "both") ~ (.) %>%
				nest(data = -!!.vertical) %>%
				mutate(data = map(data, ~ .x %>% mutate(!!.abundance := !!.abundance %>% scale_robust()))) %>%
				unnest(data),
			~ (.)
		) %>%
		
		# If .scale column
		when(
			.scale %in% c("column", "both") ~ (.) %>%
				nest(data = -!!.horizontal) %>%
				mutate(data = map(data, ~ .x %>% mutate(!!.abundance := !!.abundance %>% scale_robust()))) %>%
				unnest(data),
			~ (.)
		) %>%
		
		distinct(!!.vertical,!!.horizontal,!!.abundance) %>%
		spread(!!.horizontal,!!.abundance)
	
	abundance_mat =
		abundance_tbl %>%
		as_matrix(rownames = quo_name(.vertical)) 
		# %>%
		# t() %>%
		# apply(2, scale_robust) %>%
		# t()
	 
	# Colors tiles
	# If palette_abundance is a function pass it directly, otherwise check if the character array is of length 3
	colors = 
		palette_abundance %>%
		ifelse2_pipe(
			palette_abundance %>% class() %>% equals("function"),
			length(palette_abundance) != 3,
			~ .x,
			~ stop("tidyHeatmap says: If palette_abundance is a vector of hexadecimal colous, it should have 3 values. If you want more customisation, you can pass to palette_abundance a function, that is derived as for example \"colorRamp2(c(-2, 0, 2), palette_abundance)\""	),
			~ colorRamp2(
				
				# min and max and intermediates based on length of the palette
				seq(from=min(abundance_mat), to=max(abundance_mat), length.out = length(palette_abundance)),
				palette_abundance
			)
		)
	
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
	
	# Check if there are nested column in the data frame
	if(.data %>% lapply(class)  %>% equals("list") %>% any)
		warning("tidyHeatmap says: nested/list column are present in your data frame and have been dropped as their unicity cannot be identified by dplyr.")
	
	# Get x and y anntation columns
	x_y_annot_cols =
		.data %>%
		select_if(negate(is.list)) %>%
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
										"tidyHeatmap says: Your annotation \"%s\" is not unique to vertical nor horizontal dimentions",
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
		ifelse_pipe(length(get_grouping_columns(.data)) > 0, ~ tail(.x, -length(get_grouping_columns(.data))))
	
	# See if there is annotation
	top_left_annot = 
		.data %>%
		
		# Check if NA in annotations
		mutate_at(vars(!!annotation), function(x) { replace_na(x, "NA")  } ) %>% 
			
		# Get annotation
		 get_top_left_annotation(
			!!.horizontal,
			!!.vertical,
			!!.abundance,
			!!annotation,
			x_y_annot_cols,
			palette_annotation
		)
	

	top_annot =  
		c(group_annotation$top_annotation, top_left_annot$top_annotation) %>%
		list_drop_null() %>%
		ifelse_pipe(
			(.) %>% length %>% `>` (0) && !is.null((.)), # is.null needed for check Windows CRAN servers
			~ do.call("columnAnnotation", .x ),
			~ NULL
		)
	
	left_annot = 
		c(group_annotation$left_annotation, top_left_annot$left_annotation) %>%
		list_drop_null() %>%
		ifelse_pipe(
			(.) %>% length %>% `>` (0) && !is.null((.)), # is.null needed for check Windows CRAN servers
			~ do.call("rowAnnotation", .x),
			~ NULL
		)
	

	abundance_mat %>%
		Heatmap(
			name = quo_name(.abundance),
			column_title = quo_name(.horizontal),
			row_title = quo_name(.vertical),
			# width = unit(0.5 * 13, "cm"),
			# height = unit(0.5 * .data %>% distinct(!!.vertical) %>% nrow, "cm"),
			col = colors,
			row_split = group_annotation$row_split,
			column_split = group_annotation$col_split,
			left_annotation = left_annot,
			top_annotation  = top_annot,
			cluster_row_slices = FALSE,
			cluster_column_slices = FALSE,
			row_names_gp = gpar(fontsize = min(12, 320 / dim(abundance_mat)[1])),
			column_names_gp = gpar(fontsize = min(12, 320 / dim(abundance_mat)[2])),
			#,
			#	clustering_distance_columns = robust_dist,
			# ,
			#
			# inflection =  anno_points( << THIS CAN ALSO BE AUTOMATIC GIVING COLUMN DISTINCT WITH .vertical AND TYPE anno_POINTS
			# 	tbl %>% distinct(symbol_ct, inflection) %>%
			# 		arrange(symbol_ct) %>% pull(inflection)
			# )
			
			...
		)
	
	
}
