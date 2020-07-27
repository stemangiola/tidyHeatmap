#' input_heatmap
#'
#' @description input_heatmap() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
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
#' @importFrom rlang dots_list
#' @importFrom methods new
#'
#' @name input_heatmap
#' @rdname input_heatmap
#'
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .horizontal The name of the column horizontally presented in the heatmap
#' @param .vertical The name of the column vertically presented in the heatmap
#' @param .abundance The name of the transcript/gene abundance column
#' @param transform A function, used to transform .value, for example log1p
#' @param .scale A character string. Possible values are c(\"none\", \"row\", \"column\", \"both\")
#' @param palette_abundance A character vector, or a function for higher customisation (colorRamp2). This is the palette that will be used as gradient for abundance. If palette_abundance is a vector of hexadecimal colours, it should have 3 values. If you want more customisation, you can pass to palette_abundance a function, that is derived as for example `colorRamp2(c(-2, 0, 2), palette_abundance)`
#' @param palette_grouping A list of character vectors. This is the list of palettes that will be used for grouping 
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
input_heatmap = function(.data,
												.horizontal,
												.vertical,
												.abundance,
												transform = NULL,
												.scale = "row",
												palette_abundance = c("#440154FF", "#21908CFF", "#fefada" ), #c(viridis(3)[1:2],"#fefada")
												palette_grouping = list(),
												...) {
	

	# Comply with CRAN NOTES
	. = NULL
	col_name = NULL
	orientation = NULL
	
	# Make col names
	.horizontal = enquo(.horizontal)
	.vertical = enquo(.vertical)
	.abundance = enquo(.abundance)
	 
	# Arguments
	arguments = 
		as.list(environment()) %>% 
		c(list(.horizontal = .horizontal, .vertical = .vertical, .abundance = .abundance))
	
	# Check if palette discrete and continuous are lists
	if(!is.list(palette_grouping) )
		stop("tidyHeatmap says: the arguments palette_discrete and palette_continuous must be lists. E.g., list(rep(\"#000000\", 20))")
	
	# Check that there have at least one value in the heatmap
	if(.data %>% filter(!!.abundance %>% is.na %>% `!`) %>% nrow %>% equals(0))
		stop("tidyHeatmap says: your dataset does not have any non NA values")
	
	# Get abundance matrix
	abundance_tbl =
		.data %>%
		ungroup() %>%
		
		# Check if transform is needed
		when(
			is_function(transform) ~ 
				mutate(., !!.abundance := !!.abundance %>% transform()) %>%
				
				# Check if log introduced -Inf
				when(
					
					# NAN produced
					filter(., !!.abundance %>% is.nan) %>% nrow %>% `>` (0) ~ stop("tidyHeatmap says: you applied a transformation that introduced NaN."),
					
					# -Inf produced
					pull(., !!.abundance) %>% min %>% equals(-Inf) ~ stop("tidyHeatmap says: you applied a transformation that introduced negative infinite .value, was it log? If so please use log1p."),
					~(.)
				),
			~ (.)
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
	 
	# Colors tiles
	# If palette_abundance is a function pass it directly, otherwise check if the character array is of length 3
	colors = 
		palette_abundance %>%
		when(
			palette_abundance %>% class() %>% equals("function") ~ (.),
			length(palette_abundance) != 3 ~ stop("tidyHeatmap says: If palette_abundance is a vector of hexadecimal colours, it should have 3 values. If you want more customisation, you can pass to palette_abundance a function, that is derived as for example \"colorRamp2(c(-2, 0, 2), palette_abundance)\""	),
			
			# For the crazy scenario when only one value is present in the heatmap (tidyHeatmap/issues/40)
			min(abundance_mat, na.rm = T) == max(abundance_mat, na.rm = T) ~ colorRamp2(
				
				# min and max and intermediates based on length of the palette
				seq(from=min(abundance_mat, na.rm = T)-1, to=max(abundance_mat, na.rm = T)+1, length.out = length(palette_abundance)),
				palette_abundance
			),
			
			# In the normal situation
			~ colorRamp2(
				
				# min and max and intermediates based on length of the palette
				seq(from=min(abundance_mat, na.rm = T), to=max(abundance_mat, na.rm = T), length.out = length(palette_abundance)),
				palette_abundance
			)
		)
	
	# Define object
	new(
		"InputHeatmap",
		data = .data %>% reduce_to_tbl_if_in_class_chain,
		input = list(
			abundance_mat,
			name = quo_name(.abundance),
			column_title = quo_name(.horizontal),
			row_title = quo_name(.vertical),
			col = colors,
			row_names_gp = gpar(fontsize = min(12, 320 / dim(abundance_mat)[1])),
			column_names_gp = gpar(fontsize = min(12, 320 / dim(abundance_mat)[2]))
		)  %>%
		c(rlang::dots_list(...)),
		arguments = arguments
	)
	
}

#' @importFrom utils tail
add_grouping = function(my_input_heatmap){
	   
	# Fix CRAN nots
	.rows = NULL
	
	
	# Check if there are nested column in the data frame
	if(my_input_heatmap@data %>% lapply(class)  %>% equals("list") %>% any)
		warning("tidyHeatmap says: nested/list column are present in your data frame and have been dropped as their unicity cannot be identified by dplyr.")
	
	# Column names
	.horizontal = my_input_heatmap@arguments$.horizontal
	.vertical = my_input_heatmap@arguments$.vertical
	.abundance = my_input_heatmap@arguments$.abundance
	
	# Add custom palette to discrete if any
	my_input_heatmap@palette_discrete = my_input_heatmap@palette_discrete %>% c(my_input_heatmap@arguments$palette_grouping)
	
	# Number of grouping
	how_many_grouping = my_input_heatmap@data %>% attr("groups") %>% select(-.rows) %>% ncol
	
	# Colors annotations
	palette_annotation = my_input_heatmap@palette_discrete %>% head(how_many_grouping) 
	
	# Take away used palettes
	my_input_heatmap@palette_discrete = my_input_heatmap@palette_discrete %>% tail(-how_many_grouping)
	
	# See if I have grouping and setup framework
	group_annotation = get_group_annotation(
		my_input_heatmap@data,
		!!.horizontal,
		!!.vertical,
		!!.abundance,
		palette_annotation
	)
	
	# Isolate top annotation
	my_input_heatmap@group_top_annotation = group_annotation$top_annotation 
	
	# Isolate left annotation
	my_input_heatmap@group_left_annotation = group_annotation$left_annotation 
	
	my_input_heatmap@input  =
		my_input_heatmap@input %>%
		c(
			list(
				row_split = group_annotation$row_split,
				column_split = group_annotation$col_split,
				cluster_row_slices = FALSE,
				cluster_column_slices = FALSE
			)
		) 
	
	my_input_heatmap
}


#' add_annotation
#'
#' @description add_annotation() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
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
#' @importFrom rlang dots_list
#'
#' @name add_annotation
#' @rdname add_annotation
#'
#' @param my_input_heatmap A `InputHeatmap` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param annotation Vector of quotes
#' @param type A character vector of the set c(\"tile\", \"point\", \"bar\", \"line\")
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
add_annotation = function(my_input_heatmap,
												 annotation,
												 type = rep("tile", length(quo_names(annotation))),
												 palette_discrete = list(),
												 palette_continuous = list()) {
	
	# Solve CRAN note
	annot_type = NULL
	
	.data = my_input_heatmap@data

	
	# Comply with CRAN NOTES
	. = NULL
	col_name = NULL
	orientation = NULL
	
	# Make col names
	# Column names
	.horizontal = my_input_heatmap@arguments$.horizontal
	.vertical = my_input_heatmap@arguments$.vertical
	.abundance = my_input_heatmap@arguments$.abundance
	annotation = enquo(annotation)
	
	# Check if palette discrete and continuous are lists
	if(!is.list(palette_discrete) | !is.list(palette_continuous))
		stop("tidyHeatmap says: the arguments palette_discrete and palette_continuous must be lists. E.g., list(rep(\"#000000\", 20))")
	
	# Add custom palette to discrete if any
	my_input_heatmap@palette_discrete = palette_discrete %>% c(my_input_heatmap@palette_discrete)
	my_input_heatmap@palette_continuous = palette_continuous %>% c(my_input_heatmap@palette_continuous)
	
	# Colors annotations
	palette_annotation = list(
		discrete = 	my_input_heatmap@palette_discrete,
		continuous = my_input_heatmap@palette_continuous
	)
	
	# Check if there are nested column in the data frame
	if(.data %>% lapply(class)  %>% equals("list") %>% any)
		warning("tidyHeatmap says: nested/list column are present in your data frame and have been dropped as their unicity cannot be identified by dplyr.")
	
	# Data frame of row and column columns
	x_y_annot_cols = 
		.data %>%
		get_x_y_annotation_columns(!!.horizontal,!!.vertical,!!.abundance) 
	
	# Check if annotation is compatible with your dataset
	quo_names(annotation) %>%
		setdiff(x_y_annot_cols %>% pull(col_name)) %>%
		when( quo_names(annotation) != "NULL" & length(.) > 0 ~ 
						stop(
							sprintf(
								"tidyHeatmap says: Your annotation \"%s\" is not unique to vertical nor horizontal dimentions",
								(.) %>% paste(collapse = ", ")
							)
						))
	
	# Get annotation
	.data_annot = 
		.data %>%
		get_top_left_annotation( !!.horizontal,
														 !!.vertical,
														 !!.abundance,
														 !!annotation,	palette_annotation,	type, x_y_annot_cols)
	
	# Number of grouping
	how_many_discrete = .data_annot %>% filter(annot_type=="discrete") %>% nrow
	how_many_continuous = .data_annot %>% filter(annot_type=="continuous") %>% nrow
	
	# Eliminate used  annotations
	my_input_heatmap@palette_discrete = my_input_heatmap@palette_discrete %>% when(how_many_discrete>0 ~ tail(., -how_many_discrete) , ~ (.))
	my_input_heatmap@palette_continuous = my_input_heatmap@palette_continuous %>% when(how_many_continuous>0 ~ tail(., -how_many_continuous), ~ (.))
	
	# # Check if annotation is compatible with your dataset
	# x_y_annot_cols %>%
	# 	inner_join(.data_annot %>% distinct(col_name), by="col_name") %>%
	# 	count(col_name) %>%
	# 	filter(n > 1) %>%
	# 	pull(col_name) %>%
	# 	when( length(.) > 0 ~ 
	# 				stop(
	# 					sprintf(
	# 						"tidyHeatmap says: Your annotation \"%s\" is unique to vertical and horizontal dimentions",
	# 						(.) %>% paste(collapse = ", ")
	# 					)
	# 					))
	
	# Isolate top annotation
	my_input_heatmap@top_annotation =  
		my_input_heatmap@top_annotation %>%
		bind_rows(.data_annot %>% filter(orientation == "column") ) 
	
	# Isolate left annotation
	my_input_heatmap@left_annotation = 
		my_input_heatmap@left_annotation %>%
		bind_rows(	.data_annot %>% filter(orientation == "row") ) 

	my_input_heatmap
	
}
