plot_heatmap = function(.data,
												.horizontal,
												.vertical,
												.abundance,
												annotation = NULL,
												type = rep("tile", length(quo_names(annotation))),
												transform = NULL,
												.scale = "row",
												palette_value = c("#440154FF", "#21908CFF", "#fefada" ), #c(viridis(3)[1:2],"#fefada")
												palette_discrete = list(),
												palette_continuous = list(),
												...) {
	
	# Comply with CRAN NOTES
	. = NULL
	col_name = NULL
	orientation = NULL
	
	
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
		
		# Check if transform is needed
		when(
			is_function(transform) ~ 
				mutate(., !!.abundance := !!.abundance %>% transform()) %>%
				
				# Check if log introduced -Inf
				when(
					
					# NAN produced
					filter(., !!.abundance %>% is.nan) %>% nrow %>% gt(0) ~ stop("tidyHeatmap says: you applied a transformation that introduced NaN."),
					
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
	# If palette_value is a function pass it directly, otherwise check if the character array is of length 3
	colors = 
		palette_value %>%
		ifelse2_pipe(
			palette_value %>% class() %>% equals("function"),
			length(palette_value) != 3,
			~ .x,
			~ stop("tidyHeatmap says: If palette_value is a vector of hexadecimal colous, it should have 3 values. If you want more customisation, you can pass to palette_value a function, that is derived as for example \"colorRamp2(c(-2, 0, 2), palette_value)\""	),
			~ colorRamp2(
				
				# min and max and intermediates based on length of the palette
				seq(from=min(abundance_mat), to=max(abundance_mat), length.out = length(palette_value)),
				palette_value
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
	
	# See if I have grouping and setup framework
	group_annotation = get_group_annotation_OLD(
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
		ifelse_pipe(length(get_grouping_columns_OLD(.data)) > 0, ~ tail(.x, -length(get_grouping_columns_OLD(.data))))
	
	# Get annotation
	.data_annot = 
		.data %>%
		get_top_left_annotation_OLD(	!!.horizontal,
														 !!.vertical,
														 !!.abundance,
														 !!annotation,	palette_annotation,	type, x_y_annot_cols)
	
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
	top_annot =  
		c(
			group_annotation$top_annotation, 
			.data_annot %>% 
				filter(orientation == "column") %>%
				annot_to_list_OLD()
		) %>%
		list_drop_null() %>%
		ifelse_pipe(
			(.) %>% length %>% gt(0) && !is.null((.)), # is.null needed for check Windows CRAN servers
			~ do.call("columnAnnotation", .x ),
			~ NULL
		)
	
	# Isolate left annotation
	left_annot = 
		c(group_annotation$left_annotation, .data_annot %>% 
				filter(orientation == "row") %>%
				annot_to_list_OLD()) %>%
		list_drop_null() %>%
		ifelse_pipe(
			(.) %>% length %>% gt(0) && !is.null((.)), # is.null needed for check Windows CRAN servers
			~ do.call("rowAnnotation", .x),
			~ NULL
		)
	
	abundance_mat %>%
		Heatmap(
			name = quo_name(.abundance),
			column_title = quo_name(.horizontal),
			row_title = quo_name(.vertical),
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

get_top_left_annotation_OLD = function(.data_, .column, .row, .abundance, annotation, palette_annotation, type, x_y_annot_cols){
	
	# Comply with CRAN NOTES
	data = NULL
	fx = NULL
	annot = NULL
	annot_type = NULL
	idx = NULL
	value = NULL
	orientation = NULL
	col_name = NULL
	col_orientation = NULL
	
	
	.column = enquo(.column) 
	.row = enquo(.row) 
	.abundance = enquo(.abundance)
	annotation = enquo(annotation)
	
	#type_to_annot_function = list("tile" = NULL, "point" = anno_points, "bar" = anno_barplot, "line" = anno_lines)
	annotation_function = type_to_annot_function[type]
	
	# Create dataset
	quo_names(annotation) %>%
		as_tibble %>%
		rename(col_name = value) %>%
		
		# delete if annotation is NULL
		when(quo_is_null(annotation) ~ slice(., 0), ~ (.)) %>%
		
		# Add orientation
		left_join(x_y_annot_cols,  by = "col_name") %>%
		mutate(col_orientation = map_chr(orientation, ~ .x %>% when((.) == "column" ~ quo_name(.column), (.) == "row" ~ quo_name(.row)))) %>%
		
		# Add data
		mutate(
			data = map2(
				col_name,
				col_orientation,
				~
					.data_ %>%
					ungroup() %>%
					select(.y, .x) %>%
					distinct() %>%
					arrange_at(vars(.y)) %>%
					pull(.x)
			)
		)  %>%
		
		# Add function
		mutate(fx = annotation_function) %>%
		
		# Apply annot function if not NULL otherwise pass original annotation
		# This because no function for ComplexHeatmap = to tile
		mutate(annot = pmap(list(data, fx, orientation), ~  {
			
			# Trick needed for map BUG: could not find function "..2"
			fx = ..2
			
			# Do conditional
			if(is_function(fx)) fx(..1, which=..3) 
			else .x
		})) %>%
		
		# # Check if NA in annotations
		# mutate_at(vars(!!annotation), function(x) {
		# 	if(any(is.na(x))) { warning("tidyHeatmap says: You have NAs into your annotation column"); replace_na(x, "NA"); } 
		# 	else { x } 
		# } ) %>% 
		
		# Add color indexes separately for each orientation
		mutate(annot_type = map_chr(annot, ~ .x %>% when(class(.) %in% c("factor", "character", "logical") ~ "discrete",
																										 class(.) %in% c("integer", "numerical", "numeric", "double") ~ "continuous",
																										 ~ "other"
		) )) %>%
		group_by(annot_type) %>%
		mutate(idx =  row_number()) %>%
		ungroup() %>%
		mutate(color = map2(annot, idx,  ~ {
			if(.x %>% class %in% c("factor", "character", "logical"))
				colorRampPalette(palette_annotation$discrete[[.y]])(length(unique(.x))) %>% set_names(unique(.x))
			else if (.x %>% class %in% c("integer", "numerical", "numeric", "double"))
				colorRampPalette(palette_annotation$continuous[[.y]])(length(.x)) %>% colorRamp2(seq(min(.x), max(.x), length.out = length(.x)), .)
			else NULL
		})) %>%
		
		# Stop if annotations discrete bigger than palette
		when(
			(.) %>%  pull(data) %>% map_chr(~ .x %>% class) %in% 
				c("factor", "character") %>% which %>% length %>%
				gt(palette_annotation$discrete %>% length) ~
				stop("tidyHeatmap says: Your discrete annotaton columns are bigger than the palette available"),
			~ (.)
		) %>%
		
		# Stop if annotations continuous bigger than palette
		when(
			(.) %>%  pull(data) %>% map_chr(~ .x %>% class) %in% 
				c("int", "dbl", "numeric") %>% which %>% length %>%
				gt( palette_annotation$continuous %>% length) ~
				stop("tidyHeatmap says: Your continuous annotaton columns are bigger than the palette available"),
			~ (.)
		)
	
	
}

get_group_annotation_OLD = function(.data, .column, .row, .abundance, annotation, x_y_annot_cols, palette_annotation){
	
	# Comply with CRAN NOTES
	data = NULL
	. = NULL
	orientation = NULL
	
	# Make col names
	.column = enquo(.column)
	.row = enquo(.row)
	.abundance = enquo(.abundance)
	annotation = enquo(annotation)
	
	# Setup default NULL
	top_annotation = NULL
	left_annotation = NULL
	row_split = NULL
	col_split = NULL
	
	# Column groups
	col_group = get_grouping_columns_OLD(.data)
	
	if("groups" %in%  (.data %>% attributes %>% names)) {
		x_y_annotation_cols = 
			x_y_annot_cols %>%
			nest(data = -orientation) %>%
			mutate(data = map(data, ~ .x %>% pull(1))) %>%
			{
				df = (.)
				pull(df, data) %>% set_names(pull(df, orientation))
			} %>%
			map(
				~ .x %>% intersect(col_group)
			)
		
		# Check if you have more than one grouping, at the moment just one is accepted
		if(x_y_annotation_cols %>% lapply(length) %>% unlist %>% max %>% gt(1))
			stop("tidyHeatmap says: At the moment just one grouping per dimension (max 1 row and 1 column) is supported.")
		
		if(length(x_y_annotation_cols$row) > 0){
			
			# Row split
			row_split = 
				.data %>%
				ungroup() %>%
				distinct(!!.row, !!as.symbol(x_y_annotation_cols$row)) %>%
				arrange(!!.row) %>%
				pull(!!as.symbol(x_y_annotation_cols$row))
			
			# Create array of colors
			palette_fill_row = palette_annotation$discrete[[1]][1:length(unique(row_split))] %>% set_names(unique(row_split))
			
			left_annotation_args = 
				list(
					ct = anno_block(  
						gp = gpar(fill = palette_fill_row ),
						labels = row_split %>% unique %>% sort,
						labels_gp = gpar(col = "white"),
						which = "row"
					)
				)
			
			left_annotation = as.list(left_annotation_args)
			
			# Eliminate palette
			palette_annotation$discrete = palette_annotation$discrete[-1]
			
		}
		
		if(length(x_y_annotation_cols$column) > 0){
			# Col split
			col_split = 
				.data %>%
				ungroup() %>%
				distinct(!!.column, !!as.symbol(x_y_annotation_cols$column)) %>%
				arrange(!!.column) %>%
				pull(!!as.symbol(x_y_annotation_cols$column))
			
			# Create array of colors
			palette_fill_column = palette_annotation$discrete[[1]][1:length(unique(col_split))] %>% set_names(unique(col_split))
			
			top_annotation_args = 
				list(
					ct = anno_block(  
						gp = gpar(fill = palette_fill_column ),
						labels = col_split %>% unique %>% sort,
						labels_gp = gpar(col = "white"),
						which = "column"
					)
				)
			
			top_annotation = as.list(top_annotation_args)
		}
	}
	
	# Return
	list( left_annotation = left_annotation, row_split = row_split, top_annotation = top_annotation, col_split = col_split )
}

get_grouping_columns_OLD = function(.data){
	
	# Comply with CRAN NOTES
	.rows = NULL
	
	if("groups" %in%  (.data %>% attributes %>% names))
		.data %>% attr("groups") %>% select(-.rows) %>% colnames()
	else c()
}

annot_to_list_OLD = function(.data){
	
	# Comply with CRAN NOTES
	col_name = NULL
	annot = NULL
	
	.data %>% pull(annot) %>% set_names(.data %>% pull(col_name))  %>%
		
		# If list is populated
		when(length(.) > 0 ~ (.) %>% c(
			col = list(.data %>%
								 	filter(map_lgl(color, ~ .x %>% is.null %>% `!`)) %>%
								 	{ set_names( pull(., color),  pull(., col_name))    })
		), ~ (.))
	
}

#' DEPRECATED. Adds a bar annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_bar() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom grid unit 
#' 
#'
#' @name add_bar
#' @rdname add_bar-method
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to top_annotation or left_annotation of the ComplexHeatmap container
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' print("DEPRECATED") 
#'
#'
#' @noRd
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("add_bar", function(.data,
															 .column,
															 palette = NULL, size = NULL,...)
	standardGeneric("add_bar"))

#' DEPRECATED. Adds a tile annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_tile() from a `InputHeatmap` object, adds a tile annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom grid unit 
#'
#' @name add_tile
#' @rdname add_tile-method
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to top_annotation or left_annotation of the ComplexHeatmap container
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' print("DEPRECATED") 
#'
#' @noRd
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("add_tile", function(.data,
																.column,
																palette = NULL, size = NULL, ...)
	standardGeneric("add_tile"))

#' DEPRECATED. add_tile
#' 
#' @docType methods
#' @rdname add_tile-method
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("add_tile", "InputHeatmap", function(.data,
																							 .column,
																							 palette = NULL, size = NULL,...){
	
	deprecate_warn("1.9.0", "tidyHeatmap::add_tile()", details = "Please use `annotation_tile()` instead")
	
	.column = enquo(.column)
	
	annotation_tile(.data = .data, .column = !!.column, palette = palette, size = size, ...)
	
})

#' DEPRECATED. Adds a point annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_point() from a `InputHeatmap` object, adds a point annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom grid unit 
#' 
#'
#' @name add_point
#' @rdname add_point-method
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to top_annotation or left_annotation of the ComplexHeatmap container
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' print("DEPRECATED") 
#'
#'
#' @noRd
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("add_point", function(.data,
																 .column,
																 palette = NULL, size = NULL,...)
	standardGeneric("add_point"))

#' DEPRECATED. add_point
#' 
#' @docType methods
#' @rdname add_point-method
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("add_point", "InputHeatmap", function(.data,
																								.column,
																								palette = NULL, size = NULL,...){
	
	.column = enquo(.column)
	
	deprecate_warn("1.9.0", "tidyHeatmap::add_point()", details = "Please use `annotation_point()` instead")
	
	annotation_point(.data = .data, .column = !!.column, palette = palette, size = size, ...)
	
})

#' DEPRECATED. Adds a line annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_line() from a `InputHeatmap` object, adds a line annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom grid unit 
#' 
#'
#' @name add_line
#' @rdname add_line-method
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to top_annotation or left_annotation of the ComplexHeatmap container
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' print("DEPRECATED") 
#'
#'
#' @noRd
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("add_line", function(.data,
																.column,
																palette = NULL,size = NULL, ...)
	standardGeneric("add_line"))

#' DEPRECATED. add_line
#' 
#' @docType methods
#' @rdname add_line-method
#' 
#'
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("add_line", "InputHeatmap", function(.data,
																							 .column,
																							 palette = NULL, size = NULL,...){
	
	.column = enquo(.column)
	
	deprecate_warn("1.9.0", "tidyHeatmap::add_line()", details = "Please use `annotation_line()` instead")
	
	annotation_line(.data = .data, .column = !!.column, palette = palette, size = size, ...)
	
})

#' DEPRECATED. Adds a bar annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_bar() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom grid unit 
#' 
#'
#' @name add_bar
#' @rdname add_bar-method
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to top_annotation or left_annotation of the ComplexHeatmap container
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' print("DEPRECATED") 
#'
#'
#' @noRd
setGeneric("add_bar", function(.data,
																			.column,
																			palette = NULL, size = NULL,...)
	standardGeneric("add_bar"))

#' DEPRECATED. add_bar
#' 
#' @docType methods
#' @rdname add_bar-method
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("add_bar", "InputHeatmap", function(.data,
																							.column,
																							palette = NULL, size = NULL,...){
	
	deprecate_warn("1.9.0", "tidyHeatmap::add_bar()", details = "Please use `annotation_bar()` instead")
	
	.column = enquo(.column)
	
	annotation_bar(.data = .data, .column = !!.column, palette = palette, size = size, ...)
	
})