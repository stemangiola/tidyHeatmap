#' input_heatmap
#'
#' @description input_heatmap() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
#'
#' @import dplyr
#' @import tidyr
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
#' @importFrom tidyr pivot_wider
#'
#' @name input_heatmap
#' @rdname input_heatmap
#'
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .horizontal The name of the column horizontally presented in the heatmap
#' @param .vertical The name of the column vertically presented in the heatmap
#' @param .abundance The name of the transcript/gene abundance column
#' @param transform A function, used to transform .value, for example log1p
#' @param scale A character string. Possible values are c(\"none\", \"row\", \"column\", \"both\")
#' @param palette_value A character vector, or a function for higher customisation (colorRamp2). This is the palette that will be used as gradient for abundance. If palette_value is a vector of hexadecimal colours, it should have 3 values. If you want more customisation, you can pass to palette_value a function, that is derived as for example `colorRamp2(c(-2, 0, 2), palette_value)`
#' @param palette_grouping A list of character vectors. This is the list of palettes that will be used for grouping  
#' @param ... Further arguments to be passed to ComplexHeatmap::Heatmap
#'
#' @details To be added.
#'
#' @return A `ComplexHeatmap` object
#'
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
input_heatmap = function(.data,
												.horizontal,
												.vertical,
												.abundance,
												transform = NULL,
												scale = "none",
												palette_value = c("#440154FF", "#21908CFF", "#fefada" ), #c(viridis(3)[1:2],"#fefada")
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
	if(.data %>% filter(!!.abundance %>% is.na %>% not) %>% nrow %>% equals(0))
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
					filter(., !!.abundance %>% is.nan) %>% nrow %>% gt(0) ~ stop("tidyHeatmap says: you applied a transformation that introduced NaN."),
					
					# -Inf produced
					pull(., !!.abundance) %>% min %>% equals(-Inf) ~ stop("tidyHeatmap says: you applied a transformation that introduced negative infinite .value, was it log? If so please use log1p."),
					~(.)
				),
			~ (.)
		) %>%
		
		# If scale row
		when(
			scale %in% c("row", "both") ~ (.) %>%
				nest(data = -!!.vertical) %>%
				mutate(data = map(data, ~ .x %>% mutate(!!.abundance := !!.abundance %>% scale_robust()))) %>%
				unnest(data),
			~ (.)
		) %>%
		
		# If scale column
		when(
			scale %in% c("column", "both") ~ (.) %>%
				nest(data = -!!.horizontal) %>%
				mutate(data = map(data, ~ .x %>% mutate(!!.abundance := !!.abundance %>% scale_robust()))) %>%
				unnest(data),
			~ (.)
		) %>%
		
		distinct(!!.vertical,!!.horizontal,!!.abundance) %>%
	  
	  # Arrange both columns and rows
	  # do not leave the order of appearence dictate the order of columns and rows
	  pivot_wider(names_from =  !!.horizontal, values_from =  !!.abundance, names_sort = TRUE) |> 
	  arrange(!!.vertical)
	
	abundance_mat =
		abundance_tbl %>%
		as_matrix(rownames = quo_name(.vertical)) 
	 
	# Colors tiles
	# If palette_value is a function pass it directly, otherwise check if the character array is of length 3
	colors = 
		palette_value %>%
		when(
			palette_value %>% class() %>% equals("function") ~ (.),
			length(palette_value) != 3 ~ stop("tidyHeatmap says: If palette_value is a vector of hexadecimal colours, it should have 3 values. If you want more customisation, you can pass to palette_value a function, that is derived as for example \"colorRamp2(c(-2, 0, 2), palette_value)\""	),
			
			# For the crazy scenario when only one value is present in the heatmap (tidyHeatmap/issues/40)
			min(abundance_mat, na.rm = T) == max(abundance_mat, na.rm = T) ~ colorRamp2(
				
				# min and max and intermediates based on length of the palette
				seq(from=min(abundance_mat, na.rm = T)-1, to=max(abundance_mat, na.rm = T)+1, length.out = length(palette_value)),
				palette_value
			),
			
			# In the normal situation
			~ colorRamp2(
				
				# min and max and intermediates based on length of the palette
				seq(from=min(abundance_mat, na.rm = T), to=max(abundance_mat, na.rm = T), length.out = length(palette_value)),
				palette_value
			)
		)
	
	# Define object
	new(
		"InputHeatmap",
		data = .data %>% reduce_to_tbl_if_in_class_chain,
		# Due to the `.homonyms="last"` parameter, additional arguments passed by the user
		# via `...` overwrite the defaults given below (See also `?rlang::dots_list`)
		input = rlang::dots_list(
			abundance_mat,
			name = quo_name(.abundance),
			column_title = quo_name(.horizontal),
			row_title = quo_name(.vertical),
			col = colors,
			row_names_gp = gpar(fontsize = min(12, 320 / dim(abundance_mat)[1])),
			column_names_gp = gpar(fontsize = min(12, 320 / dim(abundance_mat)[2])),
			...,
			.homonyms="last"
		),
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
	
	# Number of groups
	how_many_groups = my_input_heatmap@data %>% attr("groups") %>% nrow
	
	# Number of grouping
	how_many_grouping = my_input_heatmap@data %>% attr("groups") %>% select(-.rows) %>% ncol
	
	# Add custom palette to discrete if any
	my_input_heatmap@palette_discrete =
		my_input_heatmap@arguments$palette_grouping %>%
		when(
			length(.) < how_many_grouping ~ {
				# Needed for piping
				pg = .
				
				my_input_heatmap@arguments$palette_grouping %>%
					c(
						rep("#ffffff", how_many_groups) %>%
							list() %>%
							rep(how_many_grouping-length(pg))
					)
			},
			~ (.)
		) %>%
		c(my_input_heatmap@palette_discrete)
	
	# Colours annotations
	palette_annotation = my_input_heatmap@palette_discrete %>% head(how_many_grouping) 
	
	# Take away used palettes
	my_input_heatmap@palette_discrete = my_input_heatmap@palette_discrete %>% tail(-how_many_grouping)
	
	# See if I have grouping and setup framework
	group_annotation = 
	  my_input_heatmap@data |>
	  get_group_annotation(
		!!.horizontal,
		!!.vertical,
		!!.abundance,
		palette_annotation,
		group_label_fontsize = my_input_heatmap@arguments$group_label_fontsize %||% 8,
		show_group_name = my_input_heatmap@arguments$show_group_name %||% TRUE,
		group_strip_height = my_input_heatmap@arguments$group_strip_height %||% unit(9, "pt")
	)
	
	# Isolate top annotation
	my_input_heatmap@group_top_annotation = group_annotation$top_annotation 
	
	# Isolate left annotation
	my_input_heatmap@group_left_annotation = group_annotation$left_annotation 
	
	my_input_heatmap@input  =
		my_input_heatmap@input %>% 
		when(
			!is.null(group_annotation$row_split) ~ c(., list(row_split = group_annotation$row_split, cluster_row_slices = FALSE)),
			~ (.)
		) %>%
		when(
			!is.null(group_annotation$col_split) ~ c(., list(column_split = group_annotation$col_split, cluster_column_slices = FALSE)),
			~ (.)
		)
	
	my_input_heatmap
}


#' add_annotation
#'
#' @description add_annotation() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
#'
#' @import dplyr
#' @import tidyr
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
#' @importFrom grid unit
#'
#' @name add_annotation
#' @rdname add_annotation
#'
#' @param my_input_heatmap A `InputHeatmap` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param annotation Vector of quotes
#' @param type A character vector of the set c(\"tile\", \"point\", \"bar\", \"line\")
#' @param palette_discrete A list of character vectors. This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#' @param palette_continuous A list of character vectors. This is the list of palettes that will be used for horizontal and vertical continuous annotations. The continuous classification of annotations depends on the column type of your input tibble (e.g., integer, numerical, double).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to top_annotation or left_annotation of the ComplexHeatmap container
#'
#' @details To be added.
#'
#' @return A `ComplexHeatmap` object
#'
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
add_annotation = function(my_input_heatmap,
												 annotation,
												 type = rep("tile", length(quo_names(annotation))),
												 palette_discrete = list(),
												 palette_continuous = list(),  size = NULL, ...) {
	
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
														 !!annotation,	palette_annotation,	type, x_y_annot_cols, size, ...)
	
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

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_symbol() from a `InputHeatmap` object, adds a symbol annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#' 
#'
#' @name layer_symbol
#' @rdname layer_symbol-method
#'
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#' @param symbol A character string of length one. The values allowed are "point" ,     "square" ,    "diamond" ,   "arrow_up" ,  "arrow_down",  "star",  "asterisk"
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#' @docType methods
#' 
#' @keywords internal
#' @noRd
#' 
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> layer_symbol()
#'
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("layer_symbol", function(.data,
																		...,
																		symbol = "point",
																		.size = NULL)
	standardGeneric("layer_symbol"))

#' layer_symbol
#' 
#' @docType methods
#' @rdname layer_symbol-method
#' 
#' @keywords internal
#' @noRd
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_symbol", "InputHeatmap", function(.data,
																									 ...,
																									 symbol = "point",
																									 .size = NULL){
	
	.data_drame = .data@data
	.size = enquo(.size)
	
	symbol_dictionary = 
		list(
			point = 21,
			square = 22,
			diamond = 23,
			arrow_up = 24,
			arrow_down = 25,
			star = 8,
			asterisk = 42
		)
	
	if(!symbol %in% names(symbol_dictionary) | length(symbol) != 1) 
		stop(sprintf("tidyHeatmap says: the symbol argument must be one character string, among %s", paste(names(symbol_dictionary))))
	
	# Comply with CRAN NOTES
	. = NULL
	column = NULL
	row = NULL
	
	# Make col names
	# Column names
	.horizontal = .data@arguments$.horizontal
	.vertical = .data@arguments$.vertical
	.abundance = .data@arguments$.abundance
	
	# Extract the abundance matrix for dimensions of the text
	abundance_mat = .data@input[[1]]
	
	# Append which cells have to be signed
	.data@layer_symbol= 
		.data@layer_symbol |>
		bind_rows(
			.data_drame |>
				droplevels() |>
				mutate(
					column = !!.horizontal %>%  as.factor()  %>%  as.integer(),
					row = !!.vertical  %>%  as.factor() %>% as.integer()
				) |>
				filter(...) |>
				mutate(shape = symbol_dictionary[[!!symbol]]) |> 
				
				# Add size
				when(
					quo_is_null(.size) ~ mutate(., size = min(3, 80 / max(dim(abundance_mat)) )) ,
					~ mutate(., size := !!.size )
				) |> 
				
				select(column, row, shape, size) 
		)
	
	.data
	
	
})

