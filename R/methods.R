#the class definition
InputHeatmap<-setClass(
	"InputHeatmap",  
	slots = list(
		input = "list", 
		data = "tbl",
		palette_discrete = "list", 
		palette_continuous = "list",
		group_top_annotation = "list",
		group_left_annotation = "list",
		top_annotation = "tbl",
		left_annotation = "tbl",
		arguments = "list" ,
		layer_symbol = "tbl"
	),
	prototype=list(
		palette_discrete=
			list(
				brewer.pal(9, "Set1"),
				brewer.pal(8, "Set2"),
				brewer.pal(12, "Set3"),
				brewer.pal(8, "Dark2"),
				brewer.pal(8, "Accent"),
				brewer.pal(8, "Pastel2")
			), 
		palette_continuous=
			list(
				brewer.pal(11, "Spectral") %>% rev,
				viridis(n = 5),
				magma(n = 5),
				brewer.pal(11, "PRGn"),
				brewer.pal(11, "BrBG")
			),
		input = list(),
		top_annotation =  tibble(col_name = character(), orientation = character(), col_orientation = character(), data = list(),      fx = list(),    annot = list(),     annot_type= character(),   idx = integer(), color = list()),
		left_annotation = tibble(col_name = character(), orientation = character(), col_orientation = character(), data = list(),      fx = list(),    annot = list(),     annot_type= character(),   idx = integer(), color = list()),
		group_top_annotation = list(),
		group_left_annotation = list(),
		layer_symbol = tibble(column = integer(), row = integer(), shape = integer())
	)
)

#' @importFrom methods show
#' @importFrom tibble rowid_to_column
#' @importFrom grid grid.points
setMethod("show", "InputHeatmap", function(object){
	
	# Fix CRAN notes
	. = NULL
	index_column_wise = NULL
	shape = NULL
	
	object@input$top_annotation = 
		c(
			object@group_top_annotation,
			object@top_annotation %>% annot_to_list()
		) %>%
		list_drop_null() %>%
		when(
			
			# is.null needed for check Windows CRAN servers
			(.) %>% length %>% gt(0) && !is.null(.) ~ do.call("columnAnnotation", . ),
			~ NULL
		)
	
	object@input$left_annotation = 
		c(
			object@group_left_annotation,
			object@left_annotation %>% annot_to_list()
		) %>%
		list_drop_null()  %>%
		when(
			
			# is.null needed for check Windows CRAN servers
			(.) %>% length %>% gt(0) && !is.null(.)	~ do.call("rowAnnotation", . ),
			~ NULL
		)
	
	# On-top layer
	object@input$layer_fun = function(j, i, x, y, w, h, fill) {
		ind = 
			tibble(row = i, column = j) %>%
			rowid_to_column("index_column_wise") %>%
			
			# Filter just points to label
			inner_join(object@layer_symbol, by = c("row", "column")) %>%
			select(`index_column_wise`, `shape`)
		
		if(nrow(ind)>0)
			grid.points(
				x[ind$index_column_wise], y[ind$index_column_wise], 
				pch = ind$shape , 
				size = unit(3, "mm"), 
				gp = gpar(col = NULL, fill="#161616")
			)
	}
	
	

					

	
	
	show(do.call(Heatmap, object@input))
} )

#' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description heatmap() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom lifecycle is_present
#' @importFrom lifecycle deprecate_warn
#' 
#'
#' @name heatmap
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .row The name of the column vertically presented in the heatmap
#' @param .column The name of the column horizontally presented in the heatmap
#' @param .value The name of the column for the value of the element/feature pair
#' @param transform A function, used to transform .value row-wise (e.g., transform = log1p)
#' @param .scale A character string. Possible values are c(\"none\", \"row\", \"column\", \"both\")
#' @param palette_value A character vector This is the palette that will be used as gradient for .value. For example c("red", "white", "blue"). For higher flexibility you can use circlize::colorRamp2\(c\(-2, -1, 0, 1, 2\), viridis::magma\(5\)\)
#' @param palette_grouping A list of character vectors. This is the list of palettes that will be used for grouping. For example list(RColorBrewer::brewer.pal(8, "Accent")) or list(c("#B3E2CD", "#FDCDAC", "#CBD5E8")) or list(c("black", "red")) 
#' @param ... Further arguments to be passed to ComplexHeatmap::Heatmap
#' 
#' @param annotation DEPRECATED. please use the annotation functions add_* function \(\* one of tile, point, bar, line  \).
#' @param type DEPRECATED. please use the annotation functions add_* function \(\* one of tile, point, bar, line  \).
#' @param palette_discrete DEPRECATED. please use the annotation functions add_* function \(\* one of tile, point, bar, line  \).
#' @param palette_continuous DEPRECATED. please use the annotation functions add_* function \(\* one of tile, point, bar, line  \).
#'
#' @details This function takes a tbl as an input and creates a `ComplexHeatmap` plot. The information is stored in a `InputHeatmap` object that is updated along the pipe statement, for example adding annotation layers. 
#'
#' @return A `InputHeatmap` objects that gets evaluated to a `ComplexHeatmap` object
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' tidyHeatmap::N52 %>%
#' group_by( `Cell type`) %>%
#' tidyHeatmap::heatmap(
#'  .row = symbol_ct,
#'  .column = UBR,
#'  .value = `read count normalised log`,
#' )
#'
#' @docType methods
#' @rdname heatmap-methods
#'
#' @export
setGeneric("heatmap", function(.data,
															 .row, 
															 .column,
															 .value,
															 transform = NULL,
															 .scale = "row",
															 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
															 palette_grouping = list(),
															 
															 # DEPRECATED
															 annotation = NULL,
															 type = rep("tile", length(quo_names(annotation))),
															 palette_discrete = list(),
															 palette_continuous = list(),
															 ...) standardGeneric("heatmap"))

#' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
#' @inheritParams heatmap
#' 
#' @docType methods
#' @rdname heatmap-methods
#' 
#' @return A `InputHeatmap` object
#' 
heatmap_ <-
	function(.data,
					 .row, 
					 .column,
					 .value,
					 transform = NULL,
					 .scale = "row",
					 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
					 palette_grouping = list(),
					 
					 # DEPRECATED
					 annotation = NULL,
					 type = rep("tile", length(quo_names(annotation))),
					 palette_discrete = list(),
					 palette_continuous = list(),
					 ...)
	{
		# Comply with CRAN NOTES
		. = NULL
		
		# Make col names
		annotation = enquo(annotation)
		
		# Check if transform is of correct type
		if(!(is.null(transform) || is_function(transform))) stop("tidyHeatmap says: transform has to be a function. is_function(transform) == TRUE")
		
		# Check if .scale is of correct type
		if(.scale %in% c("none", "row", "column", "both") %>% `!`) stop("tidyHeatmap says: the .scale parameter has to be one of c(\"none\", \"row\", \"column\", \"both\")")

		# Check if type is of the right kind
		if(type %>% setdiff(names(type_to_annot_function)) %>% length %>% gt(0))
			stop("tidyHeatmap says: not all components of `type` parameter are valid.")
		
		# Message about change of style, once per session
		if(length(palette_grouping)==0 & getOption("tidyHeatmap_white_group_message",TRUE)) {
			message("tidyHeatmap says: (once per session) from release 1.2.3 the grouping labels have white background by default. To add color for one-ay grouping specify palette_grouping = list(c(\"red\", \"blue\"))")
			options("tidyHeatmap_white_group_message"=FALSE) 
		}
		
		.row = enquo(.row)
		.column = enquo(.column)
		.value <- enquo(.value)

		# Validation
		.data %>% validation(!!.column, !!.row, !!.value)
		
		# DEPRECATION OF ANNOTATION
		if (is_present(annotation) & !quo_is_null(annotation)) {
			
			# Signal the deprecation to the user
			deprecate_warn("1.1.0", "tidyHeatmap::heatmap(annotation = )", details = "Please use the new annotation framework instead: heatmap(...) %>% add_tile(...) %>% add_point(...) %>% add_bar() %>% add_line() %>% ...")
			
			# Deal with the deprecated argument for compatibility
			return(		.data %>%
									plot_heatmap(
										.horizontal = !!.column,
										.vertical = !!.row,
										.abundance = !!.value,
										annotation = !!annotation,
										type = type,
										transform = transform,
										.scale = .scale,
										palette_value = palette_value,
										palette_discrete = palette_discrete,
										palette_continuous = palette_continuous,
										...
									))
		}

		
		.data %>% 
			
			# # Check if data is rectangular
			# ifelse_pipe(
			# 	!check_if_data_rectangular((.), !!.column, !!.row, !!.value),
			# 	~  eliminate_sparse_transcripts(.x, !!.row)
			# ) %>%
			
			# Run plotting function
			input_heatmap(
				.horizontal = !!.column,
				.vertical = !!.row,
				.abundance = !!.value,
				transform = transform,
				.scale = .scale,
				palette_value = palette_value,
				palette_grouping = palette_grouping,
				...
			)		%>%
			
			# Add group annotation if any
			when( "groups" %in%  (attributes(.data) %>% names) ~ 	add_grouping(.), ~ (.))
		
	}

#' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
#' 
#' @docType methods
#' @rdname heatmap-methods
#' 
#' @return A `InputHeatmap` object
#'
setMethod("heatmap", "tbl", heatmap_)

#' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
#' 
#' @docType methods
#' @rdname heatmap-methods
#' 
#' @return A `InputHeatmap` object
#'
setMethod("heatmap", "tbl_df", heatmap_)

# #' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
# #' @inheritParams heatmap
# #' 
# #' @docType methods
# #' @rdname heatmap-methods
# #' 
# #' @return A `InputHeatmap` object
# #'
# setMethod("heatmap", "tidybulk", heatmap_)

#' Adds a tile annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_tile() from a `InputHeatmap` object, adds a tile annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name add_tile
#' @rdname add_tile-methods
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors  This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% add_tile(CAPRA_TOTAL)
#'
#'
#' @export
setGeneric("add_tile", function(.data,
																.column,
																palette = NULL)
	standardGeneric("add_tile"))

#' add_tile
#' 
#' @docType methods
#' @rdname add_tile-methods
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("add_tile", "InputHeatmap", function(.data,
																							 .column,
																							 palette = NULL){
	
	.column = enquo(.column)
	
	.data %>% add_annotation(
		!!.column,
		type = "tile",
		
		# If annotation is discrete
		palette_discrete = 
			.data@data %>% 
			ungroup() %>%
			select(!!.column) %>% 
			sapply(class) %>% 
			when(. %in% c("factor", "character", "logical") &	!is.null(palette) ~ list(palette), ~ list()),
		
		# If annotation is continuous
		palette_continuous = 
			.data@data %>% 
			ungroup() %>%
			select(!!.column) %>% 
			sapply(class) %>% 
			when(. %in% c("integer", "numerical", "numeric", "double") &	!is.null(palette) ~ list(palette), ~ list())
	)
	
})

#' Adds a point annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_point() from a `InputHeatmap` object, adds a point annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name add_point
#' @rdname add_point-methods
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors  This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% add_point()
#'
#'
#' @export
setGeneric("add_point", function(.data,
																.column,
																palette = NULL)
	standardGeneric("add_point"))

#' add_point
#' 
#' @docType methods
#' @rdname add_point-methods
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("add_point", "InputHeatmap", function(.data,
																							 .column,
																							 palette = NULL){
	
	.column = enquo(.column)
	
	.data %>% add_annotation(	!!.column,	type = "point")
	
})

#' Adds a line annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_line() from a `InputHeatmap` object, adds a line annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name add_line
#' @rdname add_line
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors  This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% add_line()
#'
#'
#' @export
setGeneric("add_line", function(.data,
																 .column,
																 palette = NULL)
	standardGeneric("add_line"))

#' add_line
#' 
#' @docType methods
#' @rdname add_line-methods
#' 
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors  This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#'
#'
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("add_line", "InputHeatmap", function(.data,
																								.column,
																								palette = NULL){
	
	.column = enquo(.column)
	
	.data %>% add_annotation(	!!.column,	type = "line")
	
})

#' Adds a bar annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description add_bar() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name add_bar
#' @rdname add_bar
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors  This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% add_bar()
#'
#'
#' @export
setGeneric("add_bar", function(.data,
																.column,
																palette = NULL)
	standardGeneric("add_bar"))

#' add_bar
#' 
#' @docType methods
#' @rdname add_bar-methods
#' 
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colors  This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("add_bar", "InputHeatmap", function(.data,
																							 .column,
																							 palette = NULL){
	
	.column = enquo(.column)
	
	.data %>% add_annotation(	!!.column,	type = "bar")
	
})

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_symbol() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name layer_symbol
#' @rdname layer_symbol
#'
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#' @param symbol A character string of length one. The values allowed are "point" ,     "square" ,    "diamond" ,   "arrow_up" ,  "arrow_down"
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#' @keywords internal
#' @docType methods
#' 
#' @noRd
#' 
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% layer_symbol()
#'
#'
setGeneric("layer_symbol", function(.data,
																		...,
																		symbol = "point")
	standardGeneric("layer_symbol"))

#' layer_symbol
#' 
#' @docType methods
#' @rdname layer_symbol-methods
#' 
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#' @param symbol A character string of length one. The values allowed are "point" ,     "square" ,    "diamond" ,   "arrow_up" ,  "arrow_down"
#' 
#' @noRd
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_symbol", "InputHeatmap", function(.data,
																									 ...,
																									 symbol = "point"){
	
	.data_drame = .data@data
	
	
	symbol_dictionary = 
		list(
			point = 21,
			square = 22,
			diamond = 23,
			arrow_up = 24,
			arrow_down = 25
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
	
	# Append which cells have to be signed
	.data@layer_symbol= 
		.data@layer_symbol %>%
		bind_rows(
			.data_drame %>%
				droplevels() %>%
				mutate(
					column = !!.horizontal %>% as.factor %>% as.integer,
					row = !!.vertical %>% as.factor %>% as.integer
				) %>%
				filter(...) %>%
				select(column, row) %>%
				mutate(shape = symbol_dictionary[[symbol]])
		)
	
	.data

	
})

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_arrow_up() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name layer_arrow_up
#' @rdname layer-methods
#'
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% layer_arrow_up()
#'
#'
#' @export
setGeneric("layer_arrow_up", function(.data,	...)
	standardGeneric("layer_arrow_up"))

#' layer_arrow_up
#' 
#' @docType methods
#' @rdname layer-methods
#' 
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_arrow_up", "InputHeatmap", function(.data, ...){ .data %>%	layer_symbol(..., symbol="arrow_up") })

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_arrow_down() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name layer_arrow_down
#' @rdname layer-methods
#' 
#'
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% layer_arrow_down()
#'
#'
#' @export
setGeneric("layer_arrow_down", function(.data,	...)
	standardGeneric("layer_arrow_down"))

#' layer_arrow_down
#' 
#' @docType methods
#' @rdname layer-methods
#' 
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_arrow_down", "InputHeatmap", function(.data, ...){ .data %>%	layer_symbol(..., symbol="arrow_down") })

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_point() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name layer_point
#' @rdname layer-methods
#'
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% layer_point()
#'
#'
#' @export
setGeneric("layer_point", function(.data,	...)
	standardGeneric("layer_point"))

#' layer_point
#' 
#' @docType methods
#' @rdname layer-methods
#' 
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_point", "InputHeatmap", function(.data, ...){ .data %>%	layer_symbol(..., symbol="point") })

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_square() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name layer_square
#' @rdname layer-methods
#'
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% layer_square()
#'
#'
#' @export
setGeneric("layer_square", function(.data,	...)
	standardGeneric("layer_square"))

#' layer_square
#' 
#' @docType methods
#' @rdname layer-methods
#' 
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_square", "InputHeatmap", function(.data, ...){ .data %>%	layer_symbol(..., symbol="square") })

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_diamond() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#'
#' @name layer_diamond
#' @rdname layer-methods
#'
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% layer_diamond()
#'
#'
#' @export
setGeneric("layer_diamond", function(.data,	...)
	standardGeneric("layer_diamond"))

#' layer_diamond
#' 
#' @docType methods
#' @rdname layer-methods
#' 
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#'
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_diamond", "InputHeatmap", function(.data, ...){ .data %>%	layer_symbol(..., symbol="diamond") })

#' Split the heatmap row-wise depending on the biggest branches in the cladogram.
#'
#' \lifecycle{maturing}
#'
#' @description split_rows() from a `InputHeatmap` object, split the row cladogram.
#'
#' @importFrom stats hclust
#' @importFrom dendextend cutree
#' @importFrom purrr when
#' 
#'
#' @name split_rows
#' @rdname split-methods
#'
#' @param .data A `InputHeatmap` 
#' @param number_of_groups An integer. The number of groups to split the cladogram into.
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#' @docType methods
#' 
#' 
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% split_rows(2)
#'
#' @export
setGeneric("split_rows", function(.data,
																	number_of_groups)
	standardGeneric("split_rows"))

#' split_rows
#' 
#' @docType methods
#' @rdname split-methods
#' 
#' @param .data A `InputHeatmap` 
#' @param number_of_groups An integer. The number of groups to split the cladogram into.
#'
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("split_rows", "InputHeatmap", function(.data,
																								 number_of_groups){
	
	# Get the same methods as the heatmap
	distance_method = .data@input %>% when(
		"clustering_distance_rows" %in% names(.) ~ .data@input$clustering_distance_rows,
		~ "euclidean"
	)
	clustering_method = .data@input %>% when(
		"clustering_method_rows" %in% names(.) ~ .data@input$clustering_method_rows,
		~ "complete"
	)
	
	# Get clusters
	hr = 
		.data@input[[1]] %>%
		dist(method = distance_method) %>%
		hclust(method = clustering_method)
	
	# Append to input
	.data@input$row_split = dendextend::cutree(hr, k = number_of_groups)
	
	.data
	
})

#' Split the heatmap column-wise depending on the biggest branches in the cladogram.
#'
#' \lifecycle{maturing}
#'
#' @description split_columns() from a `InputHeatmap` object, split the column cladogram.
#'
#' @importFrom stats hclust
#' @importFrom dendextend cutree
#' 
#'
#' @name split_columns
#' @rdname split-methods
#'
#' @param .data A `InputHeatmap` 
#' @param number_of_groups An integer. The number of groups to split the cladogram into.
#'
#'
#' @details It uses `ComplexHeatmap` as visualisation tool.
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
#' @docType methods
#' 
#' 
#' @examples
#'
#' library(dplyr)
#' 
#' hm = 
#'   tidyHeatmap::N52 %>%
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm %>% split_columns(2)
#'
#' @export
setGeneric("split_columns", function(.data,
																		 number_of_groups)
	standardGeneric("split_columns"))

#' split_columns
#' 
#' @docType methods
#' @rdname split-methods
#' 
#' @param .data A `InputHeatmap` 
#' @param number_of_groups An integer. The number of groups to split the cladogram into.
#'
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("split_columns", "InputHeatmap", function(.data,
																										number_of_groups){
	
	# Get the same methods as the heatmap
	distance_method = .data@input %>% when(
		"clustering_distance_columns" %in% names(.) ~ .data@input$clustering_distance_columns,
		~ "euclidean"
	)
	clustering_method = .data@input %>% when(
		"clustering_method_columns" %in% names(.) ~ .data@input$clustering_method_columns,
		~ "complete"
	)
	
	# Get clusters
	hr = 
		.data@input[[1]] %>%
		t() %>%
		dist(method = distance_method) %>%
		hclust(method = clustering_method)
	
	# Append to input
	.data@input$column_split = dendextend::cutree(hr, k = number_of_groups)
	
	.data
	
})

#' Save plot on PDF file
#'
#' \lifecycle{maturing}
#' 
#' @importFrom utils capture.output
#' @import  grDevices
#'
#' @description save_pdf() takes as input a Heatmap from ComplexHeatmap and save it to PDF file
#'
#'
#' @name save_pdf
#'
#' @param .heatmap A `Heatmap` 
#' @param filename A character string. The name of the output file/path
#' @param width A `double`. Plot width
#' @param height A `double`. Plot height
#' @param units	A character string. units ("in", "cm", or "mm")
#' 
#' @details It simply save an `Heatmap` to a PDF file use pdf() function in the back end
#'
#' @return NA
#'
#'
#' @examples
#' 
#' 
#' library(dplyr)
#' 	tidyHeatmap::heatmap(
#'   dplyr::group_by(tidyHeatmap::pasilla,		location, type),
#'   .column = sample,
#'   .row = symbol,
#'   .value = `count normalised adjusted`,
#'  ) %>%
#'  save_pdf(tempfile())
#'
#' 
#' @docType methods
#' @rdname save_pdf-methods
#' @export
#'
setGeneric("save_pdf", function(.heatmap,
																filename,
																width = NULL,
																height = NULL,
																units = c("in", "cm", "mm") )
	standardGeneric("save_pdf"))

.save_pdf = function(.heatmap,
										 filename,
										 width = NULL,
										 height = NULL,
										 units = c("in", "cm", "mm")){
	
	# Adapt to ggsave
	if(is.null(width)) width = NA
	if(is.null(height)) height = NA
	
	
	dev = plot_dev("pdf", filename)
	dim <- plot_dim(c(width, height), units = units)
	
	old_dev <- dev.cur()
	dev(filename = filename, width = dim[1], height = dim[2])
	on.exit(capture.output({
		dev.off()
		if (old_dev > 1) dev.set(old_dev) # restore old device unless null device
	}))
	print(.heatmap)
	
	invisible()
	
}

#' save_pdf
#' 
#' @param .heatmap A `Heatmap` 
#' @param filename A character string. The name of the output file/path
#' @param width A `double`. Plot width
#' @param height A `double`. Plot height
#' @param units	A character string. units ("in", "cm", or "mm")
#' 
#' 
setMethod("save_pdf", "Heatmap", .save_pdf)

#' save_pdf
#' 
#' @param .heatmap A `Heatmap` 
#' @param filename A character string. The name of the output file/path
#' @param width A `double`. Plot width
#' @param height A `double`. Plot height
#' @param units	A character string. units ("in", "cm", or "mm")
#' 
#' 
setMethod("save_pdf", "InputHeatmap", .save_pdf)



