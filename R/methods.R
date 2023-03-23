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
		layer_symbol = "tbl",
		layer_text = "tbl"
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
				brewer.pal(11, "Spectral") |> rev(),
				viridis(n = 5),
				magma(n = 5),
				brewer.pal(11, "PRGn"),
				brewer.pal(11, "BrBG")
			),
		input = list(),
		top_annotation =  tibble(col_name = character(), orientation = character(), col_orientation = character(), data = list(),      fx = list(),    annot = list(),     annot_type= character(),   idx = integer(), color = list(), further_arguments = list()),
		left_annotation = tibble(col_name = character(), orientation = character(), col_orientation = character(), data = list(),      fx = list(),    annot = list(),     annot_type= character(),   idx = integer(), color = list(), further_arguments = list()),
		group_top_annotation = list(),
		group_left_annotation = list(),
		layer_symbol = tibble(column = integer(), row = integer(), shape = integer()),
		layer_text = tibble(column = integer(), row = integer(), text = character(), size = numeric())
		
	)
)


#' Creates a  `ComplexHeatmap` object for less standard plot manipulation (e.g. changing legend position)
#'
#' \lifecycle{maturing}
#'
#' @description as_ComplexHeatmap() takes a `InputHeatmap` object and produces a `Heatmap` object
#'
#' @importFrom methods show
#' @importFrom tibble rowid_to_column
#' @importFrom grid grid.points grid.text
#' 
#'
#' @name as_ComplexHeatmap
#'
#' @param tidyHeatmap A `InputHeatmap` object from tidyHeatmap::heatmap() call
#' 
#' @return A `ComplexHeatmap` 
#'
#'
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
#' as_ComplexHeatmap()
#'
#' @docType methods
#' @rdname as_ComplexHeatmap-method
#'
#' @export
#' 
setGeneric("as_ComplexHeatmap", function(tidyHeatmap) standardGeneric("as_ComplexHeatmap"))


#' Creates a  `ComplexHeatmap` object for less standard plot manipulation (e.g. changing legend position)
#'
#' @importFrom ComplexHeatmap columnAnnotation
#' @importFrom ComplexHeatmap rowAnnotation
#'
#' @docType methods
#' @rdname as_ComplexHeatmap-method
#'
#' @export
#' 
setMethod("as_ComplexHeatmap", "InputHeatmap", function(tidyHeatmap){
	
	# Fix CRAN notes
	. = NULL
	index_column_wise = NULL
	shape = NULL
	
	tidyHeatmap@input$top_annotation = 
		c(
			tidyHeatmap@group_top_annotation,
			tidyHeatmap@top_annotation |> annot_to_list()
		) |>
		list_drop_null() |>
		when(
			
			# is.null needed for check Windows CRAN servers
			length(.) |> gt(0) && !is.null(.) ~ do.call("columnAnnotation", . ),
			~ NULL
		)
	
	tidyHeatmap@input$left_annotation = 
		c(
			tidyHeatmap@group_left_annotation,
			tidyHeatmap@left_annotation |> annot_to_list()
		) |>
		list_drop_null()  |>
		when(
			
			# is.null needed for check Windows CRAN servers
			length(.) |> gt(0) && !is.null(.)	~ do.call("rowAnnotation", . ),
			~ NULL
		)
	
	# On-top layer
	tidyHeatmap@input$layer_fun = function(j, i, x, y, w, h, fill) {
		
		
		# Add symbol
		ind = 
			tibble(row = i, column = j) |>
			rowid_to_column("index_column_wise") |>
			
			# Filter just points to label
			inner_join(tidyHeatmap@layer_symbol, by = c("row", "column"))
		
		# Return graphical elements
		if(nrow(ind)>0){
			grid.points(
				x[ind$index_column_wise], y[ind$index_column_wise], 
				pch = ind$shape , 
				size = unit(3, "mm"), 
				gp = gpar(col = NULL, fill="#161616")
			)
		}
		
		# Add text
		ind_text =
			tibble(row = i, column = j) |>
			rowid_to_column("index_column_wise") |>
			
			# Filter just points to label
			inner_join(tidyHeatmap@layer_text, by = c("row", "column")) 
		
		# Return graphical elements
		if(nrow(ind_text) > 0){
			grid.text(
				ind_text$text,
				x[ind_text$index_column_wise],
				y[ind_text$index_column_wise],
				gp = gpar(fontsize = ind_text$size, col = "#000000")
			)
		}
		
	}
	
	return(do.call(Heatmap, tidyHeatmap@input))
})

setMethod("show", "InputHeatmap", function(object){
	
	object |>
		as_ComplexHeatmap() |>
		show()
})

#' @rdname plot_arithmetic
#' @export
"+.InputHeatmap" <- function(e1, e2) {
	
	as_ComplexHeatmap(e1) + as_ComplexHeatmap(e2)
}

#' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description heatmap() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
#'
#' @importFrom ComplexHeatmap Heatmap
#' @importFrom rlang enquo
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
#' @param scale A character string. Possible values are c(\"none\", \"row\", \"column\", \"both\")
#' @param palette_value A character vector This is the palette that will be used as gradient for .value. For example c("red", "white", "blue"). For higher flexibility you can use circlize::colorRamp2\(c\(-2, -1, 0, 1, 2\), viridis::magma\(5\)\)
#' @param palette_grouping A list of character vectors. This is the list of palettes that will be used for grouping. For example list(RColorBrewer::brewer.pal(8, "Accent")) or list(c("#B3E2CD", "#FDCDAC", "#CBD5E8")) or list(c("black", "red")) 
#' 
#' @param .scale DEPRECATED. please use scale instead \( with no dot prefix \).
#' @param ... The arguments that will be passed to the Heatmap function of ComplexHeatmap backend
#' 
#' @details This function takes a tbl as an input and creates a `ComplexHeatmap` plot. The information is stored in a `InputHeatmap` object that is updated along the pipe statement, for example adding annotation layers. 
#'
#' @return A `InputHeatmap` objects that gets evaluated to a `ComplexHeatmap` object
#'
#'
#'
#' @examples
#'
#' 
#' tidyHeatmap::N52 |>
#'   dplyr::group_by( `Cell type`) |>
#'   tidyHeatmap::heatmap(
#'    .row = symbol_ct,
#'    .column = UBR,
#'    .value = `read count normalised log`,
#'   )
#'
#' @docType methods
#' @rdname heatmap-method
#'
#' @export
setGeneric("heatmap", function(.data,
															 .row, 
															 .column,
															 .value,
															 transform = NULL,
															 scale = "none",
															 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
															 palette_grouping = list(),
															 
															 # DEPRECATED 
															 .scale = NULL,
															 ...) standardGeneric("heatmap"))

#' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
#' @inheritParams heatmap
#' 
#' @docType methods
#' @rdname heatmap-method
#' 
#' @return A `InputHeatmap` object
#' 
heatmap_ <-
	function(.data,
					 .row, 
					 .column,
					 .value,
					 transform = NULL,
					 scale = "none",
					 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
					 palette_grouping = list(),
					 
					 # DEPRECATED
					 .scale = NULL,
					 ...)
	{
		# Comply with CRAN NOTES
		. = NULL
		
		# Check if transform is of correct type
		if(!(is.null(transform) || is_function(transform))) stop("tidyHeatmap says: transform has to be a function. is_function(transform) == TRUE")
		
		# Check if scale is of correct type
		if(scale %in% c("none", "row", "column", "both") |> not()) stop("tidyHeatmap says: the scale parameter has to be one of c(\"none\", \"row\", \"column\", \"both\")")
		
		# # Message about change of style, once per session
		# if(length(palette_grouping)==0 & getOption("tidyHeatmap_white_group_message",TRUE)) {
		# 	message("tidyHeatmap says: (once per session) from release 1.2.3 the grouping labels have white background by default. To add color for one-ay grouping specify palette_grouping = list(c(\"red\", \"blue\"))")
		# 	options("tidyHeatmap_white_group_message"=FALSE) 
		# }
		
		# Message about change of scale, once per session
		if(scale == "none" & getOption("tidyHeatmap_default_scaling_none",TRUE)) {
			message("tidyHeatmap says: (once per session) from release 1.7.0 the scaling is set to \"none\" by default. Please use scale = \"row\", \"column\" or \"both\" to apply scaling")
			options("tidyHeatmap_default_scaling_none"=FALSE) 
		}
		
		.row = enquo(.row)
		.column = enquo(.column)
		.value <- enquo(.value)
		
		# Validation
		.data |> validation(!!.column, !!.row, !!.value)
		
		# DEPRECATION OF SCALE
		if (is_present(.scale) && !is.null(.scale)) {
			
			# Signal the deprecation to the user
			deprecate_warn("1.7.0", "tidyHeatmap::heatmap(.scale = )", details = "Please use scale (without dot prefix) instead: heatmap(scale = ...)")
			
			scale = .scale
			
		}
		
		.data |> 
			
			# # Check if data is rectangular
			# ifelse_pipe(
			# 	!check_if_data_rectangular((.), !!.column, !!.row, !!.value),
			# 	~  eliminate_sparse_transcripts(.x, !!.row)
			# ) |>
			
			# Run plotting function
			input_heatmap(
				.horizontal = !!.column,
				.vertical = !!.row,
				.abundance = !!.value,
				transform = transform,
				scale = scale,
				palette_value = palette_value,
				palette_grouping = palette_grouping,
				...
			)		|>
			
			# Add group annotation if any
			when( "groups" %in%  (attributes(.data) |> names()) ~ 	add_grouping(.), ~ (.))
		
	}

#' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
#' 
#' @docType methods
#' @rdname heatmap-method
#' 
#' @return A `InputHeatmap` object
#'
setMethod("heatmap", "tbl", heatmap_)

#' Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`
#' 
#' @docType methods
#' @rdname heatmap-method
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
#' @description annotation_tile() from a `InputHeatmap` object, adds a tile annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom grid unit 
#'
#' @name annotation_tile
#' @rdname annotation_tile-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> annotation_tile(CAPRA_TOTAL)
#'
#'
#' hm |> 
#'   annotation_tile(
#'     inflection, 
#'     palette = circlize::colorRamp2(c(0, 3,10), c("white", "green", "red"))
#'  )
#'
#' @export
setGeneric("annotation_tile", function(.data,
																			 .column,
																			 palette = NULL, size = NULL, ...)
	standardGeneric("annotation_tile"))

#' annotation_tile
#' 
#' @docType methods
#' @rdname annotation_tile-method
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("annotation_tile", "InputHeatmap", function(.data,
																											.column,
																											palette = NULL, size = NULL,...){
	
	.column = enquo(.column)
	
	.data |> add_annotation(
		!!.column,
		type = "tile",
		
		# If annotation is discrete
		palette_discrete = 
			.data@data |> 
			ungroup() |>
			select(!!.column) |> 
			sapply(class) |> 
			when(. %in% c("factor", "character", "logical") &	!is.null(palette) ~ list(palette), ~ list()),
		
		# If annotation is continuous
		palette_continuous = 
			.data@data |> 
			ungroup() |>
			select(!!.column) |> 
			sapply(class) |> 
			when(. %in% c("integer", "numerical", "numeric", "double") &	!is.null(palette) ~ list(palette), ~ list()),
		
		size = size,
		...
	)
	
})

#' Adds a point annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description annotation_point() from a `InputHeatmap` object, adds a point annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom grid unit 
#' 
#'
#' @name annotation_point
#' @rdname annotation_point-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> annotation_point(inflection)
#'
#'
#' @export
setGeneric("annotation_point", function(.data,
																				.column,
																				palette = NULL, size = NULL,...)
	standardGeneric("annotation_point"))

#' annotation_point
#' 
#' @docType methods
#' @rdname annotation_point-method
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("annotation_point", "InputHeatmap", function(.data,
																											 .column,
																											 palette = NULL, size = NULL,...){
	
	.column = enquo(.column)
	
	.data |> add_annotation(	!!.column,	type = "point", 		size = size,...)
	
})

#' Adds a line annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description annotation_line() from a `InputHeatmap` object, adds a line annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom grid unit 
#' 
#'
#' @name annotation_line
#' @rdname annotation_line-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> annotation_line(inflection)
#'
#'
#' @export
setGeneric("annotation_line", function(.data,
																			 .column,
																			 palette = NULL,size = NULL, ...)
	standardGeneric("annotation_line"))

#' annotation_line
#' 
#' @docType methods
#' @rdname annotation_line-method
#' 
#'
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("annotation_line", "InputHeatmap", function(.data,
																											.column,
																											palette = NULL, size = NULL,...){
	
	.column = enquo(.column)
	
	.data |> add_annotation(	!!.column,	type = "line", 		size = size,...)
	
})

#' Adds a bar annotation layer to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description annotation_bar() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom grid unit 
#' 
#'
#' @name annotation_bar
#' @rdname annotation_bar-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> annotation_bar(inflection)
#'
#'
#' @export
setGeneric("annotation_bar", function(.data,
																			.column,
																			palette = NULL, size = NULL,...)
	standardGeneric("annotation_bar"))

#' annotation_bar
#' 
#' @docType methods
#' @rdname annotation_bar-method
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("annotation_bar", "InputHeatmap", function(.data,
																										 .column,
																										 palette = NULL, size = NULL,...){
	
	.column = enquo(.column)
	
	.data |> add_annotation(	!!.column,	type = "bar", size = size,...)
	
})


#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_arrow_up() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' 
#'
#' @name layer_arrow_up
#' @rdname layer_arrow_up-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> layer_arrow_up()
#'
#'
#' @export
setGeneric("layer_arrow_up", function(.data,	...)
	standardGeneric("layer_arrow_up"))

#' layer_arrow_up
#' 
#' @docType methods
#' @rdname layer_arrow_up-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_arrow_up", "InputHeatmap", function(.data, ...){ .data |>	layer_symbol(..., symbol="arrow_up") })

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_arrow_down() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' 
#'
#' @name layer_arrow_down
#' @rdname layer_arrow_down-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> layer_arrow_down()
#'
#'
#' @export
setGeneric("layer_arrow_down", function(.data,	...)
	standardGeneric("layer_arrow_down"))

#' layer_arrow_down
#' 
#' @docType methods
#' @rdname layer_arrow_down-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_arrow_down", "InputHeatmap", function(.data, ...){ .data |>	layer_symbol(..., symbol="arrow_down") })

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_point() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' 
#'
#' @name layer_point
#' @rdname layer_point-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> layer_point()
#'
#'
#' @export
setGeneric("layer_point", function(.data,	...)
	standardGeneric("layer_point"))

#' layer_point
#' 
#' @docType methods
#' @rdname layer_point-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_point", "InputHeatmap", function(.data, ...){ .data |>	layer_symbol(..., symbol="point") })

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_square() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' 
#'
#' @name layer_square
#' @rdname layer_square-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> layer_square()
#'
#'
#' @export
setGeneric("layer_square", function(.data,	...)
	standardGeneric("layer_square"))

#' layer_square
#' 
#' @docType methods
#' @rdname layer_square-method
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_square", "InputHeatmap", function(.data, ...){ .data |>	layer_symbol(..., symbol="square") })

#' Adds a layers of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_diamond() from a `InputHeatmap` object, adds a bar annotation layer.
#'
#' @importFrom rlang enquo
#' 
#'
#' @name layer_diamond
#' @rdname layer_diamond-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> layer_diamond()
#'
#'
#' @export
setGeneric("layer_diamond", function(.data,	...)
	standardGeneric("layer_diamond"))

#' layer_diamond
#' 
#' @docType methods
#' @rdname layer_diamond-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_diamond", "InputHeatmap", function(.data, ...){ .data |>	layer_symbol(..., symbol="diamond") })

#' Adds a layer of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_star() from a `InputHeatmap` object, adds a symbol annotation layer over the heatmap tiles.
#'
#' @importFrom rlang enquo
#' 
#'
#' @name layer_star
#' @rdname layer_star-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> layer_star()
#'
#'
#' @export
setGeneric("layer_star", function(.data,	...)
	standardGeneric("layer_star"))

#' layer_star
#' 
#' @docType methods
#' @rdname layer_star-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_star", "InputHeatmap", function(.data, ...){ .data |>	layer_symbol(..., symbol="star") })

#' Adds a layer of symbols above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_asterisk() from a `InputHeatmap` object, adds a symbol annotation layer over the heatmap tiles.
#'
#' @importFrom rlang enquo
#' 
#'
#' @name layer_asterisk
#' @rdname layer_asterisk-method
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> layer_asterisk()
#'
#'
#' @export
setGeneric("layer_asterisk", function(.data,	...)
	standardGeneric("layer_asterisk"))

#' layer_asterisk
#' 
#' @docType methods
#' @rdname layer_asterisk-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_asterisk", "InputHeatmap", function(.data, ...){ .data |>	layer_symbol(..., symbol="asterisk") })


#' Adds a layers of texts above the heatmap tiles to a `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description layer_text() from a `InputHeatmap` object, adds a text annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' 
#' 
#'
#' @name layer_text
#' @rdname layer_text-method
#'
#' @param .data A `InputHeatmap` 
#' @param ... Expressions that return a logical value, and are defined in terms of the variables in .data. If multiple expressions are included, they are combined with the & operator. Only rows for which all conditions evaluate to TRUE are kept.
#' @param .value A column name or character string. 
#' @param .size A column name or a double. 
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
#'   tidyHeatmap::N52 |>
#'   mutate(my_text = "t") |>
#'   tidyHeatmap::heatmap(
#'     .row = text_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' ) 
#' 
#' hm |> layer_text(.value = "a")
#' hm |> layer_text(.value = my_text)
#'
#' @export
setGeneric("layer_text", function(.data,
																	...,
																	.value,
																	.size = NULL)
	standardGeneric("layer_text"))

#' layer_text
#' 
#' @importFrom rlang quo_is_null
#' 
#' @docType methods
#' @rdname layer_text-method
#' 
#' @keywords internal
#' @noRd
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_text", "InputHeatmap", function(.data,
																								 ...,
																								 .value,
																								 .size = NULL){
	
	.data_drame = .data@data
	.size = enquo(.size)
	
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
	.data@layer_text= 
		.data@layer_text |>
		bind_rows(
			
			.data_drame |>
				droplevels() |>
				mutate(
					column = !!.horizontal %>%  as.factor()  %>%  as.integer(),
					row = !!.vertical  %>%  as.factor() %>% as.integer()
				) |>
				filter(...) |>

				mutate(text := as.character( !!enquo(.value) )) |> 
				
				# Add size
				when(
					quo_is_null(.size) ~ mutate(., size = min(12, 320 / max(dim(abundance_mat)) )) ,
					~ mutate(., size := !!.size )
					) |> 
				
				select(column, row, text, size) 
				

		)
	
	.data
	
	
})

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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> split_rows(2)
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
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("split_rows", "InputHeatmap", function(.data,
																								 number_of_groups){
	
	# Get the same methods as the heatmap
	distance_method = .data@input |> when(
		"clustering_distance_rows" %in% names(.) ~ .data@input$clustering_distance_rows,
		~ "euclidean"
	)
	clustering_method = .data@input |> when(
		"clustering_method_rows" %in% names(.) ~ .data@input$clustering_method_rows,
		~ "complete"
	)
	
	# Get clusters
	hr = 
		.data@input[[1]] |>
		dist(method = distance_method) |>
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
#' 
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> split_columns(2)
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
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("split_columns", "InputHeatmap", function(.data,
																										number_of_groups){
	
	# Get the same methods as the heatmap
	distance_method = .data@input |> when(
		"clustering_distance_columns" %in% names(.) ~ .data@input$clustering_distance_columns,
		~ "euclidean"
	)
	clustering_method = .data@input |> when(
		"clustering_method_columns" %in% names(.) ~ .data@input$clustering_method_columns,
		~ "complete"
	)
	
	# Get clusters
	hr = 
		.data@input[[1]] |>
		t() |>
		dist(method = distance_method) |>
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
#' 	tidyHeatmap::heatmap(
#'   dplyr::group_by(tidyHeatmap::pasilla,		location, type),
#'   .column = sample,
#'   .row = symbol,
#'   .value = `count normalised adjusted`,
#'  ) |>
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



