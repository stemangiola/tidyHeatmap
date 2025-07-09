# Specify undefined global variables for R CMD check
utils::globalVariables(c("shape", "size", "text", ":="))

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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("as_ComplexHeatmap", function(tidyHeatmap) standardGeneric("as_ComplexHeatmap"))


#' Creates a  `ComplexHeatmap` object for less standard plot manipulation (e.g. changing legend position)
#'
#' @importFrom ComplexHeatmap columnAnnotation
#' @importFrom ComplexHeatmap rowAnnotation
#' @importFrom ComplexHeatmap HeatmapAnnotation
#'
#' @docType methods
#' @rdname as_ComplexHeatmap-method
#'
#' @export
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setMethod("as_ComplexHeatmap", "InputHeatmap", function(tidyHeatmap){
	
	# Fix CRAN notes
	. = NULL
	index_column_wise = NULL
	shape = NULL
	
	top_annotations <- c(
	  tidyHeatmap@group_top_annotation,
	  tidyHeatmap@top_annotation |> annot_to_list()
	) |>
	  list_drop_null() |> 
	  filter_args(
	    HeatmapAnnotation, 
	    force_keep = 	    
	      seq_len(nrow(tidyHeatmap@top_annotation) + length(tidyHeatmap@group_top_annotation))

	   ) # force keep because columnAnnotation, which calls HeatmapAnnotation has ellipse as first argument. Pretty peculiar setup |>
	
	
	tidyHeatmap@input$top_annotation <- 
	  if (length(top_annotations) > 0 && !is.null(top_annotations)) {
	    do.call("columnAnnotation", top_annotations)
	  } else {
	    NULL
	  }
	
	left_annotations <- c(
	  tidyHeatmap@group_left_annotation,
	  tidyHeatmap@left_annotation |> annot_to_list()
	) |>
	  list_drop_null() |> 
	  filter_args(
	    HeatmapAnnotation, 
	    force_keep = 
	      seq_len(nrow(tidyHeatmap@left_annotation) + length(tidyHeatmap@group_left_annotation))
	   ) # force keep because columnAnnotation, which calls HeatmapAnnotation has ellipse as first argument. Pretty peculiar setup
	
	tidyHeatmap@input$left_annotation <- 
	  if (length(left_annotations) > 0 && !is.null(left_annotations)) {
	    do.call("rowAnnotation", left_annotations)
	  } else {
	    NULL
	  }
	
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
				size = unit(ind$size, "mm"), 
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
#' 
#' @importFrom ComplexHeatmap add_heatmap
#' 
#' @export
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
"+.InputHeatmap" <- function(e1, e2) {
	
  add_heatmap(as_ComplexHeatmap(e1), as_ComplexHeatmap(e2)) 

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
#' @param .data A `tbl_df` formatted as | <.row> | <.column> | <.value> | <...> |
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
#' @return An `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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
#' @param .data a `InputHeatmap` object created calling `tidyHeatmap::heatmap()`
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to 
#'   \code{\link[ComplexHeatmap:anno_block]{anno_block}} and 
#'   \code{\link[ComplexHeatmap:HeatmapAnnotation]{HeatmapAnnotation}}
#'   if you want to fine tune the aesthetics.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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
#' @param .data a `InputHeatmap` object created calling `tidyHeatmap::heatmap()`
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to 
#'   \code{\link[ComplexHeatmap:anno_points]{anno_points}} and 
#'   \code{\link[ComplexHeatmap:HeatmapAnnotation]{HeatmapAnnotation}}
#'   if you want to fine tune the aesthetics.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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
#' @param .data a `InputHeatmap` object created calling `tidyHeatmap::heatmap()`
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to 
#'   \code{\link[ComplexHeatmap:anno_lines]{anno_lines}} and 
#'   \code{\link[ComplexHeatmap:HeatmapAnnotation]{HeatmapAnnotation}}
#'   if you want to fine tune the aesthetics.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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
#' @param .data a `InputHeatmap` object created calling `tidyHeatmap::heatmap()`
#' @param .column Vector of quotes
#' @param palette A character vector of colors, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param ... The arguments that will be passed to 
#'   \code{\link[ComplexHeatmap:anno_barplot]{anno_barplot}} and 
#'   \code{\link[ComplexHeatmap:HeatmapAnnotation]{HeatmapAnnotation}}
#'   if you want to fine tune the aesthetics.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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

#' Adds a numeric annotation layer to an `InputHeatmap`, that on evaluation creates a `ComplexHeatmap`
#'
#' \lifecycle{maturing}
#'
#' @description `annotation_numeric()` from an `InputHeatmap` object adds a numeric annotation layer.
#'
#' @importFrom rlang enquo
#' @importFrom grid unit 
#'
#' @name annotation_numeric
#' @rdname annotation_numeric-method
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .column Vector of quotes
#' @param palette A character vector of colours, or a function such as colorRamp2 (see examples).
#' @param size A grid::unit object, e.g. unit(2, "cm"). This is the height or width of the annotation depending on the orientation.
#' @param labels_format A function to format the numeric labels. By default, 
#' it formats numbers to two decimal places using `sprintf("%.2f", x)`. You can
#' supply any function that takes a numeric vector and returns a character vector
#' for customised formatting.
#' @param ... The arguments that will be passed to 
#'   \code{\link[ComplexHeatmap:anno_numeric]{anno_numeric}} and 
#'   \code{\link[ComplexHeatmap:HeatmapAnnotation]{HeatmapAnnotation}}
#'   if you want to fine tune the aesthetics.
#'
#' @details It uses `ComplexHeatmap` as the visualisation tool.
#'
#' @return An `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`.
#'
#' @examples
#'
#' hm = 
#'   tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' )
#' 
#' hm |> annotation_numeric(inflection)
#' 
#' # Align to the right 
#' hm |> annotation_numeric(inflection, align_to = "right")
#'
#' @export
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss, 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("annotation_numeric", function(.data,
                                          .column,
                                          palette = NULL, size = NULL, labels_format = function(x) sprintf("%.1f", x), ...)
  standardGeneric("annotation_numeric"))

#' annotation_numeric
#' 
#' @docType methods
#' @rdname annotation_numeric-method
#' 
#' @return An `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`.
#'
setMethod("annotation_numeric", "InputHeatmap", function(.data,
                                                         .column,
                                                         palette = NULL, size = NULL, labels_format = function(x) sprintf("%.1f", x), ...) {
  .column <- enquo(.column)
  
  .data |> add_annotation(!!.column, type = "numeric", size = size, labels_format = labels_format, ...)
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
#' @param .size A column name or a double. The size of the elements of the layer.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("layer_arrow_up", function(.data,..., 
																			.size = NULL)
	standardGeneric("layer_arrow_up"))

#' layer_arrow_up
#' 
#' @docType methods
#' @rdname layer_arrow_up-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_arrow_up", "InputHeatmap", function(.data,..., 
																										 .size = NULL){ .data |>	layer_symbol(..., symbol="arrow_up", .size = !!enquo(.size)) })

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
#' @param .size A column name or a double. The size of the elements of the layer.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("layer_arrow_down", function(.data,..., 
																				.size = NULL)
	standardGeneric("layer_arrow_down"))

#' layer_arrow_down
#' 
#' @docType methods
#' @rdname layer_arrow_down-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_arrow_down", "InputHeatmap", function(.data,..., 
																											 .size = NULL){ .data |>	layer_symbol(..., symbol="arrow_down", .size = !!enquo(.size)) })

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
#' @param .size A column name or a double. The size of the elements of the layer.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("layer_point", function(.data,..., 
																	 .size = NULL)
	standardGeneric("layer_point"))

#' layer_point
#' 
#' @docType methods
#' @rdname layer_point-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_point", "InputHeatmap", function(.data,..., 
																									.size = NULL){ .data |>	layer_symbol(..., symbol="point", .size = !!enquo(.size)) })

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
#' @param .size A column name or a double. The size of the elements of the layer.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("layer_square", function(.data,..., 
																		.size = NULL)
	standardGeneric("layer_square"))

#' layer_square
#' 
#' @docType methods
#' @rdname layer_square-method
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
#'
setMethod("layer_square", "InputHeatmap", function(.data,..., 
																									 .size = NULL){ .data |>	layer_symbol(..., symbol="square", .size = !!enquo(.size)) })

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
#' @param .size A column name or a double. The size of the elements of the layer.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("layer_diamond", function(.data,..., 
																		 .size = NULL)
	standardGeneric("layer_diamond"))

#' layer_diamond
#' 
#' @docType methods
#' @rdname layer_diamond-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
setMethod("layer_diamond", "InputHeatmap", function(.data,..., 
																										.size = NULL){ .data |>	layer_symbol(..., symbol="diamond", .size = !!enquo(.size)) })

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
#' @param .size A column name or a double. The size of the elements of the layer.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("layer_star", function(.data,..., 
																	.size = NULL)
	standardGeneric("layer_star"))

#' layer_star
#' 
#' @docType methods
#' @rdname layer_star-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
setMethod("layer_star", "InputHeatmap", function(.data,..., 
																								 .size = NULL){ .data |>	layer_symbol(..., symbol="star", .size = !!enquo(.size)) })

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
#' @param .size A column name or a double. The size of the elements of the layer.
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("layer_asterisk", function(.data,..., 
																			.size = NULL)
	standardGeneric("layer_asterisk"))

#' layer_asterisk
#' 
#' @docType methods
#' @rdname layer_asterisk-method
#' 
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
setMethod("layer_asterisk", "InputHeatmap", function(.data,..., 
																										 .size = NULL){ .data |>	layer_symbol(..., symbol="asterisk", .size = !!enquo(.size)) })


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
#' @param .size A column name or a double. The size of the elements of the layer.
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
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#' ) 
#' 
#' hm |> layer_text(.value = "a")
#' hm |> layer_text(.value = my_text)
#'
#' @export
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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
#' 
#' @return A `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
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
setMethod("save_pdf", "Heatmap", .save_pdf)

#' save_pdf
#' 
#' @param .heatmap A `Heatmap` 
#' @param filename A character string. The name of the output file/path
#' @param width A `double`. Plot width
#' @param height A `double`. Plot height
#' @param units	A character string. units ("in", "cm", or "mm")
setMethod("save_pdf", "InputHeatmap", .save_pdf)

#' Add group annotation strips to a tidyHeatmap
#'
#' @description `annotation_group()` adds group annotation strips to a tidyHeatmap object, 
#' allowing you to visually group rows or columns based on categorical variables. This is 
#' useful for highlighting biological or experimental groups in your heatmap.
#'
#' @param .data A tidyHeatmap object
#' @param ... Grouping columns (unquoted, like dplyr::group_by)
#' @param palette_grouping List of color vectors for each grouping. Each element should be 
#'   a vector of colors for the corresponding grouping variable.
#' @param group_label_fontsize Font size for group labels
#' @param show_group_name Logical, show the group annotation name
#' @param group_strip_height Height of group strip as a grid unit (default: 9pt)
#' @return A tidyHeatmap object with group annotation strips added
#' 
#' @examples
#' 
#' # Basic usage with row grouping
#' tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#'   ) |>
#'   annotation_group(CAPRA_TOTAL)
#' 
#' # With custom colors and formatting
#' tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#'   ) |>
#'   annotation_group(
#'     CAPRA_TOTAL,
#'     palette_grouping = list(c("#E64B35", "#4DBBD5")),
#'     group_label_fontsize = 10,
#'     show_group_name = FALSE
#'   )
#' 
#' # Multiple grouping variables
#' tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#'   ) |>
#'   annotation_group(
#'     CAPRA_TOTAL, 
#'     `Cell type`,
#'     palette_grouping = list(
#'       c("#E64B35", "#4DBBD5"),  # colors for CAPRA_TOTAL
#'       c("#00A087", "#F39B7F")   # colors for Cell type
#'     )
#'   )
#' 
#' @export
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("annotation_group", function(.data, ...) standardGeneric("annotation_group"))

#' @export
#' @rdname annotation_group
setMethod("annotation_group", "InputHeatmap", function(
  .data,
  ...,
  palette_grouping = list(),
  group_label_fontsize = 8,
  show_group_name = TRUE,
  group_strip_height = grid::unit(9, "pt")
) {
  group_vars <- rlang::enquos(...)
  if (length(group_vars) > 0) {
    .data@data <- dplyr::group_by(.data@data, !!!group_vars)
    .data@arguments$group_vars <- group_vars
  }
  .data@arguments$palette_grouping <- palette_grouping
  .data@arguments$group_label_fontsize <- group_label_fontsize
  .data@arguments$show_group_name <- show_group_name
  .data@arguments$group_strip_height <- group_strip_height

  add_grouping(.data)
})

#' Retrieve heatmap data and dendrograms as plotted
#'
#' \lifecycle{maturing}
#'
#' @description get_heatmap_data() extracts the heatmap matrix as it appears in the plot along with the row and column dendrograms, all with consistent naming.
#'
#' @importFrom ComplexHeatmap draw row_order column_order row_dend column_dend
#'
#' @name get_heatmap_data
#' @rdname get_heatmap_data-method
#'
#' @param .data A `InputHeatmap` object from tidyHeatmap::heatmap()
#'
#' @details This function converts the InputHeatmap to ComplexHeatmap, draws it to perform clustering, then extracts the ordered matrix and dendrograms exactly as they appear in the heatmap plot.
#' 
#' @return A list containing:
#' \itemize{
#'   \item matrix: The abundance matrix with rows and columns ordered as in the heatmap
#'   \item row_dend: The row dendrogram object
#'   \item column_dend: The column dendrogram object
#' }
#'
#' @examples
#'
#' hm <- tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#'   ) |>
#'   annotation_group(
#'     CAPRA_TOTAL,
#'     palette_grouping = list(c("#E64B35", "#4DBBD5")),
#'     group_label_fontsize = 10,
#'     show_group_name = FALSE
#'   )
#' 
#' # Multiple grouping variables
#' tidyHeatmap::N52 |>
#'   tidyHeatmap::heatmap(
#'     .row = symbol_ct,
#'     .column = UBR,
#'     .value = `read count normalised log`
#'   ) |>
#'   annotation_group(
#'     CAPRA_TOTAL, 
#'     `Cell type`,
#'     palette_grouping = list(
#'       c("#E64B35", "#4DBBD5"),  # colors for CAPRA_TOTAL
#'       c("#00A087", "#F39B7F")   # colors for Cell type
#'     )
#'   )
#' 
#' # Get heatmap data as plotted
#' result <- hm |> get_heatmap_data()
#' ordered_matrix <- result$matrix
#' row_dendrogram <- result$row_dend
#' column_dendrogram <- result$column_dend
#'
#' @export
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
setGeneric("get_heatmap_data", function(.data) standardGeneric("get_heatmap_data"))

#' get_heatmap_data
#' 
#' @docType methods
#' @rdname get_heatmap_data-method
#' 
#' @return A list containing the ordered matrix, row dendrogram, and column dendrogram
#'
setMethod("get_heatmap_data", "InputHeatmap", function(.data) {
	
	# Convert to ComplexHeatmap and draw it
	ch <- .data |> as_ComplexHeatmap()
	ch_drawn <- ComplexHeatmap::draw(ch)
	
	# Get row and column orders
	row_ord <- ComplexHeatmap::row_order(ch_drawn)
	col_ord <- ComplexHeatmap::column_order(ch_drawn)
	
	# Get the abundance matrix from the original object
	abundance_mat <- .data@input[[1]]
	
	# Handle grouped heatmaps (row_ord and col_ord might be lists) vs regular heatmaps
	if (is.list(row_ord)) {
		# For grouped heatmaps, concatenate all group orders and remove names
		row_ord <- as.integer(unlist(row_ord))
	}
	if (is.list(col_ord)) {
		# For grouped heatmaps, concatenate all group orders and remove names
		col_ord <- as.integer(unlist(col_ord))
	}
	
	# Create ordered matrix with consistent row and column names
	ordered_matrix <- abundance_mat[row_ord, col_ord]
	
	# Get dendrograms
	row_dendrogram <- ComplexHeatmap::row_dend(ch_drawn)
	column_dendrogram <- ComplexHeatmap::column_dend(ch_drawn)
	

	
	return(list(
		matrix = ordered_matrix,
		row_dend = row_dendrogram,
		column_dend = column_dendrogram
	))

})



