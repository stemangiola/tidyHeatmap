#the class definition
InputHeatmap<-setClass(
	"InputHeatmap",  
	slots = c(
		input = "list", 
		data = "tbl",
		palette_discrete = "list", 
		palette_continuous = "list",
		arguments = "list" ,
		top_annotation = "tbl",
		left_annotation = "tbl",
		group_top_annotation = "list",
		group_left_annotation = "list"
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
		group_left_annotation = list()
	)
)

setMethod("show", "InputHeatmap", function(object){
	
	object@input$top_annotation = 
		c(
			object@group_top_annotation,
			object@top_annotation %>% annot_to_list()
		) %>%
		list_drop_null() %>%
		ifelse_pipe(
			(.) %>% length %>% `>` (0) && !is.null((.)), # is.null needed for check Windows CRAN servers
			~ do.call("columnAnnotation", .x ),
			~ NULL
		)
	
	object@input$left_annotation = 
		c(
			object@group_left_annotation,
			object@left_annotation %>% annot_to_list()
		) %>%
		list_drop_null()  %>%
		ifelse_pipe(
			(.) %>% length %>% `>` (0) && !is.null((.)), # is.null needed for check Windows CRAN servers
			~ do.call("rowAnnotation", .x ),
			~ NULL
		)
	
	show(do.call(Heatmap, object@input))
} )




#' Creates a  `InputHeatmap` plot from `tbl_df` on evaluation creates a `ComplexHeatmap`
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
#' @rdname heatmap
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .row The name of the column vertically presented in the heatmap
#' @param .column The name of the column horizontally presented in the heatmap
#' @param .value The name of the transcript/gene abundance column
#' @param transform A function, used to transform .value row-wise (e.g., transform = log1p)
#' @param .scale A character string. Possible values are c(\"none\", \"row\", \"column\", \"both\")
#' @param palette_value A character vector This is the palette that will be used as gradient for .value. For higher flexibility you can use circlize::colorRamp2\(c\(-2, -1, 0, 1, 2\), viridis::magma\(5\)\)
#' @param palette_grouping A list of character vectors. This is the list of palettes that will be used for grouping 
#' @param ... Further arguments to be passed to ComplexHeatmap::Heatmap
#' 
#' @param annotation DEPRECATED. please use the annotation functions add_* function \(\* one of tile, point, bar, line  \).
#' @param type DEPRECATED. please use the annotation functions add_* function \(\* one of tile, point, bar, line  \).
#' @param palette_discrete DEPRECATED. please use the annotation functions add_* function \(\* one of tile, point, bar, line  \).
#' @param palette_continuous DEPRECATED. please use the annotation functions add_* function \(\* one of tile, point, bar, line  \).
#' @param .horizontal DEPRECATED. Please use .column instead
#' @param .vertical DEPRECATED. Please use .row instead
#' @param .abundance DEPRECATED. Please use .value instead
#' @param log_transform DEPRECATED. Please use transform instead
#' @param palette_abundance DEPRECATED. Please use palette_value instead
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
#'
#' @export
heatmap <-
	function(.data,
					 .row, 
					 .column,
					 .value,
					 transform = NULL,
					 .scale = "row",
					 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
					 palette_grouping = list(),
					 
					 # DESPRECATED
					 annotation = NULL,
					 type = rep("tile", length(quo_names(annotation))),
					 palette_discrete = list(),
					 palette_continuous = list(),
					 .abundance  = NULL,
					 .horizontal = NULL,
					 .vertical = NULL,
					 log_transform = NULL,
					 palette_abundance = NULL,
					 ...) {
		UseMethod("heatmap", .data)
	}

#' Creates a  `ComplexHeatmap` plot from `tbl_df`
#' @inheritParams heatmap
#' @export
heatmap.default <-
	function(.data,
					 .row, 
					 .column,
					 .value,
					 transform = NULL,
					 .scale = "row",
					 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
					 palette_grouping = list(),
					 
					 # DESPRECATED
					 annotation = NULL,
					 type = rep("tile", length(quo_names(annotation))),
					 palette_discrete = list(),
					 palette_continuous = list(),
					 .abundance  = NULL,
					 .horizontal = NULL,
					 .vertical = NULL,
					 log_transform = NULL,
					 palette_abundance = NULL,
					 ...)
	{
		message("tidyHeatmap::heatmap function cannot be applied to this object. Please input a tibble (tbl_df) object.")
	}

#' Creates a  `ComplexHeatmap` plot from `tbl_df`
#' @inheritParams heatmap
#' @export
heatmap.tbl_df <-
	function(.data,
					 .row, 
					 .column,
					 .value,
					 transform = NULL,
					 .scale = "row",
					 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
					 palette_grouping = list(),
					 
					 # DESPRECATED
					 annotation = NULL,
					 type = rep("tile", length(quo_names(annotation))),
					 palette_discrete = list(),
					 palette_continuous = list(),
					 .abundance  = NULL,
					 .horizontal = NULL,
					 .vertical = NULL,
					 log_transform = NULL,
					 palette_abundance = NULL,
					 ...)
	{
		# Comply with CRAN NOTES
		. = NULL
		
		# Make col names
		.horizontal = enquo(.horizontal) # DEPRECATED
		.vertical = enquo(.vertical) # DEPRECATED
		.abundance = enquo(.abundance) # DEPRECATED
		annotation = enquo(annotation)
		
		# Check if transform is of correct type
		if(!(is.null(transform) || is_function(transform))) stop("tidyHeatmap says: transform has to be a function. is_function(transform) == TRUE")
		
		# Check if .scale is of correct type
		if(.scale %in% c("none", "row", "column", "both") %>% `!`) stop("tidyHeatmap says: the .scale parameter has to be one of c(\"none\", \"row\", \"column\", \"both\")")

		# Check if type is of the right kind
		if(type %>% setdiff(names(type_to_annot_function)) %>% length %>% `>` (0))
			stop("tidyHeatmap says: not all components of `type` parameter are valid.")
		
		# Deprecation .abundance
		
		if (is_present(.abundance) & !quo_is_null(.abundance)) {
			
			# Signal the deprecation to the user
			deprecate_warn("0.99.11", "tidyHeatmap::heatmap(.abundance = )", "tidyHeatmap::heatmap(.value = )")
			
			# Deal with the deprecated argument for compatibility
			.value <- enquo(.abundance)
		}
		
		# Deprecation .horizontal
		
		if (is_present(.horizontal) & !quo_is_null(.horizontal)) {
			
			# Signal the deprecation to the user
			deprecate_warn("0.99.12", "tidyHeatmap::heatmap(.horizontal = )", "tidyHeatmap::heatmap(.column = )")
			
			# Deal with the deprecated argument for compatibility
			.column <- enquo(.horizontal)
		}
		
		# Deprecation .vertical
		
		if (is_present(.vertical) & !quo_is_null(.vertical)) {
			
			# Signal the deprecation to the user
			deprecate_warn("0.99.12", "tidyHeatmap::heatmap(.vertical = )", "tidyHeatmap::heatmap(.row = )")
			
			# Deal with the deprecated argument for compatibility
			.row <- enquo(.vertical)
		}
		
		# Deprecation log_transform
		
		if (is_present(log_transform) & !is.null(log_transform)) {
			
			# Signal the deprecation to the user
			deprecate_warn("0.99.13", "tidyHeatmap::heatmap(log_transform = )", "tidyHeatmap::heatmap(tranform = )")
			
			# Deal with the deprecated argument for compatibility
			if(log_transform) tranform <- log
		}
		
		# Deprecation palette_abundance
		
		if (is_present(palette_abundance) & !is.null(palette_abundance)) {
			
			# Signal the deprecation to the user
			deprecate_warn("0.99.15", "tidyHeatmap::heatmap(palette_abundance = )", "tidyHeatmap::heatmap(palette_value = )")
			
			# Deal with the deprecated argument for compatibility
			palette_value <- palette_abundance
		}
		
		.row = enquo(.row)
		.column = enquo(.column)
		.value <- enquo(.value)

		# Validation
		.data %>% validation(!!.column, !!.row, !!.value)
		

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
				palette_abundance = palette_value,
				palette_grouping = palette_grouping,
				...
			)		%>%
			
			# Add group annotation if any
			when( "groups" %in%  (attributes(.data) %>% names) ~ 	add_grouping(.), ~ (.))
		
		# WITH OLD PARAMETRISATION
		
		# .data %>% 
		# 	
		# 	# # Check if data is rectangular
		# 	# ifelse_pipe(
		# 	# 	!check_if_data_rectangular((.), !!.column, !!.row, !!.value),
		# 	# 	~  eliminate_sparse_transcripts(.x, !!.row)
		# 	# ) %>%
		# 	
		# # Run plotting function
		# input_heatmap(
		# 	.horizontal = !!.column,
		# 	.vertical = !!.row,
		# 	.abundance = !!.value,
		# 	annotation = !!annotation,
		# 	type = type,
		# 	transform = transform,
		# 	.scale = .scale,
		# 	palette_abundance = palette_value,
		# 	palette_discrete = palette_discrete,
		# 	palette_continuous = palette_continuous,
		# 	...
		# )
		
	}

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
#' @rdname add_tile
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
#' @inheritParams add_tile
#' 
#' @docType methods
#' @rdname add_tile-methods
#' 
#' @return A `add_tile` object
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
			select(!!.column) %>% 
			sapply(class) %>% 
			when(. %in% c("factor", "character", "logical") &	!is.null(palette) ~ list(palette), ~ list()),
		
		# If annotation is continuous
		palette_continuous = 
			.data@data %>% 
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
#' @rdname add_point
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
#' @inheritParams add_point
#' 
#' @docType methods
#' @rdname add_point-methods
#' 
#' @return A `add_point` object
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
#' @inheritParams add_line
#' 
#' @docType methods
#' @rdname add_line-methods
#' 
#' @return A `add_line` object
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
#' @inheritParams add_bar
#' 
#' @docType methods
#' @rdname add_bar-methods
#' 
#' @return A `add_bar` object
#'
setMethod("add_bar", "InputHeatmap", function(.data,
																							 .column,
																							 palette = NULL){
	
	.column = enquo(.column)
	
	.data %>% add_annotation(	!!.column,	type = "bar")
	
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
#' @inheritParams save_pdf
#' 
#' @export
setMethod("save_pdf", "Heatmap", .save_pdf)

#' save_pdf
#' 
#' @inheritParams save_pdf
#' 
#' @export
setMethod("save_pdf", "InputHeatmap", .save_pdf)



