#' Creates a  `ComplexHeatmap` plot from `tbl_df`
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
#' @param annotation Vector of quotes
#' @param type A character vector of the set c(\"tile\", \"point\", \"bar\", \"line\")
#' @param transform A function, used to tranform .value row-wise (e.g., transform = log1p)
#' @param .scale A character string. Possible values are c(\"none\", \"row\", \"column\", \"both\")
#' @param palette_value A character vector This is the palette that will be used as gradient for .value
#' @param palette_discrete A list of character vectors. This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#' @param palette_continuous A list of character vectors. This is the list of palettes that will be used for horizontal and vertical continuous annotations. The continuous classification of annotations depends on the column type of your input tibble (e.g., integer, numerical, double).
#' @param .horizontal DEPRECATED. Please use .column instead
#' @param .vertical DEPRECATED. Please use .row instead
#' @param .abundance DEPRECATED. Please use .value instead
#' @param log_transform DEPRECATED. Please use transform instead
#' @param palette_abundance DEPRECATED. Please use palette_value instead
#' @param ... Further arguments to be passed to ComplexHeatmap::Heatmap
#'
#' @details To be added.
#'
#' @return A `ComplexHeatmap` object
#'
#'
#'
#' @examples
#'
#' library(dplyr)
#' tidyHeatmap::N52 %>%
#' group_by( `Cell type`) %>%
#' tidyHeatmap::heatmap(
#'  .row = symbol_ct,
#'  .column = UBR,
#'  .value = `read count normalised log`,
#'  annotation = CAPRA_TOTAL
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



#the class definition
InputHeatmap<-setClass(
	"InputHeatmap",  
	slots = c(
		input = "list", 
		data = "tbl",
		palette_discrete = "list", 
		palette_continuous = "list",
		arguments = "list" ,
		top_annotation = "list",
		left_annotation = "list"),
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
		top_annotation = list(),
		left_annotation = list()
	)
)

setMethod("show", "InputHeatmap", function(object){
	
	object@input$top_annotation = 
		object@top_annotation %>%
		list_drop_null() %>%
		ifelse_pipe(
			(.) %>% length %>% `>` (0) && !is.null((.)), # is.null needed for check Windows CRAN servers
			~ do.call("columnAnnotation", .x ),
			~ NULL
		)
	
	object@input$left_annotation = 
		object@left_annotation %>%
		list_drop_null()  %>%
		ifelse_pipe(
			(.) %>% length %>% `>` (0) && !is.null((.)), # is.null needed for check Windows CRAN servers
			~ do.call("rowAnnotation", .x ),
			~ NULL
		)
	
	show(do.call(Heatmap, object@input))
} )


