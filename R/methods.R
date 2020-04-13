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
#' @name heatmap
#' @rdname heatmap
#'
#' @param .data A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |
#' @param .row The name of the column vertically presented in the heatmap
#' @param .column The name of the column horizontally presented in the heatmap
#' @param .value The name of the transcript/gene abundance column
#' @param annotation Vector of quotes
#' @param transform A function, used to tranform .value, for example log
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
					 annotation = NULL,
					 transform = NULL,
					 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
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
#' @export
heatmap.default <-
	function(.data,
					 .row, 
					 .column,
					 .value,
					 annotation = NULL,
					 transform = NULL,
					 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
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
#' @export
heatmap.tbl_df <-
	function(.data,
					 .row, 
					 .column,
					 .value,
					 annotation = NULL,
					 transform = NULL,
					 palette_value = c("#440154FF", "#21908CFF", "#fefada" ),
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
			if(palette_abundance) palette_value <- palette_abundance
		}
		
		.row = enquo(.row)
		.column = enquo(.column)
		.value <- enquo(.value)

		# Validation
		.data %>% validation(!!.column, !!.row, !!.value)
		
		# Check if data is rectangular
		.data %>% 
			ifelse_pipe(
				!check_if_data_rectangular((.), !!.column, !!.row, !!.value),
				~  eliminate_sparse_transcripts(.x, !!.row)
			) %>%
			
		# Run plotting function
		plot_heatmap(
			.horizontal = !!.column,
			.vertical = !!.row,
			.abundance = !!.value,
			annotation = !!annotation,
			transform = transform,
			palette_abundance = palette_value,
			palette_discrete = palette_discrete,
			palette_continuous = palette_continuous,
			...
		)
		
	}



