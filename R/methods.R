#' Creates a  `ComplexHeatmap` plot from `tbl`
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
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .horizontal The name of the column horizontally presented in the heatmap
#' @param .vertical The name of the column vertically presented in the heatmap
#' @param .value The name of the transcript/gene abundance column
#' @param annotation Vector of quotes
#' @param log_transform A boolean, whether the value should be log-transformed (e.g., TRUE for RNA sequencing data)
#' @param palette_abundance A character vector This is the palette that will be used as gradient for abundance.
#' @param palette_discrete A list of character vectors. This is the list of palettes that will be used for horizontal and vertical discrete annotations. The discrete classification of annotations depends on the column type of your input tibble (e.g., character and factor).
#' @param palette_continuous A list of character vectors. This is the list of palettes that will be used for horizontal and vertical continuous annotations. The continuous classification of annotations depends on the column type of your input tibble (e.g., integer, numerical, double).
#' @param .abundance DEPRECATED. Please use .value instead
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
#'  .horizontal = UBR,
#'  .vertical = symbol_ct,
#'  .value = `read count normalised log`,
#'  annotation = CAPRA_TOTAL
#' )
#'
#'
#' @export
heatmap <-
	function(.data,
					 .horizontal,
					 .vertical,
					 .value,
					 annotation = NULL,
					 log_transform = FALSE,
					 palette_abundance = c("#440154FF", "#21908CFF", "#fefada" ),
					 palette_discrete = list(),
					 palette_continuous = list(),
					 .abundance  = NULL,
					 ...) {
		UseMethod("heatmap", .data)
	}
#' @export
heatmap.default <-
	function(.data,
					 .horizontal,
					 .vertical,
					 .value,
					 annotation = NULL,
					 log_transform = FALSE,
					 palette_abundance = c("#440154FF", "#21908CFF", "#fefada" ),
					 palette_discrete = list(),
					 palette_continuous = list(),
					 .abundance = NULL,
					 ...)
	{
		message("tidyHeatmap::heatmap function cannot be applied to this object. Please input a tibble (tbl_df) object.")
	}
#' @export
heatmap.tbl_df <-
	function(.data,
					 .horizontal,
					 .vertical,
					 .value,
					 annotation = NULL,
					 log_transform = FALSE,
					 palette_abundance = c("#440154FF", "#21908CFF", "#fefada" ),
					 palette_discrete = list(),
					 palette_continuous = list(),
					 .abundance = NULL,
					 ...)
	{
		# Comply with CRAN NOTES
		. = NULL
		
		# Make col names
		.horizontal = enquo(.horizontal)
		.vertical = enquo(.vertical)
		.abundance = enquo(.abundance)
		annotation = enquo(annotation)
		
		# Deprecation .abundance
		# Check if user has supplied `baz` instead of `bar`
		if (is_present(.abundance) & !quo_is_null(.abundance)) {
			
			# Signal the deprecation to the user
			deprecate_warn("0.99.11", "tidyHeatmap::heatmap(.abundance = )", "mypkg::foo(.value = )")
			
			# Deal with the deprecated argument for compatibility
			.value <- enquo(.abundance)
		}
		
		.value <- enquo(.value)
		
		# Validation
		.data %>% validation(!!.horizontal, !!.vertical, !!.value)
		
		# Check if data is rectangular
		.data %>% 
			ifelse_pipe(
				!check_if_data_rectangular((.), !!.horizontal, !!.vertical, !!.value),
				~  eliminate_sparse_transcripts(.x, !!.vertical)
			) %>%
			
		# Run plotting function
		plot_heatmap(
			.horizontal = !!.horizontal,
			.vertical = !!.vertical,
			.abundance = !!.value,
			annotation = !!annotation,
			log_transform = log_transform,
			palette_abundance = palette_abundance,
			palette_discrete = palette_discrete,
			palette_continuous = palette_continuous,
			...
		)
		
	}



