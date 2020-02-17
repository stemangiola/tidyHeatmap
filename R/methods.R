#' Creates a  `ComplexHeatmap` plot from `tbl`
#'
#' @description plot_heatmap() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#'
#' @name heatmap
#' @rdname heatmap
#'
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .horizontal The name of the column horizontally presented in the heatmap
#' @param .vertical The name of the column vertically presented in the heatmap
#' @param .abundance The name of the transcript/gene abundance column
#' @param annotation Vector of quotes
#' @param log_transform A boolean, whether the value should be log-transformed (e.g., TRUE for RNA sequencing data)
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
#'  .abundance = `read count normalised log`,
#'  annotation = CAPRA_TOTAL
#' )
#'
#'
#' @export
heatmap <-
	function(.data,
					 .horizontal,
					 .vertical,
					 .abundance,
					 annotation = NULL,
					 log_transform = FALSE) {
		UseMethod("heatmap", .data)
	}
#' @export
heatmap.default <-
	function(.data,
					 .horizontal,
					 .vertical,
					 .abundance,
					 annotation = NULL,
					 log_transform = FALSE)
	{
		message("tidyHeatmap::heatmap function cannot be applied to this object")
	}
#' @export
heatmap.tbl_df <-
	function(.data,
					 .horizontal,
					 .vertical,
					 .abundance,
					 annotation = NULL,
					 log_transform = FALSE)
	{
		# Make col names
		.horizontal = enquo(.horizontal)
		.vertical = enquo(.vertical)
		.abundance = enquo(.abundance)
		annotation = enquo(annotation)
		
		plot_heatmap(
			.data = .data,
			.horizontal = !!.horizontal,
			.vertical = !!.vertical,
			.abundance = !!.abundance,
			annotation = !!annotation,
			log_transform = log_transform
		)
		
	}
