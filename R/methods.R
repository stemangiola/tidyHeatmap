#' Creates a  `ComplexHeatmap` plot from `tbl`
#'
#' @description plot_heatmap() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
#'
#' @importFrom rlang enquo
#' @importFrom magrittr "%>%"
#'
#' @name heatmap
#' @rdname heatmap
#'
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .sample The name of the sample column
#' @param .transcript The name of the transcript/gene column
#' @param .abundance The name of the transcript/gene abundance column
#'
#' @details This function created a heatmap object and is useful if you want
#' to avoid to specify .sample, .transcript and .abundance arguments all the times.
#' The heatmap object have an attribute called parameters where these three
#' arguments are stored as metadata. They can be extracted as attr(<object>, "parameters").
#'
#' @return A `heatmap` object
#'
#'
#' @examples
#'
#' library(tidyverse)
#' tidyHeatmap::N52 %>%
#' group_by( `Cell type`) %>%
#' tidyHeatmap::plot_heatmap(
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
					 log_transform = F) {
		UseMethod("heatmap", .data)
	}
#' @export
heatmap.default <-
	function(.data,
					 .horizontal,
					 .vertical,
					 .abundance,
					 annotation = NULL,
					 log_transform = F)
	{
		print("tidyHeatmap::heatmap function cannot be applied to this object")
	}
#' @export
heatmap.tbl_df <-
	function(.data,
					 .horizontal,
					 .vertical,
					 .abundance,
					 annotation = NULL,
					 log_transform = F)
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
