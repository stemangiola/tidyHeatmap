% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\docType{methods}
\name{heatmap}
\alias{heatmap}
\alias{heatmap_}
\alias{heatmap,tbl-method}
\alias{heatmap,tbl_df-method}
\title{Creates a  `InputHeatmap` object from `tbl_df` on evaluation creates a `ComplexHeatmap`}
\source{
[Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
}
\usage{
heatmap(
  .data,
  .row,
  .column,
  .value,
  transform = NULL,
  scale = "none",
  palette_value = c("#440154FF", "#21908CFF", "#fefada"),
  palette_grouping = list(),
  .scale = NULL,
  ...
)

heatmap_(
  .data,
  .row,
  .column,
  .value,
  transform = NULL,
  scale = "none",
  palette_value = c("#440154FF", "#21908CFF", "#fefada"),
  palette_grouping = list(),
  .scale = NULL,
  ...
)

\S4method{heatmap}{tbl}(
  .data,
  .row,
  .column,
  .value,
  transform = NULL,
  scale = "none",
  palette_value = c("#440154FF", "#21908CFF", "#fefada"),
  palette_grouping = list(),
  .scale = NULL,
  ...
)

\S4method{heatmap}{tbl_df}(
  .data,
  .row,
  .column,
  .value,
  transform = NULL,
  scale = "none",
  palette_value = c("#440154FF", "#21908CFF", "#fefada"),
  palette_grouping = list(),
  .scale = NULL,
  ...
)
}
\arguments{
\item{.data}{A `tbl_df` formatted as | <ELEMENT> | <FEATURE> | <VALUE> | <...> |}

\item{.row}{The name of the column to use as rows}

\item{.column}{The name of the column to use as columns}

\item{.value}{The name of the column to use as values}

\item{transform}{A function to transform the data (optional)}

\item{scale}{One of "none", "row", "column", or "both" for scaling}

\item{palette_value}{A character vector of colors or a function for value colors}

\item{palette_grouping}{A list of color palettes for grouping annotations}

\item{.scale}{DEPRECATED: Use scale instead}

\item{...}{Additional arguments passed to ComplexHeatmap}
}
\value{
An `InputHeatmap` object that gets evaluated to a `ComplexHeatmap`

A `InputHeatmap` object

A `InputHeatmap` object

A `InputHeatmap` object
}
\description{
heatmap() takes a tbl object and easily produces a ComplexHeatmap plot, with integration with tibble and dplyr frameworks.
}
\details{
\lifecycle{maturing}

This function takes a tbl as an input and creates a `ComplexHeatmap` plot. The information is stored in a `InputHeatmap` object that is updated along the pipe statement, for example adding annotation layers.
}
\examples{


tidyHeatmap::N52 |>
  dplyr::group_by( `Cell type`) |>
  tidyHeatmap::heatmap(
   .row = symbol_ct,
   .column = UBR,
   .value = `read count normalised log`,
  )

}
\references{
Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
  modular heatmap production based on tidy principles." Journal of Open Source Software.
  doi:10.21105/joss.02472.
}
