# From
# https://github.com/tidyverse/ggplot2/blob/master/R/save.r


plot_dim <- function(dim = c(NA, NA), scale = 1, units = c("in", "cm", "mm"),
										 limitsize = TRUE) {
	
	units <- match.arg(units)
	to_inches <- function(x) x / c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]
	from_inches <- function(x) x * c(`in` = 1, cm = 2.54, mm = 2.54 * 10)[units]
	
	dim <- to_inches(dim) * scale
	
	if (any(is.na(dim))) {
		if (length(grDevices::dev.list()) == 0) {
			default_dim <- c(7, 7)
		} else {
			default_dim <- grDevices::dev.size() * scale
		}
		dim[is.na(dim)] <- default_dim[is.na(dim)]
		dim_f <- prettyNum(from_inches(dim), digits = 3)
		
		message("tidyHeatmap says: saving ", dim_f[1], " x ", dim_f[2], " ", units, " image")
	}
	
	if (limitsize && any(dim >= 50)) {
		stop(sprintf("
      tidyHeatmap says: dimensions exceed 50 inches (height and width are specified in '%s' not pixels).
      If you're sure you want a plot that big, use `limitsize = FALSE`.
    ", units))
	}
	
	dim
}

plot_dev <- function(device, filename = NULL, dpi = 300) {
	force(filename)
	force(dpi)
	
	if (is.function(device)) {
		if ("file" %in% names(formals(device))) {
			dev <- function(filename, ...) device(file = filename, ...)
			return(dev)
		} else {
			return(device)
		}
	}
	
	eps <- function(filename, ...) {
		grDevices::postscript(file = filename, ..., onefile = FALSE, horizontal = FALSE,
													paper = "special")
	}
	devices <- list(
		pdf =  function(filename, ..., version = "1.4") grDevices::pdf(file = filename, ..., version = version),
		png =  function(...) grDevices::png(..., res = dpi, units = "in")
	)
	
	dev <- devices[[device]]

	dev
}