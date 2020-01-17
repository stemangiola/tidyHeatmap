## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, 
                      message = FALSE, cache.lazy = FALSE)

library(tidyverse)
library(tidyHeatmap)


## -----------------------------------------------------------------------------
pasilla

## -----------------------------------------------------------------------------
pasilla %>%
	heatmap(
		.horizontal = sample,
		.vertical = symbol,
		.abundance = `count normalised adjusted`,
		annotation = c(condition, type),
		log_transform = T
	)

## -----------------------------------------------------------------------------
pasilla %>%
	group_by(location) %>%
	heatmap(
		.horizontal = sample,
		.vertical = symbol,
		.abundance = `count normalised adjusted`,
		annotation = c(condition, type),
		log_transform = T
	)

## -----------------------------------------------------------------------------
sessionInfo()

