## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, 
                      message = FALSE, cache.lazy = FALSE)

library(magrittr)
library(dplyr)
library(tidyHeatmap)


## -----------------------------------------------------------------------------
pasilla

## -----------------------------------------------------------------------------
pasilla %>%
	heatmap(
		.horizontal = sample,
		.vertical = symbol,
		.value = `count normalised adjusted`,
		annotation = c(condition, type),
		log_transform = TRUE
	)

## -----------------------------------------------------------------------------
pasilla %>%
	group_by(location) %>%
	heatmap(
		.horizontal = sample,
		.vertical = symbol,
		.value = `count normalised adjusted`,
		annotation = c(condition, type),
		log_transform = TRUE
	)

