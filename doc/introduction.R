## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, message = FALSE, cache.lazy = FALSE)

library(magrittr)
library(dplyr)
library(tidyr)
library(tidyHeatmap)
library(purrr)


## ---- eval=FALSE--------------------------------------------------------------
#  
#  devtools::install_github("stemangiola/tidyHeatmap")
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  install.packages("tidyHeatmap")
#  

## -----------------------------------------------------------------------------
mtcars_tidy = 
	mtcars %>% 
	as_tibble(rownames="Car name") %>% 
	
	# Scale
	mutate_at(vars(-`Car name`, -hp, -vs), scale) %>%
	
	# tidyfy
	gather(Property, Value, -`Car name`, -hp, -vs)

mtcars_tidy

## -----------------------------------------------------------------------------
mtcars_heatmap = 
	mtcars_tidy %>% 
		heatmap(
			`Car name`, 
			Property, 
			Value,
			annotation = hp
		)

mtcars_heatmap

## ----eval=F-------------------------------------------------------------------
#  mtcars_heatmap %>%
#  	save_pdf("mtcars_heatmap.pdf")

## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	group_by(vs) %>%
	heatmap(
		`Car name`, 
		Property, 
		Value,
		annotation = hp
	)

## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	heatmap(
		`Car name`, 
		Property, 
		Value,
		palette_value = c("red", "white", "blue")
	)

## -----------------------------------------------------------------------------
mtcars_tidy %>% 
	heatmap(
		`Car name`, 
		Property, 
		Value,
		palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
	)

## -----------------------------------------------------------------------------
tidyHeatmap::pasilla %>%
	group_by(location, type) %>%
	heatmap(
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, activation)
		)

## -----------------------------------------------------------------------------
# Chreate some more data points
pasilla_plus = 
	tidyHeatmap::pasilla %>%
		dplyr::mutate(act = activation) %>% 
		tidyr::nest(data = -sample) %>%
		dplyr::mutate(size = rnorm(n(), 4,0.5)) %>%
		dplyr::mutate(age = runif(n(), 50, 200)) %>%
		tidyr::unnest(data) 

# Plot
pasilla_plus %>%
		tidyHeatmap::heatmap(
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, activation, act, size, age),
			type = c("tile", "point", "tile", "bar", "line")
		)

