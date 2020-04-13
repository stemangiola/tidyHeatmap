## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(knitr)
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, message = FALSE, cache.lazy = FALSE)

library(magrittr)
library(dplyr)
library(tidyr)
library(tidyHeatmap)


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
mtcars_tidy %>% 
	heatmap(
		`Car name`, 
		Property, 
		Value,
		annotation = hp
	)

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
		palette_abundance = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
	)

