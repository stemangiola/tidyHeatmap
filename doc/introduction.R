## ---- eval=FALSE--------------------------------------------------------------
#  
#  devtools::install_github("stemangiola/tidyHeatmap")
#  
#  

## ---- eval=FALSE--------------------------------------------------------------
#  
#  install.packages("tidyHeatmap")
#  

## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(dplyr)
library(tidyr)
library(tidyHeatmap)
library(grid)

## -----------------------------------------------------------------------------
mtcars_tidy <- 
	mtcars |> 
	as_tibble(rownames="Car name") |> 
	
	# Scale
	mutate_at(vars(-`Car name`, -hp, -vs), scale) |>
	
	# tidyfy
	pivot_longer(cols = -c(`Car name`, hp, vs), names_to = "Property", values_to = "Value")

mtcars_tidy

## -----------------------------------------------------------------------------
mtcars_heatmap <- 
	mtcars_tidy |> 
	heatmap(`Car name`, Property, Value,	scale = "row"	) |>
	annotation_tile(hp)

mtcars_heatmap

## ----eval=F-------------------------------------------------------------------
#  mtcars_heatmap |> save_pdf("mtcars_heatmap.pdf")

## -----------------------------------------------------------------------------
# Make up more groupings
mtcars_tidy_groupings = 
	mtcars_tidy |>
	mutate(property_group = if_else(Property %in% c("cyl", "disp"), "Engine", "Other"))

mtcars_tidy_groupings |> 
	group_by(vs, property_group) |>
	heatmap(`Car name`, Property, Value,	scale = "row"	) |>
	annotation_tile(hp)

## -----------------------------------------------------------------------------
mtcars_tidy_groupings |> 
	group_by(vs, property_group) |>
	heatmap(
		`Car name`, Property, Value	,	
		scale = "row",
		palette_grouping = list(
			
			# For first grouping (vs)
			c("#66C2A5", "#FC8D62"), 
			
			# For second grouping (property_group)
			c("#b58b4c", "#74a6aa")
		)
	) |>
	annotation_tile(hp)


## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(`Car name`, Property, Value,	scale = "row"	) |>
	split_rows(2) |>
	split_columns(2)

## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row",
		row_km = 2,
		column_km = 2
	) 

## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, 
		Property, 
		Value,	
		scale = "row",
		palette_value = c("red", "white", "blue")
	)

## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, 
		Property, 
		Value,	
		scale = "row",
		palette_value = circlize::colorRamp2(
			seq(-2, 2, length.out = 11), 
			RColorBrewer::brewer.pal(11, "RdBu")
		)
	)


## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, 
		Property, 
		Value,	
		scale = "row",
		palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
	)

## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, 
		Property, 
		Value,	
		scale = "row"
	) |>
	add_tile(
		hp, 
		palette = c("red", "white", "blue")
	)

## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, 
		Property, 
		Value,	
		scale = "row"
	) |>
	annotation_tile(
		hp, 
		palette = circlize::colorRamp2(c(0, 100, 200, 300), viridis::magma(4))
	)

## -----------------------------------------------------------------------------
tidyHeatmap::pasilla |>
	group_by(location, type) |>
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row"
	) |>
	annotation_tile(condition) |>
	annotation_tile(activation)

## -----------------------------------------------------------------------------

tidyHeatmap::pasilla |>
	group_by(location, type) |>
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row",
		show_heatmap_legend = FALSE
	) |>
	annotation_tile(condition, show_legend = FALSE) |>
	annotation_tile(activation, show_legend = FALSE)

## -----------------------------------------------------------------------------
# Create some more data points
pasilla_plus <- 
	tidyHeatmap::pasilla |>
	dplyr::mutate(act = activation) |> 
	tidyr::nest(data = -sample) |>
	dplyr::mutate(size = rnorm(n(), 4,0.5)) |>
	dplyr::mutate(age = runif(n(), 50, 200)) |>
	tidyr::unnest(data) 

# Plot
pasilla_plus |>
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row"
	) |>
	annotation_tile(condition) |>
	annotation_point(activation) |>
	annotation_tile(act) |>
	annotation_bar(size) |>
	annotation_line(age)

## -----------------------------------------------------------------------------
pasilla_plus |>
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row"
	) |>
	annotation_tile(condition, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8)) |>
	annotation_point(activation, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8)) |>
	annotation_tile(act, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8)) |>
	annotation_bar(size, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8)) |>
	annotation_line(age, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8))

## -----------------------------------------------------------------------------
tidyHeatmap::pasilla |>
	
	# filter
	filter(symbol %in% head(unique(tidyHeatmap::pasilla$symbol), n = 10)) |>
	
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row"
	) |> 
	layer_point(
		`count normalised adjusted log` > 6 & sample == "untreated3" 
	)

## ---- warning=FALSE-----------------------------------------------------------

p_heatmap = heatmap(mtcars_tidy, `Car name`, Property, Value, scale = "row") 

p_heatmap + p_heatmap


## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		rect_gp = grid::gpar(col = "#161616", lwd = 0.5)
	) 

## -----------------------------------------------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		cluster_rows = FALSE
	) 

## -----------------------------------------------------------------------------
library(forcats)
mtcars_tidy |> 
	mutate(`Car name` = fct_reorder(`Car name`, `Car name`, .desc = TRUE)) %>% 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		cluster_rows = FALSE
	) 

## -----------------------------------------------------------------------------
mtcars_tidy |> 
	mutate(`Car name` = fct_reorder(`Car name`, `Car name`, .desc = TRUE)) %>% 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		column_dend_height = unit(0.2, "cm"), 
		row_dend_width = unit(0.2, "cm")
	) 

## -----------------------------------------------------------------------------
mtcars_tidy |> 
	mutate(`Car name` = fct_reorder(`Car name`, `Car name`, .desc = TRUE)) %>% 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		row_names_gp = gpar(fontsize = 7),
		column_names_gp = gpar(fontsize = 7),
		column_title_gp = gpar(fontsize = 7),
		row_title_gp = gpar(fontsize = 7)
	) 

## -----------------------------------------------------------------------------

heatmap(mtcars_tidy, `Car name`, Property, Value, scale = "row"	) %>%
	as_ComplexHeatmap() %>%
	ComplexHeatmap::draw(heatmap_legend_side = "left"	)

## -----------------------------------------------------------------------------
mtcars_tidy |> 
    heatmap(`Car name`, Property, Value,    scale = "row"   ) |>
    as_ComplexHeatmap() |>
    ComplexHeatmap::draw(
        column_title = "TITLE", 
        column_title_gp = gpar(fontsize = 16)
    )

## -----------------------------------------------------------------------------
library(ggplot2)
library(patchwork)

p_heatmap =
	mtcars_tidy |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
			show_heatmap_legend = FALSE,
		row_names_gp = gpar(fontsize = 7)
	) 

p_ggplot = tibble(value = 1:10) %>% ggplot(aes(value)) + geom_density()

wrap_heatmap(p_heatmap) + 
	p_ggplot +
	wrap_heatmap(p_heatmap) + 
	plot_layout(width = c(1, 0.3, 1))


## -----------------------------------------------------------------------------
mtcars_tidy |> 
    heatmap(`Car name`, Property, Value,  scale = "row" ) |>
    wrap_heatmap() +
		ggplot2::ggtitle("TITLE")

## -----------------------------------------------------------------------------
sessionInfo()

