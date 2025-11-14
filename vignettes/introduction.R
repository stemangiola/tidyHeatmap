params <-
list(demo_metadata = TRUE)

## ----setup, include=FALSE-----------------------------------------------------

knitr::opts_chunk$set(
  dpi = 150,
  fig.retina = 1,
  dev = "png",
  dev.args = list(
    png = list(type = "cairo-png", antialias = "subpixel" )
  )
)


## ----install, eval=FALSE------------------------------------------------------
# 
# devtools::install_github("stemangiola/tidyHeatmap")
# 
# 

## ----install2, eval=FALSE-----------------------------------------------------
# 
# install.packages("tidyHeatmap")
# 

## ----library, echo=FALSE, include=FALSE---------------------------------------
library(dplyr)
library(tidyr)
library(tidyHeatmap)
library(grid)

## ----setup data---------------------------------------------------------------
mtcars_tidy <- 
	mtcars |> 
	as_tibble(rownames="Car name") |> 
	
	# Scale
	mutate_at(vars(-`Car name`, -hp, -vs), scale) |>
	
	# tidyfy
	pivot_longer(cols = -c(`Car name`, hp, vs), names_to = "Property", values_to = "Value")

mtcars_tidy

## ----heatmap, fig.width=10, fig.height=10-------------------------------------
mtcars_heatmap <- 
	mtcars_tidy |> 
	heatmap(`Car name`, Property, Value,	scale = "row"	) |>
	annotation_tile(hp)

mtcars_heatmap

## ----save, eval=F-------------------------------------------------------------
# mtcars_heatmap |> save_pdf("mtcars_heatmap.pdf")

## ----distance, fig.width=10, fig.height=10------------------------------------

tidyHeatmap::pasilla |>
	
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row",
		
		# Arguments passed to ComplexHeatmap 
		clustering_distance_rows = "manhattan",
		clustering_distance_columns = "manhattan",
		clustering_method_rows = "ward.D",
		clustering_method_columns = "ward.D"
	) 

## ----grouping, fig.width=10, fig.height=10------------------------------------
# Make up more groupings
mtcars_tidy_groupings = 
	mtcars_tidy |>
	mutate(property_group = if_else(Property %in% c("cyl", "disp"), "Engine", "Other"))

mtcars_tidy_groupings |> 
	heatmap(`Car name`, Property, Value, scale = "row") |>
	annotation_group(vs, property_group) |>
	annotation_tile(hp)

## ----grouping2, fig.width=10, fig.height=10-----------------------------------
mtcars_tidy_groupings |> 
	heatmap(
		`Car name`, Property, Value ,  
		scale = "row"
	) |>
	annotation_group(
		vs, property_group,
		palette_grouping = list(
			# For first grouping (vs)
			c("#66C2A5", "#FC8D62"), 
			# For second grouping (property_group)
			c("#b58b4c", "#74a6aa")
		),
		group_label_fontsize = 14,
		show_group_name = TRUE,
		group_strip_height = grid::unit(20, "pt")
	) |>
	annotation_tile(hp)

## ----split, fig.width=10, fig.height=10---------------------------------------
mtcars_tidy |> 
	heatmap(`Car name`, Property, Value,	scale = "row"	) |>
	split_rows(2) |>
	split_columns(2)

## ----split2, fig.width=10, fig.height=10--------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row",
		row_km = 2,
		column_km = 2
	) 

## ----custom, fig.width=10, fig.height=10--------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, 
		Property, 
		Value,	
		scale = "row",
		palette_value = c("red", "white", "blue")
	)

## ----redblue, fig.width=10, fig.height=10-------------------------------------
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


## ----flexible, fig.width=10, fig.height=10------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, 
		Property, 
		Value,	
		scale = "row",
		palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
	)

## ----customtile, fig.width=10, fig.height=10----------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, 
		Property, 
		Value,	
		scale = "row"
	) |>
	annotation_tile(
		hp, 
		palette = c("red", "white", "blue")
	)

## ----customtile2, fig.width=10, fig.height=10---------------------------------
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

## ----multiple, fig.width=10, fig.height=10------------------------------------
tidyHeatmap::pasilla |>
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,  
		scale = "row"
	) |>
	annotation_group(location, type) |>
	annotation_tile(condition) |>
	annotation_tile(activation)

## ----multiple-aesthetics, fig.width=10, fig.height=10-------------------------
tidyHeatmap::pasilla |>
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,  
		scale = "row"
	) |>
	annotation_group(
		location, type,
		group_label_fontsize = 12,
		group_strip_height = grid::unit(15, "pt"),
		show_group_name = FALSE
	) |>
	annotation_tile(condition) |>
	annotation_tile(activation)

## ----nolegend, fig.width=10, fig.height=10------------------------------------

tidyHeatmap::pasilla |>
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row",
		show_heatmap_legend = FALSE
	) |>
	annotation_tile(condition, show_legend = FALSE) |>
	annotation_tile(activation, show_legend = FALSE)

## ----manyannotations, fig.width=10, fig.height=10-----------------------------
# Create some more data points
pasilla_plus <- 
	tidyHeatmap::pasilla |>
	dplyr::mutate(activation_2 = activation, activation_3 = activation) |> 
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
	annotation_numeric(activation_3) |>
	annotation_tile(activation_2) |>
	annotation_bar(size) |>
	annotation_line(age)

## ----size, fig.width=10, fig.height=10----------------------------------------
pasilla_plus |>
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row"
	) |>
	annotation_tile(condition, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8)) |>
	annotation_point(activation, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8)) |>
	annotation_tile(activation_2, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8)) |>
	annotation_bar(size, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8)) |>
	annotation_line(age, size = unit(0.3, "cm"),	annotation_name_gp= gpar(fontsize = 8))

## ----layer, fig.width=10, fig.height=10---------------------------------------
tidyHeatmap::pasilla |>
	
	# filter
	filter(symbol %in% head(unique(tidyHeatmap::pasilla$symbol), n = 10)) |>
	
	# Add dynamic size
	mutate(my_size = runif(n(), 1,5)) |> 
	
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row"
	) |> 
	layer_point(
		`count normalised adjusted log` > 6 & sample == "untreated3"
	) |>
	layer_square(
		`count normalised adjusted log` > 6 & sample == "untreated2",
		.size = my_size
	) |>
	layer_arrow_up(
		`count normalised adjusted log` > 6 & sample == "untreated1",
		.size = 4
	)

## ----layertext, fig.width=10, fig.height=10-----------------------------------
tidyHeatmap::pasilla |>
	
	# filter
	filter(symbol %in% head(unique(tidyHeatmap::pasilla$symbol), n = 10)) |>
	
	# Add dynamic text
	mutate(my_text = "mt", my_size = 7) |> 
	
	# Plot
	heatmap(
		.column = sample,
		.row = symbol,
		.value = `count normalised adjusted`,	
		scale = "row"
	) |> 
	layer_text(
		`count normalised adjusted log` > 6 & sample == "untreated3", 
		.value = "a", 
		.size = 15
	) |> 
	layer_text(
	`count normalised adjusted log` > 6 & sample == "untreated2", 
	.value = my_text,
	.size = my_size
)

## ----sidebyside, warning=FALSE, fig.width=10, fig.height=6--------------------

p_heatmap = heatmap(mtcars_tidy, `Car name`, Property, Value, scale = "row") 

p_heatmap + p_heatmap 


## ----three_ways, warning=FALSE, fig.width=10, fig.height=6--------------------
as_ComplexHeatmap(p_heatmap) +
  as_ComplexHeatmap(p_heatmap) +
  as_ComplexHeatmap(p_heatmap)

## ----patchworkintegrate, fig.width=10, fig.height=6---------------------------
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

p_ggplot = data.frame(value = 1:10) |> ggplot(aes(value)) + geom_density()

wrap_heatmap(p_heatmap) + 
	p_ggplot +
	
	# Add padding for better aesthetics
	wrap_heatmap(
		p_heatmap,
		padding = grid::unit(c(-30, -0, -0, -10), "points" ),
		clip = FALSE
	) + 
	plot_layout(width = c(1, 0.3, 1))


## ----borders, fig.width=10, fig.height=6--------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		rect_gp = grid::gpar(col = "#161616", lwd = 0.5)
	) 

## ----droprow, fig.width=10, fig.height=6--------------------------------------
mtcars_tidy |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		cluster_rows = FALSE
	) 

## ----reorder, fig.width=10, fig.height=6--------------------------------------
library(forcats)
mtcars_tidy |> 
	mutate(`Car name` = forcats::fct_reorder(`Car name`, `Car name`, .desc = TRUE)) |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		cluster_rows = FALSE
	) 

## ----sizedendro, fig.width=10, fig.height=6-----------------------------------
mtcars_tidy |> 
	mutate(`Car name` = forcats::fct_reorder(`Car name`, `Car name`, .desc = TRUE)) |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		column_dend_height = unit(0.2, "cm"), 
		row_dend_width = unit(0.2, "cm")
	) 

## ----sizecolumns, fig.width=10, fig.height=10---------------------------------
mtcars_tidy |> 
	mutate(`Car name` = forcats::fct_reorder(`Car name`, `Car name`, .desc = TRUE)) |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		row_names_gp = gpar(fontsize = 7),
		column_names_gp = gpar(fontsize = 7),
		column_title_gp = gpar(fontsize = 7),
		row_title_gp = gpar(fontsize = 7)
	) 

## ----removetitles, fig.width=10, fig.height=10--------------------------------
mtcars_tidy |> 
	mutate(`Car name` = forcats::fct_reorder(`Car name`, `Car name`, .desc = TRUE)) |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row", 
		row_title = "",      # Remove row title
		column_title = ""    # Remove column title
	) 

## ----align_numeric, fig.width=10, fig.height=10-------------------------------
mtcars_tidy |> 
	mutate(`Car name` = forcats::fct_reorder(`Car name`, `Car name`, .desc = TRUE)) |> 
	heatmap(
		`Car name`, Property, Value,	
		scale = "row"
	) |> 
  annotation_numeric(hp, align_to="right")

## ----sidelegend, fig.width=10, fig.height=6-----------------------------------

heatmap(mtcars_tidy, `Car name`, Property, Value, scale = "row"	) |>
	as_ComplexHeatmap() |>
	ComplexHeatmap::draw(heatmap_legend_side = "left"	)

## ----title, fig.width=10, fig.height=6----------------------------------------
mtcars_tidy |> 
    heatmap(`Car name`, Property, Value,    scale = "row"   ) |>
    as_ComplexHeatmap() |>
    ComplexHeatmap::draw(
        column_title = "TITLE", 
        column_title_gp = gpar(fontsize = 16)
    )

## ----title2, fig.width=10, fig.height=10--------------------------------------
mtcars_tidy |> 
    heatmap(`Car name`, Property, Value,  scale = "row" ) |>
    wrap_heatmap() +
		ggplot2::ggtitle("TITLE")

## -----------------------------------------------------------------------------
sessionInfo()

