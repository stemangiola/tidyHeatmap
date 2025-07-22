# Specify undefined global variables for R CMD check
utils::globalVariables(c("shape", "size", "text", ":="))

# InputHeatmap class definition
InputHeatmap <- setClass(
	"InputHeatmap",  
	slots = list(
		input = "list", 
		data = "tbl",
		palette_discrete = "list", 
		palette_continuous = "list",
		group_top_annotation = "list",
		group_left_annotation = "list",
		top_annotation = "tbl",
		left_annotation = "tbl",
		arguments = "list" ,
		layer_symbol = "tbl",
		layer_text = "tbl"
	),
	prototype = list(
		palette_discrete = list(
			brewer.pal(9, "Set1"),
			brewer.pal(8, "Set2"),
			brewer.pal(12, "Set3"),
			brewer.pal(8, "Dark2"),
			brewer.pal(8, "Accent"),
			brewer.pal(8, "Pastel2")
		), 
		palette_continuous = list(
			brewer.pal(11, "Spectral") |> rev(),
			viridis(n = 5),
			magma(n = 5),
			brewer.pal(11, "PRGn"),
			brewer.pal(11, "BrBG")
		),
		input = list(),
		top_annotation = tibble(col_name = character(), orientation = character(), col_orientation = character(), data = list(), fx = list(), annot = list(), annot_type = character(), idx = integer(), color = list(), further_arguments = list()),
		left_annotation = tibble(col_name = character(), orientation = character(), col_orientation = character(), data = list(), fx = list(), annot = list(), annot_type = character(), idx = integer(), color = list(), further_arguments = list()),
		group_top_annotation = list(),
		group_left_annotation = list(),
		layer_symbol = tibble(column = integer(), row = integer(), shape = integer()),
		layer_text = tibble(column = integer(), row = integer(), text = character(), size = numeric())
	)
) 