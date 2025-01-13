context('tests')

# For resetting
# testthat::snapshot_review('tests/')

test_that("basic plot",{

 
	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row"
		)
	
	vdiffr::expect_doppelganger("base", p)
	

})

test_that("grouped plot",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				`Cell type`
				),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row"
		)
	
	
	vdiffr::expect_doppelganger("grouped", p)
	
})

test_that("annotated plot numerical continuous intereg nominal annot",{
	
	p = 
		tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row"
		) |>
		annotation_tile(CAPRA_TOTAL)
	
	vdiffr::expect_doppelganger("annotated heatmap 1", p)
	
})

test_that("annotated plot continuous annot MUST ERROR",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	expect_error(
		tidyHeatmap::heatmap(
			 left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, sample), a = rnorm(n()))), 
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row"
		) |> 
			annotation_tile(a), "Your annotation*", fixed=FALSE) 
	
})

test_that("annotated plot continuous annot as well",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	my_UBR = c( -0.4271163,  5.3530527, -0.7269678, -5.8277242, -4.0925786,  3.4246804, -1.6002821, -6.5576121,  -2.9980416 ,-0.6470534,  4.4336807, -0.7569798,  1.6489560)
	
	set.seed(123)
	p = 
		tidyHeatmap::heatmap(
			left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, UBR), a = my_UBR)), 
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row"
		) |>
		annotation_tile(a) |>
		annotation_tile(CAPRA_TOTAL)
	
	vdiffr::expect_doppelganger("annotated heatmap 2", p)
	
})

test_that("grouped and annotated plot",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				`Cell type`
			),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row"
		) |>
		annotation_tile(CAPRA_TOTAL)
	
	
	vdiffr::expect_doppelganger("grouped annotated heatmap 1", p)
	
})

test_that("grouped double and annotated plot",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			scale = "row"
		) |>
		annotation_tile(condition) |>
		annotation_tile(activation)
	
	
	vdiffr::expect_doppelganger("grouped annotated heatmap 2", p)
	
	
})

test_that("grouping error",{
	
	expect_error(
		
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type, condition),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			scale = "row"
		) |>
			annotation_tile(condition) |>
			annotation_tile(activation),
		regexp = "tidyHeatmap says: At the moment just one grouping per dimension*"
	)
	
	
})

test_that("pasilla one annotation",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`,
			scale = "row"
		)  |>
		annotation_tile(condition)
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("pasilla heatmap 1", p)
	
})

test_that("pasilla 2 annotations",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`,
			scale = "row"
		) |>
		annotation_tile(condition) |>
		annotation_tile(type)
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("pasilla heatmap 2", p)
	
})

test_that("pasilla custom color abundance",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`,
			palette_value = c("#d80000", "#ffffff", "#283cea"),
			scale = "row"
		) |>
		annotation_tile(condition) |>
		annotation_tile(type)
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("custom color", p)
	
})


test_that("pasilla custom color discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`,
			scale = "row"
		)  |>
		annotation_tile(condition, c("#d80000", "#283cea")) |>
		annotation_tile(type)
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("custom color discrete", p)
	
})

test_that("pasilla custom color contunuous",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`,
			scale = "row"
		) |>
		annotation_tile(activation, c("#d80000", "#283cea"))
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("custom color contunuous", p)
	
})

test_that("pasilla custom color contunuous AND discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`,
			scale = "row"
		) |>
		annotation_tile(condition) |>
		annotation_tile(type) |>
		annotation_tile(activation) 
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("custom color both", p)
	
})

test_that("grouped and annotated plot both vertical and horizontal",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			scale = "row"
		) |>
		annotation_tile(condition) |>
		annotation_tile(type) |>
		annotation_tile(activation) 
	
	
	vdiffr::expect_doppelganger("grouped custom color both", p)
	
})

test_that("pass arguments with ...",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			scale = "row",
			show_heatmap_legend = FALSE
		) |>
		annotation_tile(condition) |>
		annotation_tile(type) |>
		annotation_tile(activation) 
	
	
	vdiffr::expect_doppelganger("show_heatmap_legend", p)
	
})


test_that("Custom function for fill abundance palette",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row",
			palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
		)
	
	
	vdiffr::expect_doppelganger("colorRamp2", p)
	
})

test_that("Warning if data sparse",{
	
	p=
		tidyHeatmap::heatmap(
			dplyr::slice(dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"), -1),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row", 
			palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
		)
	
	vdiffr::expect_doppelganger("sparse", p)
	
})



test_that("test log of 0",{
	
	expect_error(
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count`,
			scale = "row", 
			transform = log	
		),
		"you applied a transformation that introduced negative infinite .value"
	)
	
	p=
			tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count`,
			scale = "row", 
			transform = log1p	
		)
	
	expect_equal(class(p)[1], "InputHeatmap")
	
})

test_that("test scale",{
	
	p=tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`,
				scale = "row"
			)
	vdiffr::expect_doppelganger("scale row", p)
	
	p=tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				scale = "column"
			)
	vdiffr::expect_doppelganger("scale column", p)
	
p=tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				scale = "both"
			)
vdiffr::expect_doppelganger("scale both", p)

	p=tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				scale = "none"
			)
	vdiffr::expect_doppelganger("scale none", p)
	
	expect_error(tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				scale = "WRONG_INPUT"
			), "the scale parameter has to be one")
})

test_that("multi-type",{
	
	library(magrittr)

	p = 
		dplyr::group_by(tidyHeatmap::pasilla,		location, type) |>
		dplyr::mutate(act = activation) |> 
		tidyr::nest(data = -sample) |>
		dplyr::mutate(size = c(4.014422, 3.783935, 4.844936, 4.614196, 4.138012, 3.475512, 3.739565)) |>
		dplyr::mutate(age = c(147 , 98,  96,  83, 105, 198,  73)) |>
		tidyr::unnest(data) |>
		tidyHeatmap::heatmap(
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) |>
		annotation_tile(condition) |>
		annotation_point(activation) |>
		annotation_tile(act) |>
		annotation_bar(size) |>
		annotation_line(age)
	
	
	vdiffr::expect_doppelganger("multi-type", p)
	
})

test_that("save_pdf",{
	
	library(magrittr)
	
	filename = tempfile()
		
	tidyHeatmap::heatmap(
		dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
		.column = UBR, 
		.row = symbol_ct, 
		.value = `read count normalised log`,
		scale = "row"
	) |>
	save_pdf(filename)
	
	if (file.exists(filename)) file.remove(filename)
})

test_that("managing palette usage",{
	
	p1 = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			scale = "row"
		)
	
	l1 = length(p1@palette_discrete)
	lc2 = length(p1@palette_continuous)
	
	p2 = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			scale = "row"
		)
	
	expect_equal(length(p2@palette_discrete), l1 )
	
	p3 = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			scale = "row"
		)
	
	expect_equal(length(p3@palette_discrete), length(p2@palette_discrete))
	
	p4 =
		p3 |>
		annotation_tile(condition) |>
		annotation_tile(activation)
	
	expect_equal(length(p4@palette_discrete), length(p3@palette_discrete)-1 )
	
	p5 =
		p1 |>
		annotation_tile(condition) |>
		annotation_tile(activation)
	
	expect_equal(length(p5@palette_discrete), length(p1@palette_discrete)-1 )
	expect_equal(length(p5@palette_continuous), length(p1@palette_continuous)-1 )
	
})

test_that("test sparse matrix",{
	
	p=data.frame(G = c('G1', 'G2', 'G3'), Y = c('M1', 'M1', 'M2'), V = c(1,2,3)) |>
			as_tibble() |>
			tidyHeatmap::heatmap(
				G, Y, V,
				cluster_rows = FALSE,
				cluster_columns = FALSE,
				scale = "row"
			)
	
	vdiffr::expect_doppelganger("sparse matrix", p)
	
	
})


test_that("layer symbol",{
	
	
	
	p = 
		tidyHeatmap::N52 |>
		tidyHeatmap::heatmap(
			.row = symbol_ct,
			.column = UBR,
			.value = `read count normalised log`,
			scale = "row"
		) |> 
		layer_point(
			`read count normalised log` > 4 & 
				UBR %in% c(11405, 11427)
		)
	
	
	vdiffr::expect_doppelganger("layer symbol", p)
	
	
})

test_that("split",{
	
	p = 
		tidyHeatmap::N52 |>
		tidyHeatmap::heatmap(
			.row = symbol_ct,
			.column = UBR,
			.value = `read count normalised log`,
			scale = "row"
		) |> 
		split_rows(2) |>
		split_columns(2)
	
	
	vdiffr::expect_doppelganger("split", p)
	
	
})

test_that("legend",{
	
	p = 
		tidyHeatmap::N52 |>
		tidyHeatmap::heatmap(
			.row = symbol_ct,
			.column = UBR,
			.value = `read count normalised log`,
			scale = "row",
			show_heatmap_legend = FALSE
		) |> 
		annotation_tile(UBR, show_legend = FALSE) |>
		annotation_tile(`Cell type`, show_legend = FALSE) 
	
	vdiffr::expect_doppelganger("legend", p)
	
	
})

test_that("size annotation",{
	
	p = 
		tidyHeatmap::N52 |>
		tidyHeatmap::heatmap(
			.row = symbol_ct,
			.column = UBR,
			.value = `read count normalised log`,
			scale = "row",
			show_heatmap_legend = FALSE
		) |> 
		annotation_tile(UBR, size = unit(20, "mm")) |>
		annotation_tile(`Cell type`, size = unit(20, "mm")) 
	
	vdiffr::expect_doppelganger("size annotation", p)
	
	
})

test_that("wrap heatmap for patchwork",{
	
	library(patchwork)
	
	p = 
		tidyHeatmap::N52 |>
		tidyHeatmap::heatmap(
			.row = symbol_ct,
			.column = UBR,
			.value = `read count normalised log`,
			scale = "row"
		) %>% 
		wrap_heatmap()
	
	vdiffr::expect_doppelganger("wrap heatmap", p + p)
	
	
})

test_that("plus operator",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row"
		)
	
	p = p+p
	
	
	vdiffr::expect_doppelganger("plus operator", p)
	
})

test_that("tile colorRamp2 palette",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			scale = "row"
		) %>%
		annotation_tile(
			inflection,
			palette = colorRamp2(c(0, 3,10), c("white", "green", "red"))
		)
	
	
	vdiffr::expect_doppelganger("tile colorRamp2 palette", p)
	
})

test_that("annotation tile factor colour order",{
  
  # If factor levels correctly interpreted, colour palette should read in order: 
  # "first_level", "second_level", "third_level", "fourth_level"

  p = 
    tidyHeatmap::pasilla |> 
    distinct(sample) |>
    mutate(group = rep(c("first_level", "third_level", "second_level", "fourth_level"), length.out = n())) |>
    mutate(group = factor(group, levels = c("first_level", "second_level", "third_level", "fourth_level"))) |>
    right_join(tidyHeatmap::pasilla, by = "sample") |>
    tidyHeatmap::heatmap(
      .column = sample,
      .row = symbol,
      .value = `count normalised adjusted log`,
      scale = "row"
    )  |>
    annotation_tile(group, c("#FF004B", "#FF00FF", "#9700FF", "#2000FF"))
  
  vdiffr::expect_doppelganger("tile factor custom palette", p)
  
})


test_that("patchwork padding",{
	
	p = 
		tidyHeatmap::N52 |> 
		dplyr::filter(, Category == "Angiogenesis") |> 
		tidyHeatmap::heatmap(
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		) |> 
		wrap_heatmap(padding = grid::unit(c(-10, -10, -10, -10), unit="pt"))
	
	
	vdiffr::expect_doppelganger("patchwork padding", p)
	
})

test_that("text",{
	
	base_heatmap = 
		tidyHeatmap::pasilla |>
		mutate(my_size = 5) |>
		mutate(my_text = "a") |> 
		filter(symbol %in% head(unique(tidyHeatmap::pasilla$symbol), n = 10)) |> 
		heatmap(
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			scale = "row"
		) 
	
	# Base plot
	vdiffr::expect_doppelganger(
		"text base",
		base_heatmap |> 
			layer_text(.value="gg")
	)
	
	# Text column
	vdiffr::expect_doppelganger(
		"text with text column",
		base_heatmap |> 
			layer_text(.value=my_text)
	)
	
	# Size
	vdiffr::expect_doppelganger(
		"text with size",
		base_heatmap |> 
			layer_text(.value="gg", .size = 5)
	)
	
	# Size column
	vdiffr::expect_doppelganger(
		"text with size column",
		base_heatmap |> 
			layer_text(.value="gg", .size = my_size)
	)
	
	# Two texts
	vdiffr::expect_doppelganger(
		"text multiple",
		base_heatmap |> 
			layer_text( `count normalised adjusted log` > 6 & sample == "untreated3" , .value="gg") |> 
			layer_text( `count normalised adjusted log` < 6 & sample == "untreated3" , .value="ll") 
	)
	
	# Complex
	vdiffr::expect_doppelganger(
		"text complex",
		base_heatmap |> 
			layer_text( `count normalised adjusted log` > 6 & sample == "untreated3" , .value="ll", .size = 10) |> 
			layer_text( `count normalised adjusted log` < 6 & sample == "untreated3" , .value=my_text, .size = my_size) 
	)
})


test_that("sample_swap",{

df <- tribble(~Compound_Name, ~Compound_Class, ~col, ~log2fc,
                   "L-homoserineAA", "AA", 1, 2.93,
                   "cellobioseCH", "CH", 1, 2.09,
                   "D-maltoseCH", "CH", 1, 3.08,
                   "pectinCH", "CH", 1, -3.04,
                   "raffinoseCH", "CH", 1, -2.10)

vdiffr::expect_doppelganger(
  "sample_swap",
  df %>%
    group_by(Compound_Class) %>%
    heatmap(.row = Compound_Name, .col = col, .value = log2fc)
  )

})