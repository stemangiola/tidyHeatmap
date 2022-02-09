context('tests')

# For resetting
# testthat::snapshot_review('tests')

test_that("basic plot",{

	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
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
			.value = `read count normalised log`
		)
	
	
	vdiffr::expect_doppelganger("grouped", p)
	
})

test_that("annotated plot numerical continuous intereg nominal annot",{
	
	p = 
		tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		) |>
		add_tile(CAPRA_TOTAL)
	
	vdiffr::expect_doppelganger("annotated heatmap 1", p)
	
})

test_that("annotated plot continuous annot MUST ERROR",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	expect_error(
		tidyHeatmap::heatmap(
			 left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, sample), a = rnorm(n()))), 
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		) |> 
			add_tile(a), "Your annotation*", fixed=FALSE) 
	
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
			.value = `read count normalised log`
		) |>
		add_tile(a) |>
		add_tile(CAPRA_TOTAL)
	
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
			.value = `read count normalised log`
		) |>
		add_tile(CAPRA_TOTAL)
	
	
	vdiffr::expect_doppelganger("grouped annotated heatmap 1", p)
	
})

test_that("grouped double and annotated plot",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) |>
		add_tile(condition) |>
		add_tile(activation)
	
	
	vdiffr::expect_doppelganger("grouped annotated heatmap 2", p)
	
	
})

test_that("grouping error",{
	
	expect_error(
		
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type, condition),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) |>
			add_tile(condition) |>
			add_tile(activation),
		regexp = "tidyHeatmap says: At the moment just one grouping per dimension*"
	)
	
	
})

test_that("pasilla one annotation",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`
		)  |>
		add_tile(condition)
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("pasilla heatmap 1", p)
	
})

test_that("pasilla 2 annotations",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`
		) |>
		add_tile(condition) |>
		add_tile(type)
	
	
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
			palette_value = c("#d80000", "#ffffff", "#283cea")
		) |>
		add_tile(condition) |>
		add_tile(type)
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("custom color", p)
	
})


test_that("pasilla custom color discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`
		)  |>
		add_tile(condition, c("#d80000", "#283cea")) |>
		add_tile(type)
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("custom color discrete", p)
	
})

test_that("pasilla custom color contunuous",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`
		) |>
		add_tile(activation, c("#d80000", "#283cea"))
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("custom color contunuous", p)
	
})

test_that("pasilla custom color contunuous AND discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted log`
		) |>
		add_tile(condition) |>
		add_tile(type) |>
		add_tile(activation) 
	
	
	expect_equal(class(p)[1], "InputHeatmap")
	#vdiffr::expect_doppelganger("custom color both", p)
	
})

test_that("grouped and annotated plot both vertical and horizontal",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) |>
		add_tile(condition) |>
		add_tile(type) |>
		add_tile(activation) 
	
	
	vdiffr::expect_doppelganger("grouped custom color both", p)
	
})

test_that("pass arguments with ...",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			show_heatmap_legend = FALSE
		) |>
		add_tile(condition) |>
		add_tile(type) |>
		add_tile(activation) 
	
	
	vdiffr::expect_doppelganger("show_heatmap_legend", p)
	
})


test_that("Custom function for fill abundance palette",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
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
				.scale = "row"
			)
	vdiffr::expect_doppelganger("scale row", p)
	
	p=tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				.scale = "column"
			)
	vdiffr::expect_doppelganger("scale column", p)
	
p=tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				.scale = "both"
			)
vdiffr::expect_doppelganger("scale both", p)

	p=tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				.scale = "none"
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
		add_tile(condition) |>
		add_point(activation) |>
		add_tile(act) |>
		add_bar(size) |>
		add_line(age)
	
	
	vdiffr::expect_doppelganger("multi-type", p)
	
})

test_that("save_pdf",{
	
	library(magrittr)
	
	filename = tempfile()
		
	tidyHeatmap::heatmap(
		dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
		.column = UBR, 
		.row = symbol_ct, 
		.value = `read count normalised log`
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
			.value = `count normalised adjusted`
		)
	
	l1 = length(p1@palette_discrete)
	lc2 = length(p1@palette_continuous)
	
	p2 = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		)
	
	expect_equal(length(p2@palette_discrete), l1 )
	
	p3 = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		)
	
	expect_equal(length(p3@palette_discrete), length(p2@palette_discrete))
	
	p4 =
		p3 |>
		add_tile(condition) |>
		add_tile(activation)
	
	expect_equal(length(p4@palette_discrete), length(p3@palette_discrete)-1 )
	
	p5 =
		p1 |>
		add_tile(condition) |>
		add_tile(activation)
	
	expect_equal(length(p5@palette_discrete), length(p1@palette_discrete)-1 )
	expect_equal(length(p5@palette_continuous), length(p1@palette_continuous)-1 )
	
})

test_that("test sparse matrix",{
	
	p=data.frame(G = c('G1', 'G2', 'G3'), Y = c('M1', 'M1', 'M2'), V = c(1,2,3)) |>
			as_tibble() |>
			tidyHeatmap::heatmap(
				G, Y, V,
				cluster_rows = FALSE,
				cluster_columns = FALSE
			)
	
	vdiffr::expect_doppelganger("sparse matrix", p)
	
	
})


test_that("layer symbol",{
	
	
	
	p = 
		tidyHeatmap::N52 |>
		tidyHeatmap::heatmap(
			.row = symbol_ct,
			.column = UBR,
			.value = `read count normalised log`
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
			.value = `read count normalised log`
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
			show_heatmap_legend = FALSE
		) |> 
		add_tile(UBR, show_legend = FALSE) |>
		add_tile(`Cell type`, show_legend = FALSE) 
	
	vdiffr::expect_doppelganger("legend", p)
	
	
})

test_that("size annotation",{
	
	p = 
		tidyHeatmap::N52 |>
		tidyHeatmap::heatmap(
			.row = symbol_ct,
			.column = UBR,
			.value = `read count normalised log`,
			show_heatmap_legend = FALSE
		) |> 
		add_tile(UBR, size = unit(20, "mm")) |>
		add_tile(`Cell type`, size = unit(20, "mm")) 
	
	vdiffr::expect_doppelganger("size annotation", p)
	
	
})

test_that("wrap heatmap for patchwork",{
	
	library(patchwork)
	
	p = 
		tidyHeatmap::N52 |>
		tidyHeatmap::heatmap(
			.row = symbol_ct,
			.column = UBR,
			.value = `read count normalised log`
		) %>% 
		wrap_heatmap()
	
	vdiffr::expect_doppelganger("wrap heatmap", p + p)
	
	
})