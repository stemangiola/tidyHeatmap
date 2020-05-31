context('tests')


test_that("basic plot",{

	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		)
	
	
  expect_equal(as.character(class(p)), "Heatmap" )

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
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("annotated plot numerical continuous intereg nominal annot",{
	
	p = 
		tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			annotation = CAPRA_TOTAL
		)
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("annotated plot continuous annot MUST ERROR",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	
	expect_error(
		tidyHeatmap::heatmap(
			 left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, sample), a = rnorm(n()))), 
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			annotation = a
		), "Your annotation*", fixed=FALSE) 
	
})

test_that("annotated plot continuous annot as well",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	
	p = 
		tidyHeatmap::heatmap(
			left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, UBR), a = rnorm(n(), sd=5))), 
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			annotation = c(a, CAPRA_TOTAL)
		)
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
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
			annotation = CAPRA_TOTAL
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("grouped double and annotated plot",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, activation)
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
	
})

test_that("grouping error",{
	
	expect_error(
		
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type, condition),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, activation)
		),
		regexp = "tidyHeatmap says: At the moment just one grouping per dimension*"
	)
	
	
})

test_that("pasilla one annotation",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = condition,
			transform = log1p
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla 2 annotations",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, type),
			transform = log1p
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla custom color abundance",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, type),
			transform = log1p, 
			palette_value = c("#d80000", "#ffffff", "#283cea")
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
	# Test deprecation
	expect_warning(
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, type),
			transform = log1p, 
			palette_abundance = c("#d80000", "#ffffff", "#283cea")
		),
		"Please use the `palette_value` argument instead"
	)
	
})


test_that("pasilla custom color discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, type),
			transform = log1p, 
			palette_discrete = list(c("#d80000", "#283cea"))
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla custom color contunuous",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(activation),
			transform = log1p, 
			palette_continuous = list(c("#d80000", "#283cea"))
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla custom color contunuous AND discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, type, activation),
			transform = log1p
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("grouped and annotated plot both vertical and horizontal",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, type, activation)
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pass arguments with ...",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			annotation = c(condition, type, activation),
			show_heatmap_legend = FALSE
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})


test_that("Custom function for fill abundance palette",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("Warning if data sparse",{
	
	expect_equal(
		class(tidyHeatmap::heatmap(
			dplyr::slice(dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"), -1),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`, 
			palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
		))[1],
		"Heatmap"
	)
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
	
	expect_equal(
		class(
			tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count`, 
			transform = log1p	
		))[1],
		"Heatmap"
	)
	
})

test_that("test scale",{
	
	expect_equal(
		class(
			tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				.scale = "row"
			))[1],
		"Heatmap"
	)
	
	expect_equal(
		class(
			tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				.scale = "column"
			))[1],
		"Heatmap"
	)
	
	expect_equal(
		class(
			tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				.scale = "both"
			))[1],
		"Heatmap"
	)
	
	expect_equal(
		class(
			tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				.scale = "none"
			))[1],
		"Heatmap"
	)
	
	expect_error(
		class(
			tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
				.column = UBR, 
				.row = symbol_ct, 
				.value = `read count`, 
				.scale = "WRONG_INPUT"
			))[1],
		"the .scale parameter has to be one of"
	)
	
})
