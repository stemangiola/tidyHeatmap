context('tests')


test_that("basic plot",{

	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.horizontal = UBR, 
			.vertical = symbol_ct, 
			.abundance = `read count normalised log`
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
			.horizontal = UBR, 
			.vertical = symbol_ct, 
			.abundance = `read count normalised log`
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("annotated plot numerical continuous intereg nominal annot",{
	
	p = 
		tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.horizontal = UBR, 
			.vertical = symbol_ct, 
			.abundance = `read count normalised log`,
			annotation = CAPRA_TOTAL
		)
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("annotated plot continuous annot MUST ERROR",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	
	expect_error(
		tidyHeatmap::heatmap(
			 left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, sample), a = rnorm(n()))), 
			.horizontal = UBR, 
			.vertical = symbol_ct, 
			.abundance = `read count normalised log`,
			annotation = a
		), "Your annotation*", fixed=FALSE) 
	
})

test_that("annotated plot continuous annot as well",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	
	p = 
		tidyHeatmap::heatmap(
			left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, UBR), a = rnorm(n(), sd=5))), 
			.horizontal = UBR, 
			.vertical = symbol_ct, 
			.abundance = `read count normalised log`,
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
			.horizontal = UBR, 
			.vertical = symbol_ct, 
			.abundance = `read count normalised log`,
			annotation = CAPRA_TOTAL
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla one annotation",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.horizontal = sample,
			.vertical = symbol,
			.abundance = `count normalised adjusted`,
			annotation = condition,
			log_transform = TRUE
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla 2 annotations",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.horizontal = sample,
			.vertical = symbol,
			.abundance = `count normalised adjusted`,
			annotation = c(condition, type),
			log_transform = TRUE
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla custom color abundance",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.horizontal = sample,
			.vertical = symbol,
			.abundance = `count normalised adjusted`,
			annotation = c(condition, type),
			log_transform = TRUE, 
			palette_abundance = c("#d80000", "#ffffff", "#283cea")
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})


test_that("pasilla custom color discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.horizontal = sample,
			.vertical = symbol,
			.abundance = `count normalised adjusted`,
			annotation = c(condition, type),
			log_transform = TRUE, 
			palette_discrete = list(c("#d80000", "#283cea"))
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla custom color contunuous",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.horizontal = sample,
			.vertical = symbol,
			.abundance = `count normalised adjusted`,
			annotation = c(activation),
			log_transform = TRUE, 
			palette_continuous = list(c("#d80000", "#283cea"))
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla custom color contunuous AND discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.horizontal = sample,
			.vertical = symbol,
			.abundance = `count normalised adjusted`,
			annotation = c(condition, type, activation),
			log_transform = TRUE
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("grouped and annotated plot both vertical and horizontal",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.horizontal = sample,
			.vertical = symbol,
			.abundance = `count normalised adjusted`,
			annotation = c(condition, type, activation)
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pass arguments with ...",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.horizontal = sample,
			.vertical = symbol,
			.abundance = `count normalised adjusted`,
			annotation = c(condition, type, activation),
			column_names_gp = gpar(fontsize = 8)
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})


test_that("Custom function for fill abundance palette",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.horizontal = UBR, 
			.vertical = symbol_ct, 
			.abundance = `read count normalised log`, 
			palette_abundance = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})