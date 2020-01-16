context('tests')


test_that("basic plot",{

	p = 
		tidyHeatmap::plot_heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.horizontal = UBR, 
			.vertical = symbol_ct, 
			.abundance = `read count normalised log`
		)
	
	
  expect_equal(as.character(class(p)), "Heatmap" )

})

test_that("grouped plot",{
	
	p = 
		tidyHeatmap::plot_heatmap(
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
		tidyHeatmap::plot_heatmap(
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
		tidyHeatmap::plot_heatmap(
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
		tidyHeatmap::plot_heatmap(
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
		tidyHeatmap::plot_heatmap(
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
		tidyHeatmap::plot_heatmap(
			tidyHeatmap::pasilla,
			.horizontal = sample,
			.vertical = transcript,
			.abundance = `Log adj count`,
			annotation = condition
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

test_that("pasilla 2 annotations",{
	
	p = 
		tidyHeatmap::plot_heatmap(
			tidyHeatmap::pasilla,
			.horizontal = sample,
			.vertical = transcript,
			.abundance = `Log adj count`,
			annotation = c(condition, type)
		)
	
	
	expect_equal(as.character(class(p)), "Heatmap" )
	
})

