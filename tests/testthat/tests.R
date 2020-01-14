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

test_that("annotated plot",{
	
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

