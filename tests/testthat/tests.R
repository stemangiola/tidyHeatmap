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