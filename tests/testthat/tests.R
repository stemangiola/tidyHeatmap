context('tests')


test_that("...",{

	p = 
		tidyHeatmap::plot_heatmap(
			filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.x = UBR, 
			.y = symbol_ct, 
			.abundance = `read count normalised log`
		)
	
	
  expect_equal(as.character(class(p)), "Heatmap" )

})
