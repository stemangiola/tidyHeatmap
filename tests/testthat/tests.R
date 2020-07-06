context('tests')


test_that("basic plot",{

	p = 
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		)
	
	
  expect_equal(as.character(class(p)), "InputHeatmap" )

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
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("annotated plot numerical continuous intereg nominal annot",{
	
	p = 
		tidyHeatmap::heatmap(
				dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		) %>%
		add_tile(CAPRA_TOTAL)
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("annotated plot continuous annot MUST ERROR",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	
	expect_error(
		tidyHeatmap::heatmap(
			 left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, sample), a = rnorm(n()))), 
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		) %>% 
			add_tile(a), "Your annotation*", fixed=FALSE) 
	
})

test_that("annotated plot continuous annot as well",{
	
	my_df = dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis")
	
	p = 
		tidyHeatmap::heatmap(
			left_join(my_df,  dplyr::mutate(dplyr::distinct(my_df, UBR), a = rnorm(n(), sd=5))), 
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`
		) %>%
		add_tile(a) %>%
		add_tile(CAPRA_TOTAL)
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
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
		) %>%
		add_tile(CAPRA_TOTAL)
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("grouped double and annotated plot",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) %>%
		add_tile(condition) %>%
		add_tile(activation)
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
	
})

test_that("grouping error",{
	
	expect_error(
		
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type, condition),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) %>%
			add_tile(condition) %>%
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
			.value = `count normalised adjusted`,
			transform = log1p
		)  %>%
		add_tile(condition)
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("pasilla 2 annotations",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			transform = log1p
		) %>%
		add_tile(condition) %>%
		add_tile(type)
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("pasilla custom color abundance",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			transform = log1p, 
			palette_value = c("#d80000", "#ffffff", "#283cea")
		) %>%
		add_tile(condition) %>%
		add_tile(type)
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
	# Test deprecation
	expect_warning(
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			transform = log1p, 
			palette_abundance = c("#d80000", "#ffffff", "#283cea")
		)  %>%
			add_tile(condition) %>%
			add_tile(type),
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
			transform = log1p 
		)  %>%
		add_tile(condition, c("#d80000", "#283cea")) %>%
		add_tile(type)
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("pasilla custom color contunuous",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			transform = log1p
		) %>%
		add_tile(activation, c("#d80000", "#283cea"))
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("pasilla custom color contunuous AND discrete",{
	
	p = 
		tidyHeatmap::heatmap(
			tidyHeatmap::pasilla,
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			transform = log1p
		) %>%
		add_tile(condition) %>%
		add_tile(type) %>%
		add_tile(activation) 
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("grouped and annotated plot both vertical and horizontal",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) %>%
		add_tile(condition) %>%
		add_tile(type) %>%
		add_tile(activation) 
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("pass arguments with ...",{
	
	p = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`,
			show_heatmap_legend = FALSE
		) %>%
		add_tile(condition) %>%
		add_tile(type) %>%
		add_tile(activation) 
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
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
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
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
		"InputHeatmap"
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
		"InputHeatmap"
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
		"InputHeatmap"
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
		"InputHeatmap"
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
		"InputHeatmap"
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
		"InputHeatmap"
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

test_that("multi-type",{
	
	library(magrittr)
	
	p = 
		dplyr::group_by(tidyHeatmap::pasilla,		location, type) %>%
		dplyr::mutate(act = activation) %>% 
		tidyr::nest(data = -sample) %>%
		dplyr::mutate(size = rnorm(n(), 4,0.5)) %>%
		dplyr::mutate(age = runif(n(), 50, 200)) %>%
		tidyr::unnest(data) %>%
		tidyHeatmap::heatmap(
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		) %>%
		add_tile(condition) %>%
		add_point(activation) %>%
		add_tile(act) %>%
		add_bar(size) %>%
		add_line(age)
	
	
	expect_equal(as.character(class(p)), "InputHeatmap" )
	
})

test_that("save_pdf",{
	
	library(magrittr)
	
	filename = tempfile()
		
	tidyHeatmap::heatmap(
		dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
		.column = UBR, 
		.row = symbol_ct, 
		.value = `read count normalised log`
	) %>%
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
	
	expect_equal(length(p2@palette_discrete), l1-1 )
	
	p3 = 
		tidyHeatmap::heatmap(
			dplyr::group_by(tidyHeatmap::pasilla,		location, type),
			.column = sample,
			.row = symbol,
			.value = `count normalised adjusted`
		)
	
	expect_equal(length(p3@palette_discrete), length(p2@palette_discrete)-1 )
	
	p4 =
		p3 %>%
		add_tile(condition) %>%
		add_tile(activation)
	
	expect_equal(length(p4@palette_discrete), length(p3@palette_discrete)-1 )
	
	p5 =
		p1 %>%
		add_tile(condition) %>%
		add_tile(activation)
	
	expect_equal(length(p5@palette_discrete), length(p1@palette_discrete)-1 )
	expect_equal(length(p5@palette_continuous), length(p1@palette_continuous)-1 )
	
})

test_that("annotated plot numerical continuous intereg nominal annot",{
	
	expect_warning(
		tidyHeatmap::heatmap(
			dplyr::filter(tidyHeatmap::N52, Category == "Angiogenesis"),
			.column = UBR, 
			.row = symbol_ct, 
			.value = `read count normalised log`,
			annotation = CAPRA_TOTAL
		), "Please use the new annotation framework instead"
	)
	

})
