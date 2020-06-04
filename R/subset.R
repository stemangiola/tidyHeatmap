
# FROM NANNY
get_specific_annotation_columns = function(.data, .col){
	
	
	# Comply with CRAN NOTES
	. = NULL
	
	# Make col names
	.col = enquo(.col)
	
	# x-annotation df
	n_x = .data %>% distinct_at(vars(!!.col)) %>% nrow
	
	# element wise columns
	.data %>%
		select(-!!.col) %>%
		colnames %>%
		map(
			~
				.x %>%
				ifelse_pipe(
					.data %>%
						distinct_at(vars(!!.col, .x)) %>%
						nrow %>%
						equals(n_x),
					~ .x,
					~ NULL
				)
		) %>%
		
		# Drop NULL
		{	(.)[lengths((.)) != 0]	} %>%
		unlist
	
}

# Set internal - FROM NANNY
subset = 		function(.data,
										.column)	{
	# Make col names
	.column = enquo(.column)
	
	# Check if column present
	if(quo_names(.column) %in% colnames(.data) %>% all %>% `!`)
		stop("nanny says: some of the .column specified do not exist in the input data frame.")
	
	.data %>%
		
		# Selecting the right columns
		select(	!!.column,	get_specific_annotation_columns(.data, !!.column)	) %>%
		distinct()
	
}

