
# FROM NANNY
get_specific_annotation_columns = function(.data, .col){
	
	
	# Comply with CRAN NOTES
	. = NULL
	
	# Make col names
	.col = enquo(.col)
	
	# x-annotation df
	n_x = .data |> select(!!.col) |> distinct() |> nrow()
	
	# element wise columns
	result <- .data |>
		select(-!!.col) |>
		colnames() |>
		map(
			~
				{
					is_valid = 
						.data |>
						  select(!!.col, all_of(.x)) |> 
							distinct() |>
							nrow() |>
							equals(n_x)
					if (is_valid) .x else NULL
				}
		)
	
	# Drop NULL
	result[lengths(result) != 0] |> unlist()

	
}

# Set internal - FROM NANNY
subset = 		function(.data,
										.column)	{
	# Make col names
	.column = enquo(.column)
	
	# Check if column present
if((quo_names(.column) %in% colnames(.data)) |> all() |> not())
		stop("nanny says: some of the .column specified do not exist in the input data frame.")
	
	.data |>
		
		# Selecting the right columns
		select(	!!.column,	all_of(get_specific_annotation_columns(.data, !!.column)	)) |>
		distinct()
	
}

