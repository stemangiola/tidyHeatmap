#' This is a generalisation of ifelse that accepts an object and return an objects
#'
#' @import dplyr
#' @import tidyr
#' @importFrom purrr as_mapper
#' @importFrom tidyr replace_na
#'
#' @param .x A tibble
#' @param .p A boolean
#' @param .f1 A function
#' @param .f2 A function
#'
#' @return A tibble
ifelse_pipe = function(.x, .p, .f1, .f2 = NULL) {
  switch(.p %>% not%>% sum(1),
         as_mapper(.f1)(.x),
         if (.f2 %>% is.null %>% not)
           as_mapper(.f2)(.x)
         else
           .x)
  
}

#' This is a generalisation of ifelse that accepts an object and return an objects
#'
#' @import dplyr
#' @import tidyr
#'
#' @param .x A tibble
#' @param .p1 A boolean
#' @param .p2 ELSE IF condition
#' @param .f1 A function
#' @param .f2 A function
#' @param .f3 A function
#'
#' @return A tibble
ifelse2_pipe = function(.x, .p1, .p2, .f1, .f2, .f3 = NULL) {
  # Nested switch
  switch(# First condition
    .p1 %>% not%>% sum(1),
    
    # First outcome
    as_mapper(.f1)(.x),
    switch(
      # Second condition
      .p2 %>% not %>% sum(1),
      
      # Second outcome
      as_mapper(.f2)(.x),
      
      # Third outcome - if there is not .f3 just return the original data frame
      if (.f3 %>% is.null %>% not)
        as_mapper(.f3)(.x)
      else
        .x
    ))
}

#' Get matrix from tibble
#'
#' @import dplyr
#' @import tidyr
#' @importFrom magrittr set_rownames
#' @importFrom rlang quo_is_null
#'
#' @param tbl A tibble
#' @param rownames A character string of the rownames
#' @param do_check A boolean
#'
#' @return A matrix
#'
#'
as_matrix <- function(tbl,
                      rownames = NULL,
                      do_check = TRUE) {
  
  # Comply with CRAN NOTES
  variable = NULL
  
  rownames = enquo(rownames)
  tbl %>%
    
    # Through warning if data frame is not numerical beside the rownames column (if present)
    ifelse_pipe(
      do_check &&
        tbl %>%
        # If rownames defined eliminate it from the data frame
        ifelse_pipe(!quo_is_null(rownames), ~ .x[, -1], ~ .x) %>%
        dplyr::summarise_all(class) %>%
        tidyr::gather(variable, class) %>%
        pull(class) %>%
        unique() %>%
        `%in%`(c("numeric", "integer")) %>% not() %>% any(),
      ~ {
        warning("to_matrix says: there are NON-numerical columns, the matrix will NOT be numerical")
        .x
      }
    ) %>%
    as.data.frame() %>%
    
    # Deal with rownames column if present
    ifelse_pipe(
      !quo_is_null(rownames),
      ~ .x %>%
        magrittr::set_rownames(tbl %>% pull(!!rownames)) %>%
        select(-1)
    ) %>%
    
    # Convert to matrix
    as.matrix()
}

#' Check whether a numeric vector has been log transformed
#'
#' @param x A numeric vector
#' @param .abundance A character name of the transcript/gene abundance column
#'
#' @return NA
error_if_log_transformed <- function(x, .abundance) {
  
  # Comply with CRAN NOTES
  m = NULL
  
  .abundance = enquo(.abundance)
  
  if (x %>% nrow %>% gt(0))
    if (x %>% summarise(m = !!.abundance %>% max) %>% pull(m) < 50)
      stop(
        "tidyHeatmap says: The input was log transformed, this algorithm requires raw (un-normalised) read counts"
      )
}

#' .formula parser
#'
#' @importFrom stats terms
#'
#' @param fm a formula
#' @return A character vector
#'
#'
parse_formula <- function(fm) {
  if (attr(terms(fm), "response") == 1)
    stop("tidyHeatmap says: The .formula must be of the kind \"~ covariates\" ")
  else
    as.character(attr(terms(fm), "variables"))[-1]
}

# #' Scale design matrix
# #'
# #' @importFrom rlang set_names
# #' @importFrom stats cov
# #'
# #' @param df A tibble
# #' @param .formula a formula
# #'
# #' @return A tibble
# #'
# #'
# scale_design = function(df, .formula) {
  # 
 #  # Comply with CRAN NOTES
 #  value = sample_idx = `(Intercept)` =  NULL
 #  
 #  df %>%
 #    set_names(c("sample_idx", "(Intercept)", parse_formula(.formula))) %>%
 #    gather(cov, value,-sample_idx) %>%
 #    group_by(cov) %>%
 #    mutate(value = ifelse(
 #      !grepl("Intercept", cov) &
 #        length(union(c(0, 1), value)) != 2,
 #      scale(value),
 #      value
 #    )) %>%
 #    ungroup() %>%
 #    pivot_wider(names_from = cov, values_from = value) %>%
 #    arrange(as.integer(sample_idx)) %>%
 #    select(`(Intercept)`, any_of(parse_formula(.formula)))
 # }

#' Add attribute to abject
#'
#'
#' @param var A tibble
#' @param attribute An object
#' @param name A character name of the attribute
#'
#' @return A tibble with an additional attribute
add_attr = function(var, attribute, name) {
  attr(var, name) <- attribute
  var
}

#' Remove class to abject
#'
#'
#' @param var A tibble
#' @param name A character name of the class
#'
#' @return A tibble with an additional attribute
drop_class = function(var, name) {
  class(var) <- class(var)[!class(var)%in%name]
  var
}

#' From rlang deprecated
#'
#' @param x An array
#' @param values An array
#' @param before A boolean
#'
#' @return An array
#'
prepend = function (x, values, before = 1)
{
  n <- length(x)
  stopifnot(before > 0 && before <= n)
  if (before == 1) {
    c(values, x)
  }
  else {
    c(x[1:(before - 1)], values, x[before:n])
  }
}

#' Add class to abject
#'
#' @param var A tibble
#' @param name A character name of the attribute
#'
#' @return A tibble with an additional attribute
add_class = function(var, name) {
  
  class(var) <- prepend(class(var),name)
  
  var
}

#' Get column names either from user or from attributes
#'
#' @importFrom rlang quo_is_symbol
#'
#' @param .data A tibble
#' @param .sample A character name of the sample column
#' @param .transcript A character name of the transcript/gene column
#' @param .abundance A character name of the read count column
#'
#' @return A list of column enquo or error
get_sample_transcript_counts = function(.data, .sample, .transcript, .abundance){
  
  
  my_stop = function() {
    stop("
        tidyHeatmap says: The function does not know what your sample, transcript and counts columns are.\n
        You have to either enter those as symbols (e.g., `sample`), \n
        or use the funtion create_tt_from_tibble() to pass your column names that will be remembered.
      ")
  }
  
  if( .sample %>% quo_is_symbol() ) .sample = .sample
  else if(".sample" %in% (.data %>% attr("parameters") %>% names))
    .sample =  attr(.data, "parameters")$.sample
  else my_stop()
  
  if( .transcript %>% quo_is_symbol() ) .transcript = .transcript
  else if(".transcript" %in% (.data %>% attr("parameters") %>% names))
    .transcript =  attr(.data, "parameters")$.transcript
  else my_stop()
  
  if( .abundance %>% quo_is_symbol() ) .abundance = .abundance
  else if(".abundance" %in% (.data %>% attr("parameters") %>% names))
    .abundance = attr(.data, "parameters")$.abundance
  else my_stop()
  
  list(.sample = .sample, .transcript = .transcript, .abundance = .abundance)
  
}

#' Get column names either from user or from attributes
#'
#' @importFrom rlang quo_is_symbol
#'
#' @param .data A tibble
#' @param .sample A character name of the sample column
#' @param .abundance A character name of the read count column
#'
#' @return A list of column enquo or error
get_sample_counts = function(.data, .sample, .abundance){
  
  
  my_stop = function() {
    stop("
        tidyHeatmap says: The function does not know what your sample, transcript and counts columns are.\n
        You have to either enter those as symbols (e.g., `sample`), \n
        or use the funtion create_tt_from_tibble() to pass your column names that will be remembered.
      ")
  }
  
  if( .sample %>% quo_is_symbol() ) .sample = .sample
  else if(".sample" %in% (.data %>% attr("parameters") %>% names))
    .sample =  attr(.data, "parameters")$.sample
  else my_stop()
  
  if( .abundance %>% quo_is_symbol() ) .abundance = .abundance
  else if(".abundance" %in% (.data %>% attr("parameters") %>% names))
    .abundance = attr(.data, "parameters")$.abundance
  else my_stop()
  
  list(.sample = .sample, .abundance = .abundance)
  
}

#' Get column names either from user or from attributes
#'
#' @importFrom rlang quo_is_symbol
#'
#' @param .data A tibble
#' @param .sample A character name of the sample column
#' @param .transcript A character name of the transcript/gene column
#'
#' @return A list of column enquo or error
get_sample_transcript = function(.data, .sample, .transcript){
  
  
  my_stop = function() {
    stop("
        tidyHeatmap says: The function does not know what your sample, transcript and counts columns are.\n
        You have to either enter those as symbols (e.g., `sample`), \n
        or use the funtion create_tt_from_tibble() to pass your column names that will be remembered.
      ")
  }
  
  if( .sample %>% quo_is_symbol() ) .sample = .sample
  else if(".sample" %in% (.data %>% attr("parameters") %>% names))
    .sample =  attr(.data, "parameters")$.sample
  else my_stop()
  
  if( .transcript %>% quo_is_symbol() ) .transcript = .transcript
  else if(".transcript" %in% (.data %>% attr("parameters") %>% names))
    .transcript =  attr(.data, "parameters")$.transcript
  else my_stop()
  
  
  list(.sample = .sample, .transcript = .transcript)
  
}


#' Get column names either from user or from attributes
#'
#' @importFrom rlang quo_is_symbol
#'
#' @param .data A tibble
#' @param .element A character name of the sample column
#' @param .feature A character name of the transcript/gene column
#' @param of_samples A boolean
#'
#' @return A list of column enquo or error
#'
get_elements_features = function(.data, .element, .feature, of_samples = TRUE){
  
  # If setted by the user, enquo those
  if(
    .element %>% quo_is_symbol() &
    .feature %>% quo_is_symbol()
  )
    return(list(
      .element = .element,
      .feature = .feature
    ))
  
  # Otherwise check if attribute exists
  else {
    
    # If so, take them from the attribute
    if(.data %>% attr("parameters") %>% is.null %>% not)
      
      return(list(
        .element =  switch(
          of_samples %>% not %>% sum(1),
          attr(.data, "parameters")$.sample,
          attr(.data, "parameters")$.transcript
        ),
        .feature = switch(
          of_samples %>% not %>% sum(1),
          attr(.data, "parameters")$.transcript,
          attr(.data, "parameters")$.sample
        )
      ))
    # Else through error
    else
      stop("
        tidyHeatmap says: The function does not know what your elements (e.g., sample) and features (e.g., transcripts) are.\n
        You have to either enter those as symbols (e.g., `sample`), \n
        or use the funtion create_tt_from_tibble() to pass your column names that will be remembered.
      ")
  }
}

#' Get column names either from user or from attributes
#'
#' @importFrom rlang quo_is_symbol
#'
#' @param .data A tibble
#' @param .element A character name of the sample column
#' @param .feature A character name of the transcript/gene column
#' @param .abundance A character name of the read count column

#' @param of_samples A boolean
#'
#' @return A list of column enquo or error
#'
get_elements_features_abundance = function(.data, .element, .feature, .abundance, of_samples = TRUE){
  
  my_stop = function() {
    stop("
        tidyHeatmap says: The function does not know what your elements (e.g., sample) and features (e.g., transcripts) are.\n
        You have to either enter those as symbols (e.g., `sample`), \n
        or use the funtion create_tt_from_tibble() to pass your column names that will be remembered.
      ")
  }
  
  if( .element %>% quo_is_symbol() ) .element = .element
  else if(of_samples & ".sample" %in% (.data %>% attr("parameters") %>% names))
    .element =  attr(.data, "parameters")$.sample
  else if((!of_samples) & ".transcript" %in% (.data %>% attr("parameters") %>% names))
    .element =  attr(.data, "parameters")$.transcript
  else my_stop()
  
  if( .feature %>% quo_is_symbol() ) .feature = .feature
  else if(of_samples & ".transcript" %in% (.data %>% attr("parameters") %>% names))
    .feature =  attr(.data, "parameters")$.transcript
  else if((!of_samples) & ".sample" %in% (.data %>% attr("parameters") %>% names))
    .feature =  attr(.data, "parameters")$.sample
  else my_stop()
  
  if( .abundance %>% quo_is_symbol() ) .abundance = .abundance
  else if(".abundance" %in% (.data %>% attr("parameters") %>% names))
    .abundance = attr(.data, "parameters")$.abundance
  else my_stop()
  
  list(.element = .element, .feature = .feature, .abundance = .abundance)
}

#' Get column names either from user or from attributes
#'
#' @importFrom rlang quo_is_symbol
#'
#' @param .data A tibble
#' @param .element A character name of the sample column
#' @param of_samples A boolean
#'
#' @return A list of column enquo or error
get_elements = function(.data, .element, of_samples = TRUE){
  
  # If setted by the user, enquo those
  if(
    .element %>% quo_is_symbol()
  )
    return(list(
      .element = .element
    ))
  
  # Otherwise check if attribute exists
  else {
    
    # If so, take them from the attribute
    if(.data %>% attr("parameters") %>% is.null %>% not)
      
      return(list(
        .element =  switch(
          of_samples %>% not %>% sum(1),
          attr(.data, "parameters")$.sample,
          attr(.data, "parameters")$.transcript
        )
      ))
    # Else through error
    else
      stop("
        tidyHeatmap says: The function does not know what your elements (e.g., sample) are.\n
        You have to either enter those as symbols (e.g., `sample`), \n
        or use the funtion create_tt_from_tibble() to pass your column names that will be remembered.
      ")
  }
}

#' Get column names either from user or from attributes
#'
#' @importFrom rlang quo_is_symbol
#' @importFrom magrittr %$%
#'
#' @param .data A tibble
#' @param .abundance A character name of the abundance column
#'
#' @return A list of column enquo or error
get_abundance_norm_if_exists = function(.data, .abundance){
  
  # Comply with CRAN NOTES
  .abundance_norm = NULL
  
  # If setted by the user, enquo those
  if(
    .abundance %>% quo_is_symbol()
  )
    return(list(
      .abundance = .abundance
    ))
  
  # Otherwise check if attribute exists
  else {
    
    # If so, take them from the attribute
    if(.data %>% attr("parameters") %>% is.null %>% not)
      
      return(list(
        .abundance =  switch(
          (".abundance_norm" %in% (.data %>% attr("parameters") %>% names) &
             quo_name(.data %>% attr("parameters") %$% .abundance_norm) %in% (.data %>% colnames)
          ) %>% not %>% sum(1),
          attr(.data, "parameters")$.abundance_norm,
          attr(.data, "parameters")$.abundance
        )
      ))
    # Else through error
    else
      stop("
        tidyHeatmap says: The function does not know what your elements (e.g., sample) are.\n
        You have to either enter those as symbols (e.g., `sample`), \n
        or use the funtion create_tt_from_tibble() to pass your column names that will be remembered.
      ")
  }
}

#' Sub function of remove_redundancy_elements_though_reduced_dimensions
#'
#' @importFrom stats dist
#' @importFrom utils head
#'
#' @param df A tibble
#'
#'
#' @return A tibble with pairs to drop
select_closest_pairs = function(df) {
  
  # Comply with CRAN NOTES
  `sample 1` = `sample 2` =  NULL
  
  couples <- df %>% head(n = 0)
  
  while (df %>% nrow() > 0) {
    pair <- df %>%
      arrange(dist) %>%
      head(n = 1)
    couples <- couples %>% bind_rows(pair)
    df <- df %>%
      filter(
        !`sample 1` %in% (pair %>% select(1:2) %>% as.character()) &
          !`sample 2` %in% (pair %>% select(1:2) %>% as.character())
      )
  }
  
  couples
  
}

#' get_x_y_annotation_columns
#' 
#' @importFrom magrittr equals
#' @importFrom purrr pmap
#' 
#' @param .data A `tbl` formatted as | <SAMPLE> | <TRANSCRIPT> | <COUNT> | <...> |
#' @param .column The name of the column horizontally presented in the heatmap
#' @param .row The name of the column vertically presented in the heatmap
#' @param .abundance The name of the transcript/gene abundance column
#' 
#' @return A list
#' 
get_x_y_annotation_columns = function(.data, .column, .row, .abundance){
  
  # Comply with CRAN NOTES
  . = NULL
  value = NULL
  orientation = NULL
  col_name = NULL
  
  # Make col names
  .column = enquo(.column)
  .row = enquo(.row)
  .abundance = enquo(.abundance)
  
  .data %>%
    select_if(negate(is.list)) %>%
    ungroup() %>%
    {
      # Rows
      bind_rows(
        (.) %>% subset(!!.column) %>% colnames %>% as_tibble %>% rename(column = value) %>% gather(orientation, col_name),
        (.) %>% subset(!!.row) %>% colnames %>% as_tibble %>% rename(row = value) %>% gather(orientation, col_name)
      )
    }
}

#' @importFrom purrr map_chr
ct_colors = function(ct) 
  ct %>%
  as.character() %>%
  map_chr(
    ~ switch(
      .x,
      "E" = "#199E78",
      "F" = "#D96013",
      "M" = "#7571B3",
      "T" = "#E52E89"
    )
  )

#' @importFrom ComplexHeatmap anno_points
#' @importFrom ComplexHeatmap anno_barplot
#' @importFrom ComplexHeatmap anno_lines
#' @importFrom ComplexHeatmap anno_numeric
type_to_annot_function = list(
  "tile" = NULL, #anno_simple, 
  "point" = anno_points, 
  "bar" = anno_barplot, 
  "line" = anno_lines,
  "numeric" = anno_numeric
)

get_top_left_annotation = function(.data_, .column, .row, .abundance, annotation, palette_annotation, type, x_y_annot_cols, size, ...){
  
  # Comply with CRAN NOTES 
  data = NULL
  fx = NULL
  annot = NULL
  annot_type = NULL
  idx = NULL
  value = NULL
  orientation = NULL
  col_name = NULL
  col_orientation = NULL
  my_function = NULL
  
  .column = enquo(.column) 
  .row = enquo(.row) 
  .abundance = enquo(.abundance)
  annotation = enquo(annotation)
  
  dots_args = rlang::dots_list(...)
  
  annotation_function = type_to_annot_function[type]
  
  # Create dataset
  df = 
    quo_names(annotation) %>%
	  as_tibble %>%
	  rename(col_name = value) %>%
	  
	  # delete if annotation is NULL
	  when(quo_is_null(annotation) ~ slice(., 0), ~ (.)) %>%
	  
	  # Add orientation
	  left_join(x_y_annot_cols,  by = "col_name") %>%
	  mutate(col_orientation = map_chr(orientation, ~ .x %>% when((.) == "column" ~ quo_name(.column), (.) == "row" ~ quo_name(.row)))) 


  return_factor_ordering_by_col_or_row_names <- function(.data, col, orient) {
    .data %>%
      ungroup() %>%
      select(all_of(c(orient, col))) %>%
      distinct() %>% 
      arrange(!!as.symbol(orient)) %>%
      select(all_of(col)) %>% 
      pull(1)
  }

  
  # Add data
  df = 
    df %>%
    mutate( data = map2(col_name,  col_orientation, ~ return_factor_ordering_by_col_or_row_names(!!.data_, .x, .y) ) ) 
  
  # Check and handle NA/NaN values in data before processing
  df = df %>%
    mutate(data = map(data, ~ {
      if (length(.x) > 0 && any(is.na(.x) | is.nan(.x))) {
        warning("tidyHeatmap says: You have NA/NaN values in your annotation data. These will be replaced with 'NA'.")
        replace_na(.x, "NA")
      } else {
        .x
      }
    }))
  
  df = df %>%
	    
	  # Add function
	  mutate(my_function = annotation_function) %>%
	  mutate(type = !!type) |> 
	
	  	
	  # Apply annot function if not NULL otherwise pass original annotation
	  # This because no function for ComplexHeatmap = to tile
	  mutate(annot = pmap(list(data, type, orientation), ~  {

	    # Suppose ..1 = data, ..2 = type, ..3 = orientation
	    # dots_args is your captured ellipsis list, e.g. dots_args <- list(border=TRUE, gp=gpar(...))
	    if(..2 == "tile") {
	      # "tile" just returns the original data (..1)
	      return(..1)
	    } else {
	      # Match annotation "type" to the correct ComplexHeatmap function
	      ann_fun <- switch(
	        ..2,
	        "point" = anno_points,
	        "bar"   = anno_barplot,
	        "line"  = anno_lines,
	        "numeric" = anno_numeric,
	        stop("Unsupported annotation type: ", ..2)
	      )
	      
	      # Build the argument list for do.call
	      # If you truly need different dimension args for row vs column
	      # (e.g. width= vs. height=), handle that logic here.
	      call_args <-  list(x     = ..1, which = ..3)
	      
	      # anno_numeric does not have height argument
	      if(..2 %in% c("point","bar","line")) call_args = call_args |> c(list(height = size))
	      else if(..2 %in% c("numeric")) call_args = call_args |> c(list(width = size))
	      
	      if(..2 %in% c("numeric") & !"bg_gp" %in% names(dots_args))
	        call_args = call_args |> c(list(bg_gp = gpar(fill = "grey70", col = NA)))   
	      
	      call_args = call_args |> c(dots_args |> filter_args(ann_fun))

	      
	      # Invoke the correct anno_* function with do.call
	      return(do.call(ann_fun, call_args))
	    }

	    
  },
  # Pass dots here so they're available inside the inner function
  dots_args
  )) 
  
  df = df %>%
	  
		# Add color indexes separately for each orientation
		mutate(annot_type = map_chr(annot, ~ .x %>% when(class(.) %in% c("factor", "character", "logical") ~ "discrete",
																										class(.) %in% c("integer", "numerical", "numeric", "double") ~ "continuous",
																										~ "other"
		) )) %>%
		group_by(annot_type) %>%
		mutate(idx =  row_number()) %>%
		ungroup() %>%
  	
		mutate(color = map2(annot, idx,  ~ {
			if(.x %>% class %in% c("factor", "character", "logical")){
				
				# If is colorRamp 
				if(is(palette_annotation$discrete[[.y]], "function"))
					palette_annotation$discrete[[.y]]
				
				# If it is a list of colors
				else
				    if (is(.x, "factor")) {
				      palette_annotation$discrete[[.y]] |> _[seq_len(length(levels(.x)))] %>% set_names(levels(.x))

				    } else {
				      colorRampPalette(palette_annotation$discrete[[.y]])(length(unique(.x))) %>% set_names(unique(.x))
				    }			
			  
			} else if (.x %>% class %in% c("integer", "numerical", "numeric", "double")){
				
				# If is colorRamp 
				if(is(palette_annotation$continuous[[.y]], "function"))
					palette_annotation$continuous[[.y]]

				# If it is a list of colors
				else
					colorRampPalette(palette_annotation$continuous[[.y]])(length(.x)) %>% colorRamp2(seq(min(.x), max(.x), length.out = length(.x)), .)
				
			}
			else NULL
		})) 
  
  df = df %>%
	  	
	  mutate(further_arguments = map2(
	  	col_name, my_function,
	  	~ dots_args %>% 
	  		
	  		# If tile add size as further argument
	  		when(!is_function(.y) ~ c(., list(simple_anno_size = size)), ~ (.))
	  		
	  )) %>% 	
	  
	  # Stop if annotations discrete bigger than palette
	  when(
	    (.) %>%  pull(data) %>% map_chr(~ .x %>% class) %in% 
	      c("factor", "character") %>% which %>% length %>%
	      gt(palette_annotation$discrete %>% length) ~
	      stop("tidyHeatmap says: Your discrete annotaton columns are bigger than the palette available"),
	    ~ (.)
	  ) %>%
	  
	  # Stop if annotations continuous bigger than palette
	  when(
	    (.) %>%  pull(data) %>% map_chr(~ .x %>% class) %in% 
	      c("int", "dbl", "numeric") %>% which %>% length %>%
	      gt( palette_annotation$continuous %>% length) ~
	      stop("tidyHeatmap says: Your continuous annotaton columns are bigger than the palette available"),
	    ~ (.)
	  )
      
  
}

#' @importFrom grid unit
#' @importFrom ComplexHeatmap anno_block
#' @importFrom rlang set_names
get_group_annotation = function(
  .data, .column, .row, .abundance, palette_annotation,
  group_label_fontsize = 8,
  show_group_name = TRUE,
  group_strip_height = unit(9, "pt")
) {
  
  # Comply with CRAN NOTES
  data = NULL
  . = NULL
  orientation = NULL
  
  # Make col names
  .column = enquo(.column)
  .row = enquo(.row)
  .abundance = enquo(.abundance)

  # Setup default NULL
  top_annotation = list()
  left_annotation = list()
  row_split = NULL
  col_split = NULL
  
  # Column groups
  col_group = get_grouping_columns(.data)
  
  # Data frame of column orientation
  x_y_annot_cols = .data %>% get_x_y_annotation_columns(!!.column,!!.row,!!.abundance) 
  
  
  x_y_annotation_cols = 
    x_y_annot_cols %>%
    nest(data = -orientation) %>%
    mutate(data = map(data, ~ .x %>% pull(1))) %>%
    {
      df = (.)
      pull(df, data) %>% set_names(pull(df, orientation))
    } %>%
    map(
      ~ .x %>% intersect(col_group)
    )
   
  # Check if you have more than one grouping, at the moment just one is accepted
  if(x_y_annotation_cols %>% lapply(length) %>% unlist %>% max %>% gt(1))
    stop("tidyHeatmap says: At the moment just one grouping per dimension (max 1 row and 1 column) is supported.")
  
  # Check if annotation not specific to row or columns
  if(x_y_annotation_cols %>% unlist() %>% duplicated() %>% any())
  	stop(sprintf("tidyHeatmap says: the grouping %s is not specific to row or columns. Maybe you just have one grouping.", x_y_annotation_cols %>% unlist() %>% .[x_y_annotation_cols %>% unlist() %>% duplicated()]))
  
  if(length(x_y_annotation_cols$row) > 0){
    
    # Row split
    row_split = 
      .data %>%
      ungroup() %>%
      distinct(!!.row, !!as.symbol(x_y_annotation_cols$row)) %>%
      arrange(!!.row) %>%
      pull(!!as.symbol(x_y_annotation_cols$row))
    
    # Handle NA/NaN values in row_split
    if (any(is.na(row_split) | is.nan(row_split))) {
      warning("tidyHeatmap says: You have NA/NaN values in your row grouping column. These will be replaced with 'NA'.")
      row_split = replace_na(row_split, "NA")
    }
    
    # Create array of colours
    palette_fill_row = 
      colorRampPalette(
      palette_annotation[[1]][
        
        # If too long get the max length of palette
        1:min(length(unique(row_split)), length(palette_annotation[[1]]))
      ])(
        # Extend colours arbitrarily
        length(unique(row_split))
      ) %>%
      set_names(unique(row_split))
    
    # Old simple method
    #palette_annotation[[1]][1:length(unique(row_split))] %>% set_names(unique(row_split))
    
    palette_text_row =  if_else(palette_fill_row %in% c("#FFFFFF", "white"), "#161616", "#ffffff")
  
    left_annotation_args = 
      setNames(
        list(
          anno_block(  
            gp = gpar(fill = palette_fill_row ),
            labels = row_split %>% unique %>% sort,
            labels_gp = gpar(col = palette_text_row, fontsize = group_label_fontsize),
            which = "row",
            width = group_strip_height,
            show_name = show_group_name
          )
        ),
        x_y_annotation_cols$row
      )
    
    left_annotation = as.list(left_annotation_args)
    
    # Eliminate palette
    palette_annotation = palette_annotation[-1]
    
    }
    
    if(length(x_y_annotation_cols$column) > 0){
      # Col split
      col_split = 
        .data %>%
        ungroup() %>%
        distinct(!!.column, !!as.symbol(x_y_annotation_cols$column)) %>%
        arrange(!!.column) %>%
        pull(!!as.symbol(x_y_annotation_cols$column))
      
      # Handle NA/NaN values in col_split
      if (any(is.na(col_split) | is.nan(col_split))) {
        warning("tidyHeatmap says: You have NA/NaN values in your column grouping column. These will be replaced with 'NA'.")
        col_split = replace_na(col_split, "NA")
      }
      
      # Create array of colours
      palette_fill_column = 
        colorRampPalette(
          palette_annotation[[1]][
            
            # If too long get the max length of palette
            1:min(length(unique(col_split)), length(palette_annotation[[1]]))
          ])(
            # Extend colours arbitrarily
            length(unique(col_split))
          ) %>%
        set_names(unique(col_split))
      
      # Old simple method
      #palette_annotation[[1]][1:length(unique(col_split))] %>% set_names(unique(col_split))
  
      palette_text_column =  if_else(palette_fill_column %in% c("#FFFFFF", "white"), "#161616", "#ffffff")
      
      
      top_annotation_args = 
        setNames(
          list(
            anno_block(  
              gp = gpar(fill = palette_fill_column ),
              labels = col_split %>% unique %>% sort,
              labels_gp = gpar(col = palette_text_column, fontsize = group_label_fontsize),
              which = "column",
              height = group_strip_height,
              show_name = show_group_name
            )
          ),
          x_y_annotation_cols$column
        )
      
       top_annotation = as.list(top_annotation_args)
    }
  
  
  # Return
  list( left_annotation = left_annotation, row_split = row_split, top_annotation = top_annotation, col_split = col_split )
}

# get_group_annotation_OPTIMISED_NOT_FINISHED = function(.data, .column, .row, .abundance, palette_annotation){
#   
#   # Fix CRAN notes
#   value = NULL
#   col_name = NULL
#   col_orientation = NULL
#   annotation_function = NULL
#   
#   # Comply with CRAN NOTES
#   data = NULL
#   . = NULL
#   orientation = NULL
#   
#   # Make col names
#   .column = enquo(.column)
#   .row = enquo(.row)
#   .abundance = enquo(.abundance)
#   
#   # Setup default NULL
#   top_annotation = NULL
#   left_annotation = NULL
#   row_split = NULL
#   col_split = NULL
#   
#   # Column groups
#   col_group = get_grouping_columns(.data)
#   
#   # Dataframe of column orientation
#   x_y_annot_cols = .data %>% get_x_y_annotation_columns(!!.column,!!.row,!!.abundance) 
#   
#   
#   x_y_annotation_cols = 
#     x_y_annot_cols %>%
#     nest(data = -orientation) %>%
#     mutate(data = map(data, ~ .x %>% pull(1))) %>%
#     {
#       df = (.)
#       pull(df, data) %>% set_names(pull(df, orientation))
#     } %>%
#     map(
#       ~ .x %>% intersect(col_group)
#     )
#   
#   # Check if you have more than one grouping, at the moment just one is accepted
#   if(x_y_annotation_cols %>% lapply(length) %>% unlist %>% max %>% gt(1))
#     stop("tidyHeatmap says: At the moment just one grouping per dimension (max 1 row and 1 column) is supported.")
#   
#   # Create dataset
#   col_group %>%
#     as_tibble %>%
#     rename(col_name = value) %>%
#     
#     # delete if annotation is NULL
#     when(length(col_group)==0 ~ slice(., 0), ~ (.)) %>%
#     
#     # Add orientation
#     left_join(x_y_annot_cols,  by = "col_name") %>%
#     mutate(col_orientation = map_chr(orientation, ~ .x %>% when((.) == "column" ~ quo_name(.column), (.) == "row" ~ quo_name(.row)))) %>%
#     
#     # Add data
#     mutate(
#       data = map2(
#         col_name,
#         col_orientation,
#         ~
#           .data_ %>%
#           ungroup() %>%
#           select(all_of(c(.y, .x))) %>%
#           distinct() %>%
#           arrange_at(vars(.y)) %>%
#           pull(.x)
#       )
#     )  %>%
#     
#     # Add function
#     mutate(fx = annotation_function) 
#   
#   if(length(x_y_annotation_cols$row) > 0){
#     
#     # Row split
#     row_split = 
#       .data %>%
#       ungroup() %>%
#       distinct(!!.row, !!as.symbol(x_y_annotation_cols$row)) %>%
#       arrange(!!.row) %>%
#       pull(!!as.symbol(x_y_annotation_cols$row))
#     
#     # Create array of colors
#     palette_fill_row = palette_annotation[[1]][1:length(unique(row_split))] %>% set_names(unique(row_split))
#     
#     left_annotation_args = 
#       list(
#         ct = anno_block(  
#           gp = gpar(fill = palette_fill_row ),
#           labels = row_split %>% unique %>% sort,
#           labels_gp = gpar(col = "white"),
#           which = "row"
#         )
#       )
#     
#     left_annotation = as.list(left_annotation_args)
#     
#     # Eliminate palette
#     palette_annotation = palette_annotation[-1]
#     
#   }
#   
#   if(length(x_y_annotation_cols$column) > 0){
#     # Col split
#     col_split = 
#       .data %>%
#       ungroup() %>%
#       distinct(!!.column, !!as.symbol(x_y_annotation_cols$column)) %>%
#       arrange(!!.column) %>%
#       pull(!!as.symbol(x_y_annotation_cols$column))
#     
#     # Create array of colors
#     palette_fill_column = palette_annotation[[1]][1:length(unique(col_split))] %>% set_names(unique(col_split))
#     
#     top_annotation_args = 
#       list(
#         ct = anno_block(  
#           gp = gpar(fill = palette_fill_column ),
#           labels = col_split %>% unique %>% sort,
#           labels_gp = gpar(col = "white"),
#           which = "column"
#         )
#       )
#     
#     top_annotation = as.list(top_annotation_args)
#   }
#   
#   
#   # Return
#   list( left_annotation = left_annotation, row_split = row_split, top_annotation = top_annotation, col_split = col_split )
# }

get_grouping_columns = function(.data){
  
  # Comply with CRAN NOTES
  .rows = NULL
  
  if("groups" %in%  (.data %>% attributes %>% names))
    .data %>% attr("groups") %>% select(-.rows) %>% colnames()
  else c()
}

list_drop_null = function(.data){
  .data[!sapply(.data, is.null)] 
}

#' Scale counts in a robust way against sd == 0 
#' 
#' @param y A numerical array
#' 
#' @return A scaled and centred numerical array
#' 
#' @export
#' @references Mangiola, S. and Papenfuss, A.T., 2020. "tidyHeatmap: an R package for 
#'   modular heatmap production based on tidy principles." Journal of Open Source Software.
#'   doi:10.21105/joss.02472.
#' @source [Mangiola and Papenfuss., 2020](https://joss.theoj.org/papers/10.21105/joss.02472)
scale_robust = function(y){
  
  do_consider_df = !is.na(sd(y, na.rm=T)) && as.logical(sd(y, na.rm=T) )
  
  (y - mean(y, na.rm=T)) / ( sd(y, na.rm=T) ^ do_consider_df )
} 

#' Convert array of quosure (e.g. c(col_a, col_b)) into character vector
#'
#' @importFrom rlang quo_name
#' @importFrom rlang quo_squash
#'
#' @param v A array of quosures (e.g. c(col_a, col_b))
#'
#' @return A character vector
quo_names <- function(v) {
  
  v = quo_name(quo_squash(v))
  gsub('^c\\(|`|\\)$', '', v) %>% 
    strsplit(', ') %>% 
    unlist 
}

#' annot_to_list
#' 
#' @importFrom purrr map_lgl
#' @importFrom rlang set_names
#' 
#' @param .data A data frame
#' 
#' @return A list
annot_to_list = function(.data){
  
  # Comply with CRAN NOTES
  col_name = NULL
  annot = NULL
  value = NULL
  my_cells = NULL
  name = NULL
  data = NULL
  
  
  .data %>% 
  	pull(annot) %>%
    set_names(.data %>% pull(col_name))  %>%
    
    # If list is populated
    when(length(.) > 0 ~ (.) %>% c(
      col = list(.data %>%
                   filter(map_lgl(color, ~ .x %>% is.null %>% not)) %>%
                   { set_names( pull(., color),  pull(., col_name))    })
    ) %>%
    	
    	# Add additional arguments
    	c(
    		.data %>% 
    			pull(further_arguments) %>% 
    			combine_elements_with_the_same_name()
    	),
    
    ~ (.)) 
    
}

list_append = function(.list1, .list2){ .list1 %>% c(.list2) }

reduce_to_tbl_if_in_class_chain = function(.obj){
  .obj %>%
    when(
      
      # Eliminate all classes until tbl
      "tbl" %in% class(.) ~ drop_class(., class(.)[1:which(class(.) == "tbl")-1]  ),
      ~ (.)
    )
  
}

# Greater than
gt = function(a, b){	a > b }

# Smaller than
st = function(a, b){	a < b }

# Negation
not = function(is){	!is }

# Raise to the power
pow = function(a,b){	a^b }

#' @importFrom purrr map_dfr
#' @importFrom purrr reduce
#' @importFrom tibble enframe
#' @importFrom grid unit.c
combine_elements_with_the_same_name = function(x){
	
	# Fix CRAN notes
	my_class  = NULL
	value = NULL
	name = NULL
	data = NULL
	
	if(length(unlist(x))==0) return(unlist(x))
	else {
		list_df = 
			map_dfr(x, ~ enframe(.x)) %>% 
			mutate(my_class = map_chr(value, ~class(.x)[[1]])) 
		
		# The current backend does not allow multiple tails sizes
		if(
			list_df %>% 
				filter(my_class == "simpleUnit") %>% 
				nrow()  %>% 
				gt(1) &&
			list_df %>% 
				filter(my_class == "simpleUnit") %>% 
				pull(value) %>% 
				reduce(identical) %>% 
				not()
		)
			warning("tidyHeatmap says: the current backend only allows for one tail annotation size. The latter one will be selected.")
			
		# Select one size
		list_df =  
			bind_rows(
			list_df %>% 
				filter(my_class == "simpleUnit") %>% 
				tail(1),
			list_df %>% 
				filter(my_class != "simpleUnit")
		) %>% 
			nest(data = -c(name, my_class)) %>% 
			mutate(vector = map2(
				data, my_class,
				~ {
					if(.y == "simpleUnit") reduce(.x$value, unit.c)
					else if(.y == "gpar") combine_lists_with_the_same_name(.x$value) %>% as.list() %>% do.call(gpar, .)
					else reduce(.x$value, c)
				}
			)) 
		
			
		list_df %>% 
			pull(vector) %>% 
			set_names(list_df$name)
			
		# x = unlist(x)
		# tapply(unlist(x, use.names = FALSE), rep(names(x), lengths(x)), FUN = c)
	}

}

combine_lists_with_the_same_name = function(x){
	
	if(length(unlist(x))==0) return(unlist(x))
	else {
		x = unlist(x)
		tapply(unlist(x, use.names = FALSE), rep(names(x), lengths(x)), FUN = c)
	}
	
}

# Helper function to filter arguments for a specific function
filter_args <- function(all_args, target_func, force_keep = NULL, invert = FALSE) {
  # Get the names of the formal arguments of the target function
  valid_args <- names(formals(target_func))
  
  # Check if force_keep is numeric
  if (is.numeric(force_keep)) {
    # Get the names of arguments at the specified positions
    positional_args <- names(all_args)[force_keep]
    # Combine with the valid arguments
    valid_args <- unique(c(valid_args, positional_args))
  } else if (!is.null(force_keep)) {
    # Combine with force_keep if it is not numeric
    valid_args <- unique(c(valid_args, force_keep))
  }
  
  # Filter or exclude arguments based on the invert parameter
  if (invert) {
    all_args[!names(all_args) %in% valid_args]
  } else {
    all_args[names(all_args) %in% valid_args]
  }
}
