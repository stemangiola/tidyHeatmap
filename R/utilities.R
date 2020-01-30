

#' This is a generalisation of ifelse that acceots an object and return an objects
#'
#' @import dplyr
#' @import tidyr
#' @importFrom purrr as_mapper
#'
#' @param .x A tibble
#' @param .p A boolean
#' @param .f1 A function
#' @param .f2 A function
#'
#' @return A tibble
ifelse_pipe = function(.x, .p, .f1, .f2 = NULL) {
  switch(.p %>% `!` %>% sum(1),
         as_mapper(.f1)(.x),
         if (.f2 %>% is.null %>% `!`)
           as_mapper(.f2)(.x)
         else
           .x)
  
}

#' This is a generalisation of ifelse that acceots an object and return an objects
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
    .p1 %>% `!` %>% sum(1),
    
    # First outcome
    as_mapper(.f1)(.x),
    switch(
      # Second condition
      .p2 %>% `!` %>% sum(1),
      
      # Second outcome
      as_mapper(.f2)(.x),
      
      # Third outcome - if there is not .f3 just return the original data frame
      if (.f3 %>% is.null %>% `!`)
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
        `%in%`(c("numeric", "integer")) %>% `!`() %>% any(),
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
  
  if (x %>% nrow %>% `>` (0))
    if (x %>% summarise(m = !!.abundance %>% max) %>% pull(m) < 50)
      stop(
        "The input was log transformed, this algorithm requires raw (un-normalised) read counts"
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
    stop("The .formula must be of the kind \"~ covariates\" ")
  else
    as.character(attr(terms(fm), "variables"))[-1]
}

#' Scale design matrix
#'
#' @importFrom stats setNames
#' @importFrom stats cov
#'
#' @param df A tibble
#' @param .formula a formula
#'
#' @return A tibble
#'
#'
scale_design = function(df, .formula) {
  
  # Comply with CRAN NOTES
  value = sample_idx = `(Intercept)` =  NULL
  
  df %>%
    setNames(c("sample_idx", "(Intercept)", parse_formula(.formula))) %>%
    gather(cov, value,-sample_idx) %>%
    group_by(cov) %>%
    mutate(value = ifelse(
      !grepl("Intercept", cov) &
        length(union(c(0, 1), value)) != 2,
      scale(value),
      value
    )) %>%
    ungroup() %>%
    spread(cov, value) %>%
    arrange(as.integer(sample_idx)) %>%
    select(`(Intercept)`, one_of(parse_formula(.formula)))
}

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
        The fucntion does not know what your sample, transcript and counts columns are.\n
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
        The fucntion does not know what your sample, transcript and counts columns are.\n
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
        The fucntion does not know what your sample, transcript and counts columns are.\n
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
    if(.data %>% attr("parameters") %>% is.null %>% `!`)
      
      return(list(
        .element =  switch(
          of_samples %>% `!` %>% sum(1),
          attr(.data, "parameters")$.sample,
          attr(.data, "parameters")$.transcript
        ),
        .feature = switch(
          of_samples %>% `!` %>% sum(1),
          attr(.data, "parameters")$.transcript,
          attr(.data, "parameters")$.sample
        )
      ))
    # Else through error
    else
      stop("
        The fucntion does not know what your elements (e.g., sample) and features (e.g., transcripts) are.\n
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
        The fucntion does not know what your elements (e.g., sample) and features (e.g., transcripts) are.\n
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
    if(.data %>% attr("parameters") %>% is.null %>% `!`)
      
      return(list(
        .element =  switch(
          of_samples %>% `!` %>% sum(1),
          attr(.data, "parameters")$.sample,
          attr(.data, "parameters")$.transcript
        )
      ))
    # Else through error
    else
      stop("
        The fucntion does not know what your elements (e.g., sample) are.\n
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
    if(.data %>% attr("parameters") %>% is.null %>% `!`)
      
      return(list(
        .abundance =  switch(
          (".abundance_norm" %in% (.data %>% attr("parameters") %>% names) &
             quo_name(.data %>% attr("parameters") %$% .abundance_norm) %in% (.data %>% colnames)
          ) %>% `!` %>% sum(1),
          attr(.data, "parameters")$.abundance_norm,
          attr(.data, "parameters")$.abundance
        )
      ))
    # Else through error
    else
      stop("
        The fucntion does not know what your elements (e.g., sample) are.\n
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

get_x_y_annotation_columns = function(.data, .horizontal, .vertical, .abundance){
  
  
  # Comply with CRAN NOTES
  . = NULL
  
  # Make col names
  .horizontal = enquo(.horizontal)
  .vertical = enquo(.vertical)
  .abundance = enquo(.abundance)
  
  # x-annotation df
  n_x = .data %>% distinct(!!.horizontal) %>% nrow
  n_y = .data %>% distinct(!!.vertical) %>% nrow
  
  list(
    
    # Horizontal
    horizontal= 
      .data %>%
      select(-!!.horizontal, -!!.vertical, -!!.abundance) %>%
      colnames %>%
      map(
        ~ 
          .x %>%
          ifelse_pipe(
            .data %>%
              distinct(!!.horizontal, !!as.symbol(.x)) %>%
              nrow %>%
              equals(n_x),
            ~ .x,
            ~ NULL
          )
      ) %>%
      
      # Drop NULL
      {	(.)[lengths((.)) != 0]	} %>%
      unlist,
    
    vertical= 
      .data %>%
      select(-!!.horizontal, -!!.vertical, -!!.abundance) %>%
      colnames %>%
      map(
        ~ 
          .x %>%
          ifelse_pipe(
            .data %>%
              distinct(!!.vertical, !!as.symbol(.x)) %>%
              nrow %>%
              equals(n_y),
            ~ .x,
            ~ NULL
          )
      ) %>%
      
      # Drop NULL
      {	(.)[lengths((.)) != 0]	} %>%
      unlist
    
  )
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


get_top_annotation = function(.data, .horizontal, .vertical, .abundance, annotation, x_y_annot_cols, palette_annotation){
  
  # Make col names
  .horizontal = enquo(.horizontal)
  .vertical = enquo(.vertical)
  .abundance = enquo(.abundance)
  annotation = enquo(annotation)
  
  if(annotation %>% quo_is_symbolic()) {
    annot_col_names = .data %>% ungroup() %>% select(!!annotation) %>% colnames
    
    x_y_annotation_cols = 
      x_y_annot_cols %>%
      map(
        ~ .x %>% intersect( annot_col_names)
      )
    
    # Col annot
    col_annot = 
      x_y_annotation_cols$horizontal %>%
      map(
        ~ {
          .data %>%
            ungroup() %>%
            distinct(!!.horizontal, !!as.symbol(.x)) %>%
            arrange(!!.horizontal) %>%
            pull(!!as.symbol(.x))
        }
      )
    
    # Stop if annotations discrete bigger than palette
    if(
      col_annot %>% map_chr(~ .x %>% class) %in% 
      c("factor", "character") %>% which %>% length %>%
      `>` (palette_annotation$discrete %>% length)
    ) stop("Your discrete annotaton columns are bigger than the palette available")
    
    # Stop if annotations continuous bigger than palette
    if(
      col_annot %>% map_chr(~ .x %>% class) %in% 
      c("int", "dbl") %>% which %>% length %>%
      `>` (palette_annotation$continuous %>% length)
    ) stop("Your continuous annotaton columns are bigger than the palette available")
    
    col_annot_cont = 
      col_annot %>%
      map2(
        1:length(col_annot) %>% as.list,
        ~ {
          if(.x %>% class %in% c("factor", "character"))
            palette_annotation$discrete[[.y]][1:length(unique(.x))] %>% setNames(unique(.x))
          else
            palette_annotation$continuous[[.y]](length(.x)) %>% colorRamp2(seq(min(.x), max(.x), length.out = length(.x)), .)
          
        }
      )
    
    
    left_annotation_args = 
      col_annot %>% 
      setNames(annot_col_names) %>%
      c(
        col = list(col_annot_cont %>% setNames(annot_col_names))
      )
    
    top_annotation = do.call("HeatmapAnnotation", as.list(left_annotation_args))
    
  } else {
    top_annotation = NULL
  }
  
  # Return
  top_annotation
}


get_group_annotation = function(.data, .horizontal, .vertical, .abundance, annotation, x_y_annot_cols, palette_annotation){
  
  # Make col names
  .horizontal = enquo(.horizontal)
  .vertical = enquo(.vertical)
  .abundance = enquo(.abundance)
  annotation = enquo(annotation)
  
  # Column groups
  col_group = get_grouping_columns(.data)
  
  if("groups" %in%  (.data %>% attributes %>% names)) {
    x_y_annotation_cols = 
      x_y_annot_cols %>%
      map(
        ~ .x %>% intersect(col_group)
      )
    
    # Row split
    row_split = 
      .data %>%
      ungroup() %>%
      distinct(!!.vertical, !!as.symbol(x_y_annotation_cols$vertical)) %>%
      arrange(!!.vertical) %>%
      pull(!!as.symbol(col_group))
    
    # Create array of colors
    palette_fill = palette_annotation$discrete[[1]][1:length(unique(row_split))] %>% setNames(unique(row_split))
    
    left_annotation_args = 
      list(
        ct = anno_block(  
          gp = gpar(fill = palette_fill ),
          labels = row_split %>% unique %>% sort,
          labels_gp = gpar(col = "white"),
          which = "row"
        )
      )
    
    left_annotation = do.call("rowAnnotation", as.list(left_annotation_args))
    
  } else {
    row_split = NULL
    left_annotation =	NULL
  }
  
  # Return
  list(
    left_annotation = left_annotation,
    row_split = row_split
  )
}

get_grouping_columns = function(.data){
  
  # Comply with CRAN NOTES
  .rows = NULL
  
  if("groups" %in%  (.data %>% attributes %>% names))
    .data %>% attr("groups") %>% select(-.rows) %>% colnames()
  else c()
}


