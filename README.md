tidyHeatmap
================

<!-- badges: start -->

[![Lifecycle:maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![DOI](https://joss.theoj.org/papers/10.21105/joss.02472/status.svg)](https://doi.org/10.21105/joss.02472)
<!-- badges: end -->

## Citation

Mangiola et al., (2020). tidyHeatmap: an R package for modular heatmap
production based on tidy principles. Journal of Open Source Software,
5(52), 2472, <https://doi.org/10.21105/joss.02472>

Please have a look also to

- [tidygate](https://github.com/stemangiola/tidygate/) for adding custom
  gate information to your tibble
- [tidySingleCellExperiment](https://stemangiola.github.io/tidySingleCellExperiment/)
  for tidy manipulation of Seurat objects
- [tidyseurat](https://stemangiola.github.io/tidyseurat/) for tidy
  manipulation of Seurat objects
- [tidybulk](https://stemangiola.github.io/tidybulk/) for tidy
  high-level data analysis and manipulation
- [tidySummarizedExperiment](https://stemangiola.github.io/tidySummarizedExperiment/)
  for heatmaps produced with tidy principles

website:
[stemangiola.github.io/tidyHeatmap](https://stemangiola.github.io/tidyHeatmap/)

`tidyHeatmap` is a package that introduces tidy principles to the
creation of information-rich heatmaps. This package uses
[ComplexHeatmap](https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html)
as graphical engine.

**Advantages:**

- Modular annotation with just specifying column names
- Custom grouping of rows is easy to specify providing a grouped tbl.
  For example `df |> group_by(...)`
- Labels size adjusted by row and column total number
- Default use of Brewer and Viridis palettes

## Functions/utilities available

| Function            | Description                                                                 |
|---------------------|-----------------------------------------------------------------------------|
| `heatmap`           | Plots base heatmap                                                          |
| `group_by`          | `dplyr` function - groups heatpmap rows/columns                             |
| `annotation_tile`   | Adds tile annotation to the heatmap                                         |
| `annotation_point`  | Adds point annotation to the heatmap                                        |
| `annotation_bar`    | Adds bar annotation to the heatmap                                          |
| `annotation_line`   | Adds line annotation to the heatmap                                         |
| `layer_text`        | Add layer of text on top of the heatmap                                     |
| `layer_point`       | Adds layer of symbols on top of the heatmap                                 |
| `layer_square`      | Adds layer of symbols on top of the heatmap                                 |
| `layer_diamond`     | Adds layer of symbols on top of the heatmap                                 |
| `layer_arrow_up`    | Adds layer of symbols on top of the heatmap                                 |
| `layer_arrow_down`  | Add layer of symbols on top of the heatmap                                  |
| `layer_star`        | Add layer of symbols on top of the heatmap                                  |
| `layer_asterisk`    | Add layer of symbols on top of the heatmap                                  |
| `split_rows`        | Splits the rows based on the dendogram                                      |
| `split_columns`     | Splits the columns based on the dendogram                                   |
| `save_pdf`          | Saves the PDF of the heatmap                                                |
| `+`                 | Integrate heatmaps side-by-side                                             |
| `as_ComplexHeatmap` | Convert the tidyHeatmap output to ComplexHeatmap for non-standard “drawing” |
| `wrap_heatmap`      | Allows the integration with the `patchwork` package                         |

## Installation

To install the most up-to-date version

``` r
devtools::install_github("stemangiola/tidyHeatmap")
```

To install the most stable version (however please keep in mind that
this package is under a maturing lifecycle stage)

``` r
install.packages("tidyHeatmap")
```

## Contribution

If you want to contribute to the software, report issues or problems
with the software or seek support please open an issue
[here](https://github.com/stemangiola/tidyHeatmap/issues)

## Input data frame

The heatmaps visualise a multi-element, multi-feature dataset, annotated
with independent variables. Each observation is a element-feature pair
(e.g., person-physical characteristics).

| element         | feature         | value     | independent_variables |
|-----------------|-----------------|-----------|-----------------------|
| `chr` or `fctr` | `chr` or `fctr` | `numeric` | …                     |

Let’s transform the mtcars dataset into a tidy
“element-feature-independent variables” data frame. Where the
independent variables in this case are ‘hp’ and ‘vs’.

``` r
mtcars_tidy <- 
    mtcars |> 
    as_tibble(rownames="Car name") |> 
    
    # Scale
    mutate_at(vars(-`Car name`, -hp, -vs), scale) |>
    
    # tidyfy
    pivot_longer(cols = -c(`Car name`, hp, vs), names_to = "Property", values_to = "Value")

mtcars_tidy
```

    ## # A tibble: 288 × 5
    ##    `Car name`       hp    vs Property Value[,1]
    ##    <chr>         <dbl> <dbl> <chr>        <dbl>
    ##  1 Mazda RX4       110     0 mpg          0.151
    ##  2 Mazda RX4       110     0 cyl         -0.105
    ##  3 Mazda RX4       110     0 disp        -0.571
    ##  4 Mazda RX4       110     0 drat         0.568
    ##  5 Mazda RX4       110     0 wt          -0.610
    ##  6 Mazda RX4       110     0 qsec        -0.777
    ##  7 Mazda RX4       110     0 am           1.19 
    ##  8 Mazda RX4       110     0 gear         0.424
    ##  9 Mazda RX4       110     0 carb         0.735
    ## 10 Mazda RX4 Wag   110     0 mpg          0.151
    ## # ℹ 278 more rows

## Plotting

For plotting, you simply pipe the input data frame into heatmap,
specifying:

- The rows, cols relative column names (mandatory)
- The value column name (mandatory)
- The annotations column name(s)

mtcars

``` r
mtcars_heatmap <- 
    mtcars_tidy |> 
    heatmap(`Car name`, Property, Value,    scale = "row"   ) |>
    annotation_tile(hp)
```

    ## Warning: Using one column matrices in `filter()` was deprecated in dplyr 1.1.0.
    ## ℹ Please use one dimensional logical vectors instead.
    ## ℹ The deprecated feature was likely used in the tidyHeatmap package.
    ##   Please report the issue at <https://github.com/stemangiola/tidyHeatmap>.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

``` r
mtcars_heatmap
```

![](man/fragments/figures/heatmap-1.png)<!-- -->

## Saving

``` r
mtcars_heatmap |> save_pdf("mtcars_heatmap.pdf")
```

## Clustering

Choose alternative clustering distance and methods.

``` r
tidyHeatmap::pasilla |>
    
    heatmap(
        .column = sample,
        .row = symbol,
        .value = `count normalised adjusted`,   
        scale = "row",
        
        # Arguments passed to ComplexHeatmap 
        clustering_distance_rows = "manhattan",
        clustering_distance_columns = "manhattan",
        clustering_method_rows = "ward.D",
        clustering_method_columns = "ward.D"
    ) 
```

![](man/fragments/figures/distance-1.png)<!-- -->

## Grouping and splitting

We can easily group the data (one group per dimension maximum, at the
moment only the vertical dimension is supported) with dplyr, and the
heatmap will be grouped accordingly

``` r
# Make up more groupings
mtcars_tidy_groupings = 
    mtcars_tidy |>
    mutate(property_group = if_else(Property %in% c("cyl", "disp"), "Engine", "Other"))

mtcars_tidy_groupings |> 
    group_by(vs, property_group) |>
    heatmap(`Car name`, Property, Value,    scale = "row"   ) |>
    annotation_tile(hp)
```

![](man/fragments/figures/grouping-1.png)<!-- -->

We can provide colour palettes to groupings

``` r
mtcars_tidy_groupings |> 
    group_by(vs, property_group) |>
    heatmap(
        `Car name`, Property, Value ,   
        scale = "row",
        palette_grouping = list(
            
            # For first grouping (vs)
            c("#66C2A5", "#FC8D62"), 
            
            # For second grouping (property_group)
            c("#b58b4c", "#74a6aa")
        )
    ) |>
    annotation_tile(hp)
```

![](man/fragments/figures/grouping2-1.png)<!-- -->

We can split based on the cladogram

``` r
mtcars_tidy |> 
    heatmap(`Car name`, Property, Value,    scale = "row"   ) |>
    split_rows(2) |>
    split_columns(2)
```

![](man/fragments/figures/split-1.png)<!-- -->

We can split on kmean clustering (using ComplexHeatmap options, it is
stochastic)

``` r
mtcars_tidy |> 
    heatmap(
        `Car name`, Property, Value,    
        scale = "row",
        row_km = 2,
        column_km = 2
    ) 
```

![](man/fragments/figures/split2-1.png)<!-- -->

## Custom palettes

We can easily use custom palette, using strings, hexadecimal color
character vector,

``` r
mtcars_tidy |> 
    heatmap(
        `Car name`, 
        Property, 
        Value,  
        scale = "row",
        palette_value = c("red", "white", "blue")
    )
```

![](man/fragments/figures/custom-1.png)<!-- -->

A better-looking blue-to-red palette

``` r
mtcars_tidy |> 
    heatmap(
        `Car name`, 
        Property, 
        Value,  
        scale = "row",
        palette_value = circlize::colorRamp2(
            seq(-2, 2, length.out = 11), 
            RColorBrewer::brewer.pal(11, "RdBu")
        )
    )
```

![](man/fragments/figures/redblue-1.png)<!-- -->

Or a grid::colorRamp2 function for higher flexibility

``` r
mtcars_tidy |> 
    heatmap(
        `Car name`, 
        Property, 
        Value,  
        scale = "row",
        palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
    )
```

![](man/fragments/figures/flexible-1.png)<!-- -->

We can use custom colors for tile annotation

``` r
mtcars_tidy |> 
    heatmap(
        `Car name`, 
        Property, 
        Value,  
        scale = "row"
    ) |>
    add_tile(
        hp, 
        palette = c("red", "white", "blue")
    )
```

    ## Warning: `add_tile()` was deprecated in tidyHeatmap 1.9.0.
    ## ℹ Please use `annotation_tile()` instead
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](man/fragments/figures/customtile-1.png)<!-- -->

We can use grid::colorRamp2 function for tile annotation for specific
color scales

``` r
mtcars_tidy |> 
    heatmap(
        `Car name`, 
        Property, 
        Value,  
        scale = "row"
    ) |>
    annotation_tile(
        hp, 
        palette = circlize::colorRamp2(c(0, 100, 200, 300), viridis::magma(4))
    )
```

![](man/fragments/figures/customtile2-1.png)<!-- -->

## Multiple groupings and annotations

``` r
tidyHeatmap::pasilla |>
    group_by(location, type) |>
    heatmap(
        .column = sample,
        .row = symbol,
        .value = `count normalised adjusted`,   
        scale = "row"
    ) |>
    annotation_tile(condition) |>
    annotation_tile(activation)
```

![](man/fragments/figures/multiple-1.png)<!-- -->

Remove legends, adding aesthetics to annotations in a modular fashion,
using `ComplexHeatmap` arguments

``` r
tidyHeatmap::pasilla |>
    group_by(location, type) |>
    heatmap(
        .column = sample,
        .row = symbol,
        .value = `count normalised adjusted`,   
        scale = "row",
        show_heatmap_legend = FALSE
    ) |>
    annotation_tile(condition, show_legend = FALSE) |>
    annotation_tile(activation, show_legend = FALSE)
```

![](man/fragments/figures/nolegend-1.png)<!-- -->

## Annotation types

“tile”, “point”, “bar” and “line” are available

``` r
# Create some more data points
pasilla_plus <- 
    tidyHeatmap::pasilla |>
    dplyr::mutate(act = activation) |> 
    tidyr::nest(data = -sample) |>
    dplyr::mutate(size = rnorm(n(), 4,0.5)) |>
    dplyr::mutate(age = runif(n(), 50, 200)) |>
    tidyr::unnest(data) 

# Plot
pasilla_plus |>
    heatmap(
        .column = sample,
        .row = symbol,
        .value = `count normalised adjusted`,   
        scale = "row"
    ) |>
    annotation_tile(condition) |>
    annotation_point(activation) |>
    annotation_tile(act) |>
    annotation_bar(size) |>
    annotation_line(age)
```

![](man/fragments/figures/manyannotations-1.png)<!-- -->

## Annotation size

We can customise annotation sizes using the `grid::unit()`, and the size
of their names using in-built `ComplexHeatmap` arguments

``` r
pasilla_plus |>
    heatmap(
        .column = sample,
        .row = symbol,
        .value = `count normalised adjusted`,   
        scale = "row"
    ) |>
    annotation_tile(condition, size = unit(0.3, "cm"),  annotation_name_gp= gpar(fontsize = 8)) |>
    annotation_point(activation, size = unit(0.3, "cm"),    annotation_name_gp= gpar(fontsize = 8)) |>
    annotation_tile(act, size = unit(0.3, "cm"),    annotation_name_gp= gpar(fontsize = 8)) |>
    annotation_bar(size, size = unit(0.3, "cm"),    annotation_name_gp= gpar(fontsize = 8)) |>
    annotation_line(age, size = unit(0.3, "cm"),    annotation_name_gp= gpar(fontsize = 8))
```

![](man/fragments/figures/size-1.png)<!-- -->

# Layer symbol

Add a layer on top of the heatmap

``` r
tidyHeatmap::pasilla |>
    
    # filter
    filter(symbol %in% head(unique(tidyHeatmap::pasilla$symbol), n = 10)) |>
    
    # Add dynamic size
    mutate(my_size = runif(n(), 1,5)) |> 
    
    heatmap(
        .column = sample,
        .row = symbol,
        .value = `count normalised adjusted`,   
        scale = "row"
    ) |> 
    layer_point(
        `count normalised adjusted log` > 6 & sample == "untreated3"
    ) |>
    layer_square(
        `count normalised adjusted log` > 6 & sample == "untreated2",
        .size = my_size
    ) |>
    layer_arrow_up(
        `count normalised adjusted log` > 6 & sample == "untreated1",
        .size = 4
    )
```

![](man/fragments/figures/layer-1.png)<!-- -->

# Layer text

Add a text layer on top of the heatmap

``` r
tidyHeatmap::pasilla |>
    
    # filter
    filter(symbol %in% head(unique(tidyHeatmap::pasilla$symbol), n = 10)) |>
    
    # Add dynamic text
    mutate(my_text = "mt", my_size = 7) |> 
    
    # Plot
    heatmap(
        .column = sample,
        .row = symbol,
        .value = `count normalised adjusted`,   
        scale = "row"
    ) |> 
    layer_text(
        `count normalised adjusted log` > 6 & sample == "untreated3", 
        .value = "a", 
        .size = 15
    ) |> 
    layer_text(
    `count normalised adjusted log` > 6 & sample == "untreated2", 
    .value = my_text,
    .size = my_size
)
```

![](man/fragments/figures/layertext-1.png)<!-- -->

# Adding heatmap side-by-side

``` r
p_heatmap = heatmap(mtcars_tidy, `Car name`, Property, Value, scale = "row") 

p_heatmap + p_heatmap
```

![](man/fragments/figures/sidebyside-1.png)<!-- -->

# ComplexHeatmap further styling

## Add cell borders

``` r
mtcars_tidy |> 
    heatmap(
        `Car name`, Property, Value,    
        scale = "row", 
        rect_gp = grid::gpar(col = "#161616", lwd = 0.5)
    ) 
```

![](man/fragments/figures/borders-1.png)<!-- -->

## Drop row clustering

``` r
mtcars_tidy |> 
    heatmap(
        `Car name`, Property, Value,    
        scale = "row", 
        cluster_rows = FALSE
    ) 
```

![](man/fragments/figures/droprow-1.png)<!-- -->

## Reorder rows elements

``` r
library(forcats)
mtcars_tidy |> 
    mutate(`Car name` = fct_reorder(`Car name`, `Car name`, .desc = TRUE)) %>% 
    heatmap(
        `Car name`, Property, Value,    
        scale = "row", 
        cluster_rows = FALSE
    ) 
```

![](man/fragments/figures/reorder-1.png)<!-- -->

## Size of dendrograms

``` r
mtcars_tidy |> 
    mutate(`Car name` = fct_reorder(`Car name`, `Car name`, .desc = TRUE)) %>% 
    heatmap(
        `Car name`, Property, Value,    
        scale = "row", 
        column_dend_height = unit(0.2, "cm"), 
        row_dend_width = unit(0.2, "cm")
    ) 
```

![](man/fragments/figures/sizedendro-1.png)<!-- -->

## Size of rows/columns titles and names

``` r
mtcars_tidy |> 
    mutate(`Car name` = fct_reorder(`Car name`, `Car name`, .desc = TRUE)) %>% 
    heatmap(
        `Car name`, Property, Value,    
        scale = "row", 
        row_names_gp = gpar(fontsize = 7),
        column_names_gp = gpar(fontsize = 7),
        column_title_gp = gpar(fontsize = 7),
        row_title_gp = gpar(fontsize = 7)
    ) 
```

![](man/fragments/figures/sizecolumns-1.png)<!-- -->

## External `ComplexHeatmap` functionalities

ComplexHeatmap has some graphical functionalities that are not included
in the standard functional framework. We can use `as_ComplexHeatmap` to
convert our output before applying drawing options.

### Chainging side of legends

``` r
heatmap(mtcars_tidy, `Car name`, Property, Value, scale = "row" ) %>%
    as_ComplexHeatmap() %>%
    ComplexHeatmap::draw(heatmap_legend_side = "left"   )
```

![](man/fragments/figures/sidelegend-1.png)<!-- -->

### Add title using `draw` from `ComplexHeatmap`

``` r
mtcars_tidy |> 
    heatmap(`Car name`, Property, Value,    scale = "row"   ) |>
    as_ComplexHeatmap() |>
    ComplexHeatmap::draw(
        column_title = "TITLE", 
        column_title_gp = gpar(fontsize = 16)
    )
```

![](man/fragments/figures/title-1.png)<!-- -->

## Using patchwork to integrate heatmaps

``` r
library(ggplot2)
library(patchwork)

p_heatmap =
    mtcars_tidy |> 
    heatmap(
        `Car name`, Property, Value,    
        scale = "row", 
            show_heatmap_legend = FALSE,
        row_names_gp = gpar(fontsize = 7)
    ) 

p_ggplot = data.frame(value = 1:10) |> ggplot(aes(value)) + geom_density()

wrap_heatmap(p_heatmap) + 
    p_ggplot +
    
    # Add padding for better aesthetics
    wrap_heatmap(
        p_heatmap,
        padding = grid::unit(c(-30, -0, -0, -10), "points" ),
        clip = FALSE
    ) + 
    plot_layout(width = c(1, 0.3, 1))
```

![](man/fragments/figures/patchworkintegrate-1.png)<!-- -->

### Add title using `ggtitle` from `ggplot2`

``` r
mtcars_tidy |> 
    heatmap(`Car name`, Property, Value,  scale = "row" ) |>
    wrap_heatmap() +
        ggplot2::ggtitle("TITLE")
```

![](man/fragments/figures/title2-1.png)<!-- -->
