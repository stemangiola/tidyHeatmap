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

-   [nanny](https://github.com/stemangiola/nanny/) for tidy high-level
    data analysis and manipulation
-   [tidygate](https://github.com/stemangiola/tidygate/) for adding
    custom gate information to your tibble
-   [tidySingleCellExperiment](https://stemangiola.github.io/tidySingleCellExperiment/)
    for tidy manipulation of Seurat objects
-   [tidyseurat](https://stemangiola.github.io/tidyseurat/) for tidy
    manipulation of Seurat objects
-   [tidybulk](https://stemangiola.github.io/tidybulk/) for tidy
    high-level data analysis and manipulation
-   [tidySummarizedExperiment](https://stemangiola.github.io/tidySummarizedExperiment/)
    for heatmaps produced with tidy principles

website:
[stemangiola.github.io/tidyHeatmap](https://stemangiola.github.io/tidyHeatmap/)

`tidyHeatmap` is a package that introduces tidy principles to the
creation of information-rich heatmaps. This package uses
[ComplexHeatmap](https://bioconductor.org/packages/release/bioc/html/ComplexHeatmap.html)
as graphical engine.

**Advantages:**

-   Modular annotation with just specifying column names
-   Custom grouping of rows is easy to specify providing a grouped tbl.
    For example `df %>% group_by(...)`
-   Labels size adjusted by row and column total number
-   Default use of Brewer and Viridis palettes

## Functions/utilities available

| Function       | Description                                |
|----------------|--------------------------------------------|
| `heatmap`      | Plot base heatmap                          |
| `add_tile`     | Add tile annotation to the heatmap         |
| `add_point`    | Add point annotation to the heatmap        |
| `add_bar`      | Add bar annotation to the heatmap          |
| `add_line`     | Add line annotation to the heatmap         |
| `layer_symbol` | Add layer of symbols on top of the heatmap |
| `save_pdf`     | Save the PDF of the heatmap                |

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

| element         | feature         | value     | independent\_variables |
|-----------------|-----------------|-----------|------------------------|
| `chr` or `fctr` | `chr` or `fctr` | `numeric` | …                      |

Let’s transform the mtcars dataset into a tidy
“element-feature-independent variables” data frame. Where the
independent variables in this case are ‘hp’ and ‘vs’.

``` r
mtcars_tidy <- 
    mtcars %>% 
    as_tibble(rownames="Car name") %>% 
    
    # Scale
    mutate_at(vars(-`Car name`, -hp, -vs), scale) %>%
    
    # tidyfy
    pivot_longer(cols = -c(`Car name`, hp, vs), names_to = "Property", values_to = "Value")

mtcars_tidy
```

    ## # A tibble: 288 x 5
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
    ## # … with 278 more rows

## Plot

For plotting, you simply pipe the input data frame into heatmap,
specifying:

-   The rows, cols relative column names (mandatory)
-   The value column name (mandatory)
-   The annotations column name(s)

mtcars

``` r
mtcars_heatmap <- 
    mtcars_tidy %>% 
        heatmap(`Car name`, Property, Value ) %>%
        add_tile(hp)

mtcars_heatmap
```

![](man/figures/unnamed-chunk-6-1.png)<!-- -->

## Save

``` r
mtcars_heatmap %>% save_pdf("mtcars_heatmap.pdf")
```

## Grouping

We can easily group the data (one group per dimension maximum, at the
moment only the vertical dimension is supported) with dplyr, and the
heatmap will be grouped accordingly

``` r
mtcars_tidy %>% 
    group_by(vs) %>%
    heatmap(`Car name`, Property, Value ) %>%
    add_tile(hp)
```

![](man/figures/unnamed-chunk-8-1.png)<!-- -->

## Custom palettes

We can easily use custom palette, using strings, hexadecimal color
character vector,

``` r
mtcars_tidy %>% 
    heatmap(
        `Car name`, 
        Property, 
        Value,
        palette_value = c("red", "white", "blue")
    )
```

![](man/figures/unnamed-chunk-9-1.png)<!-- -->

Or a grid::colorRamp2 function for higher flexibility

``` r
mtcars_tidy %>% 
    heatmap(
        `Car name`, 
        Property, 
        Value,
        palette_value = circlize::colorRamp2(c(-2, -1, 0, 1, 2), viridis::magma(5))
    )
```

![](man/figures/unnamed-chunk-10-1.png)<!-- -->

## Multiple groupings and annotations

``` r
tidyHeatmap::pasilla %>%
    group_by(location, type) %>%
    heatmap(
            .column = sample,
            .row = symbol,
            .value = `count normalised adjusted`
        ) %>%
    add_tile(condition) %>%
    add_tile(activation)
```

![](man/figures/unnamed-chunk-11-1.png)<!-- -->

## Annotation types

**This feature requires &gt;= 0.99.20 version**

“tile” (default), “point”, “bar” and “line” are available

``` r
# Create some more data points
pasilla_plus <- 
    tidyHeatmap::pasilla %>%
        dplyr::mutate(act = activation) %>% 
        tidyr::nest(data = -sample) %>%
        dplyr::mutate(size = rnorm(n(), 4,0.5)) %>%
        dplyr::mutate(age = runif(n(), 50, 200)) %>%
        tidyr::unnest(data) 

# Plot
pasilla_plus %>%
        heatmap(
            .column = sample,
            .row = symbol,
            .value = `count normalised adjusted`
        ) %>%
    add_tile(condition) %>%
    add_point(activation) %>%
    add_tile(act) %>%
    add_bar(size) %>%
    add_line(age)
```

![](man/figures/unnamed-chunk-12-1.png)<!-- -->

# Layer symbol

Add a layer on top of the heatmap

``` r
tidyHeatmap::pasilla %>%
    
    # filter
    filter(symbol %in% head(unique(tidyHeatmap::pasilla$symbol), n = 10)) %>%
    
    heatmap(
            .column = sample,
            .row = symbol,
            .value = `count normalised adjusted`
        ) %>% 
    layer_symbol(
        `count normalised adjusted log` > 6 & sample == "untreated3" 
    )
```

![](man/figures/unnamed-chunk-13-1.png)<!-- -->
