---
title: 'tidyHeatmap: a modular R package for heatmap visualisation based on tidy principles'
tags:
- R
- tidyverse
- tidy
- heatmap
- data visualization
date: "07 July 2020"
output:
  pdf_document: default
  word_document: default
authors:
- name: Stefano Mangiola^[Corresponding author]
  orcid: 0000-0001-7474-836X
  affiliation: 1, 2
- name: Anthony T. Papenfuss^[Corresponding author]
  orcid: 0000-0002-1102-8506
  affiliation: 1, 2, 3, 4
bibliography: paper.bib
affiliations:
- name: Bioinformatics Division, The Walter and Eliza Hall Institute of Medical Research,
    Parkville, Victoria, Australia
  index: 1
- name: Department of Medical Biology, University of Melbourne, Melbourne, Victoria,
    Australia.
  index: 2
- name: Peter MacCallum Cancer Centre, Melbourne, VIC 3000, Australia.
  index: 3
- name: School of Mathematics and Statistics, University of Melbourne, Melbourne,
    VIC 3010, Australia.
  index: 4
---

# Background
The heatmap is a powerful tool for visualising multi-dimensional data, where individual values can be organised in a two-dimensional matrix and their values expressed as colours. Row and columns can be ordered according to their reciprocal similarity using hierarchical clustering; and dendrograms can be added to the plot to facilitate the interpretation. Row- and column-wise visual annotations, such as coloured tiles, can also be included. Within the R environment, several packages have been produced to produce heatmaps. The simplest and most readily available tool, `heatmap`, is provided within the `stats` package [@RCoreTeam:2013] and offers basic heatmaps with no annotations. The versatile package `ggplot2` can be used to produce basic heatmaps, but with fine aesthetical adjustable parameters [@Hadley:2016]. More powerful software exists for producing fully annotated, multi-panels heatmaps, such as `Pheatmap` [@Kolde2012-tu] and `ComplexHeatmap` [@Gu2016-cd]. The versatility of these packages comes at the cost of adding complexity in the user interface, characterised by many parameters and annotation functions that introduce a steep learning curve to produce complex, clear, and good-looking graphics.

Recently, efforts have been made toward the harmonisation of data frame structures and data analysis workflows using the concept of tidiness. Tidy data frames allow ease of manipulation, modelling, and visualisation; and are characterised by having a specific structure where each variable is a column and each observation is a row. The `tidyverse` is a suite of R libraries that defined the standard for tidy data and APIs [@Hadley:2019]. The unique correspondence between quantities and annotations, characteristic of tidy data frames, allows complex operations to be performed from simple user inputs, such as a list of column names. 
 
# Tidy paradigm for visualisation
`tidyHeatmap` is a graphical R package that introduces tidy principles to the creation of information-rich heatmaps. It is available in the CRAN R repository. This package uses `ComplexHeatmap` as its graphical engine. The command-line user interface is organised in (i) the main plotting utility; (ii) the annotation layer utilities; and (iii) the file output utilities. The input data frame streams along the utility path using the pipe operator from `magrittr`, allowing a high-level of modularity. The main utility allows the user to plot a base heatmap with dendrograms. The annotation utilities allow the user to serially add tile, point, bar and/or line annotation boxes to the side on the heatmap. The orientation of the annotations (row- or column-wise) is inferred by the `tidyHeatmap` algorithms, based on the input data frame. The file output utility allows the user to write vector or bitmap images directly from the R object, in the style of `ggplot2`. User defined row- or column-wise clusters can be defined effortlessly by applying the `group_by` function from `dplyr` [@Hadley:2020] to the input data frame. Besides offering a modular and user-friendly interface, `tidyHeatmap` provides publication ready aesthetics such as `viridis` [@Garnier:2018] and `brewer` [@Neuwirth:2014] colour palettes and automatic sizing of row and column labels to avoid overlapping. 


![Heatmap of the pasilla dataset including grouping and multiple annotations. Some annotation data was simulated for visualisation purposes. \label{fig:example}](paper_tables_and_figures_files/figure-gfm/example_figure-1.png)

The input is a tidy data frame with the three basic columns including row and column element identifiers, and values that will be converted into colours. Additionally, further columns can include information about element grouping and annotation.

| element         | feature         | value     | annotation | group |
| --------------- | --------------- | --------- | ---------- | ----- |
| `chr` or `fctr` | `chr` or `fctr` | `numeric` | …          | …     |


The code interface consists of modular functions linked through the pipe operator (\autoref{fig:example}). Custom colour palette can be used passing an array of colours of a colour function (e.g., circlize [@Zuguang:2014]) to the palette argument of the annotation utilities.

```r
my_heatmap = 

	# Grouping
	input_df %>%
	group_by(location) %>%
		
	# Plotting
	heatmap(feature, element, value) %>%
    
	# Annotation
	add_tile(condition) %>%
	add_tile(act) %>%
	add_point(activation) %>%
	add_bar(size) %>%
	add_line(age)

# Saving
my_heatmap %>% save_pdf("my_file.pdf")
```

# Conclusions
The use of disjointed data structures demands time consuming and bug-prone information matching for performing complex tasks. Joint, tidy data frames allow to lower the cost/benefit ratio for the user, automatic a large part on data manipulation. `tidyHeatmap` introduces a modular paradigm for specifying information-rich heatmaps, just requiring column names as input. Due to its intuitive user interface and its advanced default aesthetic features, `tidyHeatmap` is ideal for the quick production of publication-ready heatmaps. This software is designed for modular expandability.

# Acknowledgements

We acknowledge contributions all the Papenfuss Lab for feedback, and Maria Doyle for constant support.

# References
