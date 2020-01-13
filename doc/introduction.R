## ---- echo=FALSE, include=FALSE-----------------------------------------------
library(knitr)
#library(kableExtra)
knitr::opts_chunk$set(cache = TRUE, warning = FALSE, 
                      message = FALSE, cache.lazy = FALSE)
#options(width = 120)
options(pillar.min_title_chars = Inf)


library(tibble)
library(dplyr)
library(magrittr) 
library(tidyr)
library(ggplot2)
library(readr)
library(widyr) 
library(foreach)
library(rlang) 
library(purrr)
library(ttBulk)

my_theme = 	
	theme_bw() +
	theme(
		panel.border = element_blank(),
		axis.line = element_line(),
		panel.grid.major = element_line(size = 0.2),
		panel.grid.minor = element_line(size = 0.1),
		text = element_text(size=12),
		legend.position="bottom",
		aspect.ratio=1,
		strip.background = element_blank(),
		axis.title.x  = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10)),
		axis.title.y  = element_text(margin = margin(t = 10, r = 10, b = 10, l = 10))
	)

# counts_mini = 
# 	ttBulk::counts %>% 
# 	filter(transcript %in% (ttBulk::X_cibersort %>% rownames)) %>% 
# 	filter(sample %in% c("SRR1740034", "SRR1740035", "SRR1740058", "SRR1740043", "SRR1740067")) %>%
# 	mutate(condition = ifelse(sample %in% c("SRR1740034", "SRR1740035", "SRR1740058"), TRUE, FALSE))


## -----------------------------------------------------------------------------
counts = ttBulk(ttBulk::counts_mini, sample, transcript, count)
counts 

## ----aggregate, cache=TRUE----------------------------------------------------
counts.aggr =  counts %>% aggregate_duplicates( 	aggregation_function = sum )

counts.aggr 



## ----normalise, cache=TRUE----------------------------------------------------
counts.norm =  counts.aggr %>% scale_abundance(method="TMM")

counts.norm %>% select(`count`, `count normalised`, `filter out low counts`, everything())

## ----plot_normalise, cache=TRUE-----------------------------------------------
counts.norm %>% 
	ggplot(aes(`count normalised` + 1, group=sample, color=`Cell type`)) +
	geom_density() + 
	scale_x_log10() +
	my_theme

## ----mds, cache=TRUE----------------------------------------------------------
counts.norm.MDS =  counts.norm %>% reduce_dimensions(.abundance = `count normalised`, method="MDS", .dims = 3)

counts.norm.MDS %>% select(sample, contains("Dim"), `Cell type`, time ) %>% distinct()

## ----plot_mds, cache=TRUE-----------------------------------------------------
counts.norm.MDS %>%
	select(contains("Dim"), sample, `Cell type`) %>%
  distinct() %>%
  GGally::ggpairs(columns = 1:3, ggplot2::aes(colour=`Cell type`))



## ----pca, cache=TRUE----------------------------------------------------------
counts.norm.PCA = counts.norm %>% reduce_dimensions(.abundance = `count normalised`, method="PCA" ,  .dims = 3)

counts.norm.PCA %>% select(sample, contains("PC"), `Cell type`, time ) %>% distinct()

## ----plot_pca, cache=TRUE-----------------------------------------------------
counts.norm.PCA %>%
	select(contains("PC"), sample, `Cell type`) %>%
  distinct() %>%
  GGally::ggpairs(columns = 1:3, ggplot2::aes(colour=`Cell type`))

## ----tsne, cache=TRUE---------------------------------------------------------
counts.norm.tSNE =
	ttBulk::breast_tcga_mini %>%
	ttBulk(sample, ens, `count`) %>%
	reduce_dimensions(
		.abundance = `count normalised`, 
		method = "tSNE", 
		top = 500, 
		perplexity=10, 
		pca_scale =TRUE
	) 

counts.norm.tSNE %>% 
	select(contains("tSNE", ignore.case = FALSE), sample, Call) %>%
	distinct()

counts.norm.tSNE %>% 
	select(contains("tSNE", ignore.case = FALSE), sample, Call) %>%
	distinct() %>%
	ggplot(aes(x = `tSNE1`, y = `tSNE2`, color=Call)) + geom_point() + my_theme

## ----rotate, cache=TRUE-------------------------------------------------------
counts.norm.MDS.rotated =
  counts.norm.MDS %>%
	rotate_dimensions(`Dim 1`, `Dim 2`, rotation_degrees = 45, .element = sample)

## ----plot_rotate_1, cache=TRUE------------------------------------------------
counts.norm.MDS.rotated %>%
	distinct(sample, `Dim 1`,`Dim 2`, `Cell type`) %>%
	ggplot(aes(x=`Dim 1`, y=`Dim 2`, color=`Cell type` )) +
  geom_point() +
  my_theme

## ----plot_rotate_2, cache=TRUE------------------------------------------------
counts.norm.MDS.rotated %>%
	distinct(sample, `Dim 1 rotated 45`,`Dim 2 rotated 45`, `Cell type`) %>%
	ggplot(aes(x=`Dim 1 rotated 45`, y=`Dim 2 rotated 45`, color=`Cell type` )) +
  geom_point() +
  my_theme

## ----de, cache=TRUE-----------------------------------------------------------
counts %>%	test_differential_abundance(  ~ condition,  action="get")

## ----adjust, cache=TRUE-------------------------------------------------------
counts.norm.adj =
	counts.norm %>%

	  # Add fake batch and factor of interest
	  left_join(
	  	(.) %>%
	  		distinct(sample) %>%
	  		mutate(batch = c(0,1,0,1,1))
	  ) %>%
	 	mutate(factor_of_interest = `Cell type` == "b_cell") %>%

	  # Add covariate
	  adjust_abundance(
	  	~ factor_of_interest + batch,
	  	.abundance = `count normalised`,
	  	action = "get"
	  )

counts.norm.adj

## ----cibersort, cache=TRUE----------------------------------------------------
counts.cibersort =
	counts %>%
	deconvolve_cellularity(action="add", cores=2)

counts.cibersort %>% select(sample, contains("type:")) %>% distinct()

## ----plot_cibersort, cache=TRUE-----------------------------------------------
counts.cibersort %>%
	select(contains("type:"), everything()) %>%
	gather(`Cell type inferred`, `proportion`, 1:22) %>%
  distinct(sample, `Cell type`, `Cell type inferred`, proportion) %>%
  ggplot(aes(x=`Cell type inferred`, y=proportion, fill=`Cell type`)) +
  geom_boxplot() +
  facet_wrap(~`Cell type`) +
  my_theme +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), aspect.ratio=1/5)

## ----cluster, cache=TRUE------------------------------------------------------
counts.norm.cluster = counts.norm %>%
  cluster_elements(.abundance = `count normalised`, method="kmeans",	centers = 2 )

counts.norm.cluster

## ----plot_cluster, cache=TRUE-------------------------------------------------
 counts.norm.MDS %>%
  cluster_elements(
  	.abundance = `count normalised`,
  	method="kmeans",
  	centers = 2
  ) %>%
	distinct(sample, `Dim 1`, `Dim 2`, `cluster kmeans`) %>%
	ggplot(aes(x=`Dim 1`, y=`Dim 2`, color=`cluster kmeans`)) +
  geom_point() +
  my_theme

## ----SNN, cache=TRUE----------------------------------------------------------
counts.norm.SNN =	counts.norm.tSNE %>%	cluster_elements(.abundance= `count normalised`, method = "SNN")

counts.norm.SNN %>% 
	select(contains("tSNE", ignore.case = FALSE), `cluster SNN`, sample) %>%
	distinct()

counts.norm.SNN %>% 
	select(contains("tSNE", ignore.case = FALSE), `cluster SNN`, sample, Call) %>%
	gather(source, Call, c("cluster SNN", "Call")) %>%
	distinct() %>%
	ggplot(aes(x = `tSNE1`, y = `tSNE2`, color=Call)) + geom_point() + facet_grid(~source) + my_theme


# Do differential transcription between clusters
counts.norm.SNN %>%
	mutate(factor_of_interest = `cluster SNN` == 3) %>%
	test_differential_abundance(
    ~ factor_of_interest,
    action="get"
   )

## ----drop, cache=TRUE---------------------------------------------------------
counts.norm.non_redundant = counts.norm.MDS %>%  remove_redundancy(	method = "correlation" )

## ----plot_drop, cache=TRUE----------------------------------------------------
counts.norm.non_redundant %>%
	distinct(sample, `Dim 1`, `Dim 2`, `Cell type`) %>%
	ggplot(aes(x=`Dim 1`, y=`Dim 2`, color=`Cell type`)) +
  geom_point() +
  my_theme


## ----drop2, cache=TRUE--------------------------------------------------------
counts.norm.non_redundant =
	counts.norm.MDS %>%
  remove_redundancy(
  	method = "reduced_dimensions",
  	.element = sample,
  	.feature = transcript,
  	Dim_a_column = `Dim 1`,
  	Dim_b_column = `Dim 2`
  )

## ----plot_drop2, cache=TRUE---------------------------------------------------
counts.norm.non_redundant %>%
	distinct(sample, `Dim 1`, `Dim 2`, `Cell type`) %>%
	ggplot(aes(x=`Dim 1`, y=`Dim 2`, color=`Cell type`)) +
  geom_point() +
  my_theme


## ----eval=FALSE---------------------------------------------------------------
#  counts = bam_sam_to_featureCounts_tibble(
#  	file_names,
#  	genome = "hg38",
#  	isPairedEnd = TRUE,
#  	requireBothEndsMapped = TRUE,
#  	checkFragLength = FALSE,
#  	useMetaFeatures = TRUE
#  )

## ----ensembl, cache=TRUE------------------------------------------------------
counts_ensembl %>% annotate_symbol(ens)

## ---- cache=TRUE--------------------------------------------------------------
  counts.norm 

## ---- cache=TRUE--------------------------------------------------------------
  counts.norm %>%
    reduce_dimensions(
    	.abundance = `count normalised`, 
    	method="MDS" , 
    	.element = sample, 
    	.feature = transcript, 
    	.dims = 3, 
    	action="add"
    )

## ---- cache=TRUE--------------------------------------------------------------
  counts.norm %>%
    reduce_dimensions(
    	.abundance = `count normalised`, 
    	method="MDS" , 
    	.element = sample, 
    	.feature = transcript, 
    	.dims = 3, 
    	action="get"
    )

## -----------------------------------------------------------------------------
sessionInfo()

