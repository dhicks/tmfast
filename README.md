# `tmfast`: Fast fitting of topic models using PCA + varimax

This package implements an approach to quickly fitting topic models, combining partial PCA for sparse matrices with a varimax rotation, proposed by Rohe and Zang (<https://arxiv.org/abs/2004.05387>).  In simulation, as implemented here this method runs roughly an order of magnitude faster than structural topic models from the `stm` package.  The method is also deterministic and does not introduce research degrees of freedom through the Bayesian priors of LDA. 

Beyond fitting the topic models, the package includes (a) functions for [my information-theoretic approach to vocabulary selection](https://doi.org/10.1162/qss_a_00150); (b) tidiers, for extracting both word-topic and topic-document matrices into a tidyverse workflow; (c) Hellinger distance calculations and t-SNE and UMAP visualization for [my "discursive space" analysis](https://doi.org/10.1162/qss_a_00150); and (d) samplers to construct simulated corpora. 

A preprint discussing the package is available on [the arXiv](https://arxiv.org/abs/2305.01535). 

## Installation

```
install.packages("tmfast")
```

Or the development version:
```
remotes::install_github("dhicks/tmfast")
```
or fork <https://github.com/dhicks/tmfast>, clone, and install manually. 

If you wish to build the "real books" vignette from scratch, you'll need the [`tmfast.realbooks` data package](https://github.com/dhicks/tmfast.realbooks). To install this data package use `remotes`: 
```{r}
#| eval: false
remotes::install_github('dhicks/tmfast.realbooks')
```
or specify the drat repository:
```{r}
#| eval: false
install.packages('tmfast.realbooks', repos = 'https://dhicks.github.io/drat/')
```
