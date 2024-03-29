# `tmfast`: Fast fitting of topic models using PCA + varimax

This package implements an approach to quickly fitting topic models, combining partial PCA for sparse matrices with a varimax rotation, proposed by Rohe and Zang (<https://arxiv.org/abs/2004.05387>).  In simulation, as implemented here this method runs roughly an order of magnitude faster than structural topic models from the `stm` package.  The method is also deterministic and does not introduce research degrees of freedom through the Bayesian priors of LDA. 

Beyond fitting the topic models, the package includes (a) functions for [my information-theoretic approach to vocabulary selection](https://direct.mit.edu/qss/article/2/3/990/106952/Productivity-and-interdisciplinary-impacts-of); (b) tidiers, for extracting both word-topic and topic-document matrices into a tidyverse workflow; (c) Hellinger distance calculations and t-SNE and UMAP visualization for [my "discursive space" analysis](https://direct.mit.edu/qss/article/2/3/990/106952/Productivity-and-interdisciplinary-impacts-of); and (d) samplers to construct simulated corpora. 

## Installation

```
remotes::install_github("dhicks/tmfast")
```
or fork <https://github.com/dhicks/tmfast>, clone, and install manually. 

