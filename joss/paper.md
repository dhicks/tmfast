---
title: 'tmfast: Fast Topic Models via Varimax-Rotated PCA'
authors:
  - name: D. Hicks
    orcid: 0000-0001-7945-4416
    affiliation: 1
affiliations:
  - index: 1
    name: Department of Philosophy, University of California, Merced, USA
date: 20 May 2026
bibliography: paper.bib
---

# Summary

Topic modeling is a widely used natural language processing technique for discovering latent thematic structure in large text corpora. Most topic modeling approaches — including Latent Dirichlet Allocation (LDA) — rely on Bayesian inference, which is computationally intensive, stochastic, and introduces researcher degrees of freedom through the choice of prior distributions.

`tmfast` is an R package that implements a fast, deterministic alternative using varimax-rotated principal component analysis (PCA), following the "vintage factor analysis" approach of @RoheVintageFactorAnalysis2020. The package leverages truncated PCA via the `irlba` package [@BaglamaIrlbaFastTruncated2022] to efficiently handle the sparse term-document matrices typical of text data, fitting models roughly an order of magnitude faster than the widely used `stm` package [@RobertsStmPackageStructural2019]. Because fitting is deterministic, results are fully reproducible across runs without setting a random seed. Additional features include an information-theoretic approach to vocabulary selection, `broom`-compatible tidiers for extracting word-topic and topic-document distributions into tidy data workflows, and simulation samplers for benchmarking and method evaluation.

# Statement of Need

Topic modeling is widely used by computational social scientists, digital humanists, and researchers working with large text corpora to discover latent thematic structure [@BleiLatentDirichletAllocation2003; @RobertsStmPackageStructural2019]. A key advantage over other dimensionality reduction methods is that topic modeling simultaneously clusters both terms and documents, enabling analysts to assign human-meaningful, domain-specific labels to the discovered topics.

However, standard topic modeling packages are slow, stochastic, and introduce researcher degrees of freedom through the choice of Bayesian prior distributions. Because fitting even a single model is computationally expensive, analysts rarely fit and compare multiple models — arguably the most principled way to assess sensitivity of results to methodological choices [@GelmanGardenForkingPaths2013; @SteegenIncreasingTransparencyMultiverse2016]. Instead, they typically settle on a single "best" model selected by informal assessments of topic interpretability, itself an additional source of researcher degrees of freedom.

`tmfast` addresses these problems directly. Its algebraic fitting algorithm is deterministic — results are fully reproducible without setting a random seed — and requires no specification of Bayesian priors. In benchmarks, `tmfast` fits topic models approximately 20× faster than `stm` [@RobertsStmPackageStructural2019], making it practical to fit and compare many models as part of a routine analysis workflow.

# State of the Field

Topic modeling in R is dominated by packages implementing Latent Dirichlet Allocation (LDA)-based approaches [@BleiLatentDirichletAllocation2003]. The `topicmodels` package [@GrunTopicmodels2011] provides LDA and correlated topic models via variational Bayes and Gibbs sampling. The `stm` package [@RobertsStmPackageStructural2019] extends LDA to incorporate document-level metadata as covariates on topic prevalence and content, and is generally regarded as the state of the art for social-scientific text analysis in R. Both packages rely on iterative Bayesian inference, which is computationally intensive, stochastic (requiring random seeds for reproducibility), and sensitive to prior specification.

@RoheVintageFactorAnalysis2020 propose an alternative approach, approaching topic modeling using principal component analysis (PCA) followed by a varimax rotation. They show (Lemma 5.2) that the term-document occurrence rate matrix can be approximately factored into term-topic and topic-document distributions — the same quantities estimated by LDA — and that PCA with varimax rotation provides statistically consistent estimates of these latent distributions. The varimax rotation promotes sparsity in the estimated factor loadings, giving topics the interpretability property that makes them useful: each topic is characterized by a small number of high-weight terms, and each document is associated with a small number of dominant topics.

The PCA-based approach offers two practical advantages over LDA. First, it is deterministic and fully reproducible without random seeds. Second, efficient partial SVD algorithms for sparse matrices [@BaglamaAugmentedImplicitlyRestarted2005] make it substantially faster than iterative Bayesian methods — in a test case, `tmfast` fits topic models approximately 20× faster than `stm`.

# Software Design

## Fitting pipeline

`tmfast` fits topic models in two steps. First, `irlba::prcomp_irlba()` is used to compute a truncated PCA of the document-term matrix, retaining only the top principal components needed for the largest requested number of topics ($k$). Because PCA is computed only once, multiple topic numbers can be extracted from a single fit, applying varimax rotation at each requested $k$ without repeating the truncated PCA step.

Second, `stats::varimax()` is used to apply an orthogonal rotation to the top-$k$ loading matrix for each value of $k$. To ensure consistent sign conventions, factors with negative skew are automatically reflected; this makes topics "peak" rather than "valley" shaped without affecting any statistical properties.

The output is a `tmfast` object storing the PCA fit, varimax rotation matrices, and rotated loadings and scores for each requested $k$.

## Vocabulary selection

For large corpora, restricting the vocabulary before fitting substantially reduces memory use and fitting time. `tmfast` provides two information-theoretic vocabulary selection metrics as alternatives to TF-IDF.

`ndH()` measures how much information a term carries beyond what would be expected if it were distributed uniformly across documents. More precisely, it computes $\Delta H = \log_2(D) - H(p)$, where $D$ is the number of documents, $H(p)$ is the observed entropy of the term's document distribution, and the result is weighted by $\log_2(n)$ (total term count) to down-weight rare terms that may be OCR artifacts or misspellings: $\mathit{ndH} = \log_2(n) \cdot \Delta H$.

`ndR()` uses a length-proportional document distribution as the baseline rather than a uniform one, computing the KL divergence of the observed distribution from the baseline: $\Delta R = \sum_d p(d \mid w) \log_2 [p(d \mid w) / p(d)]$, then weighted by $\log_2(n)$ as before. `ndR` is preferable when documents vary significantly in length, e.g., mixing short news articles with long books, because a term concentrated in short documents scores higher under `ndR` than under `ndH`.

In practice, analysts select a target vocabulary size and retain the top-ranked terms by either metric.

## Tidiers and post-processing

`tmfast` implements `broom`-compatible a `tidy()` method that extracts word-topic ($\beta$) and topic-document ($\gamma$) distributions as long-format data frames, compatible with `tidyverse` [@WickhamWelcomeTidyverse2019] workflows and the `tidytext` package [@SilgeTextMiningTidy2017]. The `tidy()` method requires specifying the value of $k$; `tidy_all()` is provided as a convenience for extracting all values of $k$ included in the `tmfast` object in a single line. 

Because varimax loadings can be negative and do not sum to one, the tidiers apply a softmax transformation: $\beta_{wt} = \exp(L_{wt}) / \sum_w \exp(L_{wt})$ for word $w$ and topic $t$, converting raw loadings into proper probability distributions. The same transformation is applied to document scores for $\gamma$.

Softmax-normalized distributions tend to be flatter than the Dirichlet distributions that LDA assumes. To address this, `tidy()` accepts an optional `exponent` argument that applies a power renormalization: $\beta'_{wt} = \beta_{wt}^e / \sum_w \beta_{wt}^e$. Higher exponents sharpen distributions toward fewer dominant terms. `target_power()` automates the choice of exponent by finding the value whose output matches a researcher-specified target entropy, with the target computed using `expected_entropy()` (the expected Shannon entropy of a symmetric Dirichlet with concentration parameter $\alpha$). This replaces Bayesian prior specification in LDA, with a similar effect: the analyst chooses how concentrated or spread they want topics and documents to be. This also introduces an important research degree of freedom. 

## Simulation samplers

`tmfast` includes a suite of simulation tools for benchmarking and method evaluation. `rdirichlet()` samples from the Dirichlet distribution with symmetric or asymmetric concentration parameters. `peak_alpha()` constructs an asymmetric concentration vector with a single dominant component, useful for generating "pure" documents that belong primarily to one topic. `draw_corpus()` implements the standard LDA generative process: for each document, it draws a topic mixture $\theta_d \sim \text{Dirichlet}(\alpha)$, then draws each word from the corresponding topic-word distribution $\phi_k$. When the `furrr` package is available, corpus generation is parallelized automatically.

`journal_specific()` provides a complete benchmarking simulation: it generates a corpus with known true topic and word distributions ("specific" to each simulated "journal"), fits `tmfast`, aligns fitted topics to true topics as the linear assignment problem , and returns recovery accuracy as mean Hellinger distances for both $\phi$ (true word-topic) and $\theta$ (true topic-document) distributions. This enables systematic evaluation of how recovery accuracy scales with corpus size, vocabulary size, and number of topics.

# Research Impact Statement

`tmfast` is available as an R package on CRAN (version 0.1.1, released May 2026) and as an arXiv preprint (arXiv:2305.01535, posted May 2023). The package was originally developed to support the text analysis in @HicksRaceScienceMainstream2025, which used topic models to study race science in mainstream psychology journals from 1960 to 2010; it has subsequently been developed as a standalone package for the broader text mining community.

# AI Usage Disclosure

