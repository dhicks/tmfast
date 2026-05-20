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

`tmfast` is an R package that implements a fast, deterministic alternative using varimax-rotated principal component analysis (PCA), following the "vintage factor analysis" approach of @RoheVintageFactorAnalysis2020. The package leverages truncated PCA via the `irlba` package [@BaglamaIrlbaFastTruncated2022] to efficiently handle the sparse term-document matrices typical of text data, fitting models roughly an order of magnitude faster than the widely used `stm` package [@RobertsStmPackageStructural2019]. Because fitting is algebraic rather than iterative, results are fully reproducible across runs without setting a random seed. Additional features include an information-theoretic approach to vocabulary selection, `broom`-compatible tidiers for extracting word-topic and topic-document distributions into tidy data workflows, and simulation samplers for benchmarking and method evaluation.

# Statement of Need

Topic modeling is widely used by computational social scientists, digital humanists, and researchers working with large text corpora to discover latent thematic structure [@BleiLatentDirichletAllocation2003; @RobertsStmPackageStructural2019]. A key advantage over other dimensionality reduction methods is that topic modeling simultaneously clusters both terms and documents, enabling analysts to assign human-meaningful, domain-specific labels to the discovered topics.

However, standard topic modeling packages are slow, stochastic, and introduce researcher degrees of freedom through the choice of Bayesian prior distributions. Because fitting even a single model is computationally expensive, analysts rarely fit and compare multiple models — arguably the most principled way to assess sensitivity of results to methodological choices [@GelmanGardenForkingPaths2013; @SteegenIncreasingTransparencyMultiverse2016]. Instead, they typically settle on a single "best" model selected by informal assessments of topic interpretability, itself an additional source of researcher degrees of freedom.

`tmfast` addresses these problems directly. Its algebraic fitting algorithm is deterministic — results are fully reproducible without setting a random seed — and requires no specification of Bayesian priors. In benchmarks, `tmfast` fits topic models approximately 20× faster than `stm` [@RobertsStmPackageStructural2019], making it practical to fit and compare many models as part of a routine analysis workflow.

# State of the Field

Topic modeling in R is dominated by packages implementing Latent Dirichlet Allocation (LDA)-based approaches [@BleiLatentDirichletAllocation2003]. The `topicmodels` package [@GrunTopicmodels2011] provides LDA and correlated topic models via variational Bayes and Gibbs sampling. The `stm` package [@RobertsStmPackageStructural2019] extends LDA to incorporate document-level metadata as covariates on topic prevalence and content, and is generally regarded as the state of the art for social-scientific text analysis in R. Both packages rely on iterative Bayesian inference, which is computationally intensive, stochastic (requiring random seeds for reproducibility), and sensitive to prior specification.

@RoheVintageFactorAnalysis2020 propose an alternative approach, approaching topic modeling using principal component analysis (PCA) followed by a varimax rotation. They show (Lemma 5.2) that the term-document occurrence rate matrix can be approximately factored into term-topic and topic-document distributions — the same quantities estimated by LDA — and that PCA with varimax rotation provides statistically consistent estimates of these latent distributions. The varimax rotation promotes sparsity in the estimated factor loadings, giving topics the interpretability property that makes them useful: each topic is characterized by a small number of high-weight terms, and each document is associated with a small number of dominant topics.

The PCA-based approach offers two practical advantages over LDA. First, it is algebraic rather than iterative, making it deterministic and fully reproducible without random seeds. Second, efficient partial SVD algorithms for sparse matrices [@BaglamaAugmentedImplicitlyRestarted2005] make it substantially faster than iterative Bayesian methods — in a test case, `tmfast` fits topic models approximately 20× faster than `stm`.

# Software Design


# Research Impact Statement


# AI Usage Disclosure

