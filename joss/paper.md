---
title: 'tmfast: Fast Topic Models via Varimax-Rotated PCA'
authors:
  - name: Daniel J. Hicks
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


# State of the Field


# Software Design


# Research Impact Statement


# AI Usage Disclosure

