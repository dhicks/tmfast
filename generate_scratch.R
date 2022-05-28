library(tidyverse)

source('R/generators.R')

## Parameters ----
k = 5           # Num. topics
M = 300         # Num. documents
vocab = 10*M    # Vocabulary length

size = 5      # Size and mean for the negative binomial distribution of doc lengths
mu = 300

## Sampling ----
## theta_i: Topic distribution for document i
theta = rdirichlet(M, .1, k = 3)



## phi_j:  Word distribution for topic j
phi = rdirichlet(k, .1, k = vocab)

## N_i:  Length of document i
N = rnbinom(M, size = size, mu = mu)

draw_corpus(N, theta, phi)
