library(tidyverse)
library(tidytext)
library(irlba)

source('R/generators.R')

## Parameters ----
k = 5              # Num. topics / journals
Mj = 100           # Num. documents per journal
M = Mj*k
vocab = 5*M       # Vocabulary length

size = 3      # Size and mean for the negative binomial distribution of doc lengths
mu = 300

## Build journal-specific topic distributions ----
## Journal-specific alpha, with a peak value (.8 by default) and uniform otherwise
alpha = function(k, j, peak = .8) {
    alpha = rep((1 - peak)/(k-1), k)
    alpha[j] = peak
    return(alpha)
}

theta = map(1:k, ~rdirichlet(Mj, alpha(k, .x))) %>% 
    do.call(rbind, .)

theta_df = theta |> 
    as_tibble(rownames = 'doc') |> 
    mutate(doc = as.integer(doc)) |> 
    pivot_longer(starts_with('V'), 
                 names_to = 'topic', 
                 values_to = 'prob')
ggplot(theta_df, aes(doc, topic, fill = prob)) +
    geom_tile()

## Build the rest of the dtm ----
## phi_j:  Word distribution for topic j
phi = rdirichlet(k, 200/vocab, k = vocab)

## Word distributions
phi |> 
    as_tibble(rownames = 'topic') |> 
    pivot_longer(starts_with('V'), 
                 names_to = 'word', 
                 values_to = 'prob') |> 
    ggplot(aes(topic, word, fill = (prob))) +
    geom_tile() +
    scale_y_discrete(breaks = NULL)

## Zipf's law
phi |> 
    as_tibble(rownames = 'topic') |> 
    pivot_longer(starts_with('V'), 
                 names_to = 'word', 
                 values_to = 'prob') |> 
    group_by(topic) |> 
    mutate(rank = rank(desc(prob))) |> 
    arrange(topic, rank) |> 
    filter(rank < vocab/2) |>
    ggplot(aes(rank, prob, color = topic)) +
    geom_line() +
    scale_x_log10() +
    scale_y_log10()


## N_i:  Length of document i
N = rnbinom(M, size = size, mu = mu)
## Make journal #1 10x the length of the others
## - no adjustment: 1. factor is journal 1. PC and topic; recall is lousy
## - Simple length normalization (n_word / sum(n_word)) seems to cause 2 topics to collapse together, recall is terrible for one topic
## - log10(n_word / sum(n_word)): restores vanished topic, recall is okay across all topics
## - log10(n_word):  recall is about as good as log length normalization
## - sqrt(n_word):  recall comparable to log
## - log10(sqrt(n_word)):  recall comparable to log
## - PCA scaling: comparable to no adjustment
##     - scaling does help w/ screeplot
## - log + PCA scaling: maybe worse than log alone
## - log length norm + PCA scaling: comparable to log length norm
## - **log1p seems to work well**
N[1:Mj] = 10*N[1:Mj]

hist(N)

corpus = draw_corpus(N, theta, phi)
dtm = corpus |> 
    group_by(doc) |>
    mutate(len = sum(n),
           n = log1p(n)) |>
    ungroup() |>
    cast_sparse(row = doc, col = word, value = n)


## Simple PCA + varimax ---
# library(tictoc)
# tic()
# pca_trad = prcomp(dtm)
# toc()
# tic()
pca_fit = prcomp_irlba(dtm, n = ceiling(10*k), scale. = FALSE)
# toc()

## Screeplot indicates k factors
screeplot(pca_fit)

## But note that this is less than 80% of the variance
cumsum(pca_fit$sdev^2) / pca_fit$totalvar

raw_loadings = pca_fit$rotation[,1:k] %*% diag(pca_fit$sdev, k, k)

varimax_fit = varimax(raw_loadings)

scores = scale(pca_fit$x[,1:k]) %*% varimax_fit$rotmat

scores_df = scores |> 
    as_tibble(rownames = 'doc') |> 
    mutate(doc = as.integer(doc)) |> 
    pivot_longer(starts_with('V'), 
                 names_to = 'factor', 
                 values_to = 'value')

ggplot(scores_df, aes(value)) +
    geom_density() +
    facet_wrap(vars(factor))

ggplot(scores_df, aes(doc, factor, fill = abs(value))) +
    geom_tile() +
    scale_fill_gradient2()

scores_df |> 
    left_join(theta_df, by = 'doc') |> 
    group_by(doc) |> 
    filter(abs(value) == max(abs(value)), prob == max(prob)) |> 
    ungroup() |> 
    count(topic, factor) |> view()
    ggplot(aes(topic, factor)) +
    geom_tile(stat = 'bin2d') +
    theme_minimal()
