## This works okay! 
## - log1p doc term counts (don't divide by length)
## - prcomp with centering but *without* scaling (scaling makes it harder to get probability distributions)
## - after rotation, reverse factors with negative skew
## - trim loadings/scores at 0, then normalize to construct probability distributions
## - if comparing beta (fitted word-topic) to phi (true word-topic), need to account for change in order of words (and dropped words)
## - if comparing fitted & true topic-doc dists, need to account for change in order of topics
## minimum Hellinger distance of word-topic distributions in these tests ~.16
## Journal w/ giant papers seems to be a little muddier, w/ minimum ~.22

library(psych)
library(tidyverse)
library(tidytext)
library(irlba)

source('R/generators.R')
source('R/hellinger.R')

## Parameters ----
k = 5              # Num. topics / journals
Mj = 100           # Num. documents per journal
M = Mj*k
vocab = 3*M       # Vocabulary length

size = 3      # Size and mean for the negative binomial distribution of doc lengths
mu = 300

## Build journal-specific topic distributions ----
## Journal-specific alpha, with a peak value (.8 by default) and uniform otherwise
alpha = function(k, j, peak = .8) {
    alpha = rep((1 - peak)/(k-1), k)
    alpha[j] = peak
    return(alpha)
}

theta = map(1:k, ~rdirichlet(Mj, alpha(k, .x, peak = .8))) %>% 
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
phi = rdirichlet(k, .1, k = vocab)

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
## 
N[1:Mj] = 10*N[1:Mj]

hist(N)

corpus = draw_corpus(N, theta, phi)
dtm = corpus |> 
    group_by(doc) |>
    mutate(len = sum(n),
           n = log1p(n)) |>
    ungroup() |>
    cast_sparse(row = doc, col = word, value = n)

words = colnames(dtm)


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


## Varimax rotation ----
raw_loadings = pca_fit$rotation[,1:k] %*% diag(pca_fit$sdev, k, k) |> 
    magrittr::set_rownames(words)
varimax_fit_prelim = varimax(raw_loadings)
skew(varimax_fit_prelim$loadings)

## Reverse factors with negative skew (left tails)
varimax_fit = map(varimax_fit_prelim, 
                  ~ .x %*% diag(1 - 2*(skew(varimax_fit_prelim$loadings) < 0)))
skew(varimax_fit$loadings)



## Scores ----
scores = scale(pca_fit$x[,1:k]) %*% varimax_fit$rotmat
# scores = pca_fit$x[,1:k] %*% varimax_fit$rotmat
skew(scores)

## scores x loadings should approximate dtm
all(colnames(dtm) == colnames(scores %*% t(varimax_fit$loadings)))
{dtm - scores %*% t(varimax_fit$loadings)} |>
    as.numeric() |>
    summary()


scores_df = scores |> 
    as_tibble(rownames = 'doc') |> 
    mutate(doc = as.integer(doc)) |> 
    pivot_longer(starts_with('V'), 
                 names_to = 'factor', 
                 values_to = 'score')

ggplot(scores_df, aes(factor, score)) +
    geom_violin(draw_quantiles = .5)

ggplot(scores_df, aes(doc, factor, fill = score)) +
    geom_tile() +
    scale_fill_gradient2()

## Crude accuracy check
# scores_df |> 
#     left_join(theta_df, by = 'doc') |> 
#     group_by(doc) |> 
#     filter((score) == max((score)), prob == max(prob)) |> 
#     ungroup() |> 
#     # count(topic, factor) |> view()
#     ggplot(aes(topic, factor)) +
#     geom_tile(stat = 'bin2d') +
#     theme_minimal()


## Better accuracy check, using the method from Malaterre and Lareau 2022 ----
## True word-topic distribution matrix
# t(phi)

## beta: fitted varimax loadings, transformed to probability distributions
beta = varimax_fit$loadings |> 
    as_tibble(rownames = 'word') |> 
    pivot_longer(starts_with('V'), 
                 names_to = 'factor', 
                 values_to = 'loading') |> 
    group_by(factor) |> 
    ## Nudge smallest loading to 0, then normalize to sum to 1
    ## Crappy results! 
    # mutate(loading = loading - min(loading)) |>
    ## Trim at 0, then normalize to sum to 1
    ## Seems to work a little better
    filter(loading > 0) |>
    mutate(loading = loading / sum(loading)) |> 
    ungroup() |> 
    pivot_wider(names_from = 'factor', values_from = 'loading', values_fill = 0) |> 
    ## Fix order of words
    mutate(word = as.integer(word)) |> 
    arrange(word) |> 
    ## And dropped words
    complete(word = 1:vocab, fill = list(V1 = 0, V2 = 0, V3 = 0, V4 = 0, V5 = 0)) |> 
    column_to_rownames('word') |> 
    as.matrix()

## Compare Zipfian distributions
bind_rows({beta |> 
        as_tibble(rownames = 'word') |> 
        rename_with(.cols = starts_with('V'), ~ str_replace(.x, 'V', 'fa')) |> 
        pivot_longer(starts_with('fa'), 
                     names_to = 'dim', 
                     values_to = 'value') |> 
        mutate(type = 'fitted')},
        {phi |> 
                t() |> 
                as_tibble(rownames = 'word') |> 
                pivot_longer(starts_with('V'), 
                             names_to = 'dim', 
                             values_to = 'value') |> 
                mutate(type = 'true')}
) |> 
    group_by(dim) |> 
    mutate(rank = rank(desc(value))) |> 
    arrange(dim, rank) |> 
    filter(rank < 500) |>
    ggplot(aes(rank, value, color = type, group = dim)) +
    geom_line() +
    scale_y_log10() +
    scale_x_log10()

## Hellinger distance
hellinger_(phi, t(beta))



## Convert scores to topics ----
## Not all documents have at least one positive score
scores_df |>
    filter(score > 0) |>
    distinct(doc) |>
    nrow()

## Nudge everything so the minimum value is 0? 
gamma_df = scores_df |> 
    # filter(score > 0) |> 
    group_by(doc) |> 
    mutate(score = score - min(score)) |> 
    mutate(gamma = score / sum(score)) |> 
    ungroup() |> 
    rename(topic = factor)

gamma_df |> 
    mutate(journal = (doc - 1) %/% Mj + 1) |> 
    ggplot(aes(topic, gamma, group = doc, color = as.factor(journal))) +
    geom_line(alpha = .25) +
    facet_wrap(vars(journal), scales = 'free_x')

## Want to do a Hellinger distance comparison for topic-docs, but this requires first reordering fitted topics to make true ones