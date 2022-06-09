## This works okay! 
## - log1p doc term counts (don't divide by length)
## - prcomp with centering but *without* scaling (scaling makes it harder to get probability distributions)
## - after rotation, reverse factors with negative skew
## - trim loadings/scores at 0, then normalize to construct probability distributions
## - if comparing beta (fitted word-topic) to phi (true word-topic), need to account for change in order of words (and dropped words)
## - if comparing fitted & true topic-doc dists, need to account for change in order of topics
## minimum Hellinger distance of word-topic distributions in these tests ~.16
## Journal w/ giant papers seems to be a little muddier, w/ minimum ~.22

# library(psych)
library(tidyverse)
# library(tidytext)
# library(irlba)
library(lpSolve)
library(tictoc)

source('R/generators.R')
source('R/hellinger.R')
source('R/tmfast.R')

## Parameters ----
k = 5              # Num. topics / journals
Mj = 20           # Num. documents per journal
M = Mj*k
vocab = M       # Vocabulary length

size = 3      # Size and mean for the negative binomial distribution of doc lengths
mu = 300

## Build journal-specific topic distributions ----
## Journal-specific alpha, with a peak value (.8 by default) and uniform otherwise
theta = map(1:k, ~rdirichlet(Mj, peak_alpha(k, .x, peak = .8, scale = 10))) %>% 
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
phi = rdirichlet(k, .01, k = vocab)

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
# N[1:Mj] = 10*N[1:Mj]

hist(N)

corpus = draw_corpus(N, theta, phi)
dtm = corpus |> 
    group_by(doc) |>
    mutate(len = sum(n),
           n = log1p(n)) |>
    ungroup()
    # cast_sparse(row = doc, col = word, value = n)

## Explore fitted model ----
tic()
fitted = tmfast(dtm, c(2, 3, k, 2*k))
toc()

# tidy(fitted, 3, matrix = 'gamma') |> 
#     pivot_wider(names_from = 'topic', 
#                 values_from = 'gamma') |> 
#     mutate(journal = (as.integer(doc) - 1) %/% Mj + 1) |> 
#     ggplot(aes(V1, V2)) +
#     geom_jitter() +
#     facet_wrap(vars(journal))

## TODO: screeplot

## Variance coverage? 
cumsum(fitted$sdev^2) / fitted$totalvar

## Scores all have positive skew
## TODO: easier access to varimax contents
psych::skew(scores(fitted, k))



## Accuracy check, using the method from Malaterre and Lareau 2022 ----
## True word-topic distribution matrix
# t(phi)

## beta: fitted varimax loadings, transformed to probability distributions
beta = tidy(fitted, k, 'beta')

## Compare Zipfian distributions
bind_rows({beta |> 
        mutate(type = 'fitted')},
        {phi |> 
                t() |> 
                as_tibble(rownames = 'token') |> 
                pivot_longer(starts_with('V'), 
                             names_to = 'topic', 
                             values_to = 'beta') |> 
                mutate(type = 'true')}
) |> 
    group_by(type, topic) |> 
    mutate(rank = rank(desc(beta))) |> 
    arrange(type, topic, rank) |> 
    filter(rank < 500) |>
    ggplot(aes(rank, beta, color = type, group = interaction(topic, type))) +
    geom_line() +
    scale_y_log10() +
    scale_x_log10()

## Hellinger distance of word-topic distributions
beta_mx = beta |> 
    ## Fix order of words
    mutate(token = as.integer(token)) |>
    arrange(token) |>
    ## And dropped words
    complete(token = 1:vocab) |>
    pivot_wider(names_from = 'topic',
                values_from = 'beta', values_fill = 0,
                names_sort = TRUE) |>
    select(-`NA`) |> 
    ## Coerce to matrix
    column_to_rownames('token') |>
    as.matrix()
hellinger(phi, t(beta_mx))

## Use lpSolve to match fitted topics to true topics
dist = hellinger(phi, t(beta_mx))
soln = lp.assign(dist)
soln$solution

hellinger(phi, soln$solution %*% t(beta_mx))
hellinger(phi, soln$solution %*% t(beta_mx)) |> 
    diag() |> 
    summary()

## Tidy scores ----
# scores_df = tidy(fitted, k, 'gamma', rotation = soln$solution) #%>% 
#     
# ggplot(scores_df, aes(topic, gamma)) +
#     geom_violin(draw_quantiles = .5)
# 
# ## This confirms graphically that the docs mostly have the correct topics
# ggplot(scores_df, aes(as.integer(doc), topic, fill = gamma)) +
#     geom_tile() +
#     scale_fill_gradient2()

## Scores to topic distributions ----
## Not all documents have at least one positive score
# scores_df |>
#     filter(score > 0) |>
#     distinct(doc) |>
#     nrow()

gamma_df = tidy(fitted, k, 'gamma', rotation = soln$solution)

gamma_df |> 
    mutate(doc = as.integer(doc)) |> 
    ggplot(aes(doc, topic, fill = gamma)) +
    geom_raster() +
    scale_x_continuous(breaks = NULL)

gamma_df |> 
    mutate(doc = as.integer(doc), 
           journal = (doc - 1) %/% Mj + 1) |> 
    ggplot(aes(topic, gamma, group = doc, color = as.factor(journal))) +
    geom_line(alpha = .25) +
    facet_wrap(vars(journal), scales = 'free_x') +
    scale_color_discrete(guide = 'none')

## Accuracy of topic-doc distributions ----
doc_compare = hellinger(rename(theta_df, gamma = prob), 'doc', 
          topics2 = gamma_df, id2 = 'doc', 
          df = TRUE)

ggplot(doc_compare, aes(as.integer(doc_x), as.integer(doc_y), fill = 1 - dist)) +
    geom_raster() +
    scale_x_discrete(breaks = NULL) +
    scale_y_discrete(breaks = NULL)

## Comparable to word-topics, mean around .17
doc_compare |> 
    filter(doc_x == doc_y) |> 
    pull(dist) |> 
    summary()
