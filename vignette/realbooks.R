library(tidyverse)
library(tidytext)
library(janeaustenr)
library(gutenbergr)

library(tmfast)
library(stm)

library(tictoc)

## Build corpus ----
austen_df = austen_books() |>
    unnest_tokens(term, text, token = 'words') |>
    mutate(author = 'Jane Austen') |>
    count(author, book, term)

bronte = gutenberg_download(c(1260, 768, 969, 9182, 767),
                            meta_fields = c('title'))

bronte_df = bronte |>
    unnest_tokens(term, text, token = 'words') |>
    mutate(author = 'Brontë sisters') |>
    count(author, book = title, term)

# gutenberg_authors |>
#     filter(str_detect(author, 'Dickens'))
# gutenberg_works(gutenberg_author_id == 37) |>
#     view()
dickens = gutenberg_download(c(98, 730, 766, 786),
                             meta_fields = c('title'))

dickens_df = dickens |>
    unnest_tokens(term, text, token = 'words') |>
    mutate(author = 'Charles Dickens') |>
    count(author, book = title, term)

hgwells = gutenberg_download(c(35, 36, 5230, 159),
                             meta_fields = c('title'))

wells_df = hgwells |>
    unnest_tokens(term, text, token = 'words') |>
    mutate(author = 'H.G. Wells') |>
    count(author, book = title, term)

dataf = bind_rows(austen_df, bronte_df, dickens_df, wells_df)

meta = dataf |>
    group_by(author, book) |>
    summarize(n = sum(n))

count(dataf, book)

nbooks = dataf |>
    pull(book) |>
    n_distinct()

## Vocabulary selection ----
H_df = dataf |>
    group_by(term) |>
    mutate(p = n / sum(n),
           H_term = -p*log2(p)) |>
    summarize(dH = log2(nbooks) - sum(H_term), n = sum(n)) |>
    mutate(ndH = log10(n)*dH,
           in_vocab = rank(desc(ndH)) <= 1000) |>
    arrange(desc(ndH))

ggplot(H_df, aes(log10(n), dH, color = in_vocab)) +
    geom_point()

vocab = H_df |>
    filter(in_vocab) |>
    pull(term)

## Fast TM ----
dtm = dataf |>
    filter(term %in% vocab) |>
    mutate(n = log1p(n))

tic()
fitted_tmf = tmfast(dtm, n = c(3, 4, 8),
                row = book, column = term, value = n)
toc()

screeplot(fitted_tmf)

tidy(fitted_tmf, 4, matrix = 'gamma') |>
    left_join(meta, by = c('document' = 'book')) |>
    ggplot(aes(document, gamma, fill = topic)) +
    geom_col() +
    facet_wrap(vars(author), scales = 'free') +
    coord_flip()

tidy(fitted_tmf, 4, matrix = 'beta') |>
    group_by(topic) |>
    slice_max(beta, n = 7) |>
    ungroup() |>
    ggplot(aes(reorder_within(token, desc(beta), topic), beta)) +
    geom_point() +
    geom_linerange(ymin = 0, aes(ymax = beta)) +
    scale_x_reordered() +
    facet_wrap(vars(topic), scales = 'free_y') +
    coord_flip()

## STM ----
dtm2 = dataf |>
    filter(term %in% vocab) |>
    cast_sparse(book, term, n)
tic()
fitted_stm = stm(dtm2, K = 4, data = meta)
# fitted_stm = stm(dtm2, K = 0, init.type = 'Spectral')
toc()

tidy(fitted_stm, matrix = 'gamma') |>
    mutate(book = rownames(dtm2)[document]) |>
    left_join(meta, by = 'book') |>
    ggplot(aes(book, gamma, fill = as.character(topic))) +
    geom_col() +
    facet_wrap(vars(author), scales = 'free') +
    coord_flip()

tidy(fitted_stm, matrix = 'beta') |>
    group_by(topic) |>
    slice_max(beta, n = 7) |>
    ungroup() |>
    ggplot(aes(reorder_within(term, desc(beta), topic), beta)) +
    geom_point() +
    geom_linerange(ymin = 0, aes(ymax = beta)) +
    scale_x_reordered() +
    facet_wrap(vars(topic), scales = 'free_y') +
    coord_flip()

## Hellinger distance and tSNE ----
set.seed(12345)
tsne(fitted_tmf, 4) |>
    left_join(meta, by = c('document' = 'book')) |>
    ggplot(aes(x, y, color = author)) +
    geom_point()

tsne_stm = tidy(fitted_stm, matrix = 'gamma') |>
    hellinger(id1 = document, prob1 = gamma) |>
    as.dist() |>
    Rtsne::Rtsne(perplexity = floor((nbooks - 1)/3) - 1)
tsne_stm$Y |>
    magrittr::set_rownames(rownames(dtm2)) |>
    as_tibble(rownames = 'book') |>
    left_join(meta, by = 'book') |>
    ggplot(aes(V1, V2, color = author)) +
    geom_point()

