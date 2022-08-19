library(tidyverse)
library(tidytext)
library(janeaustenr)
library(gutenbergr)

library(tmfast)
library(stm)

library(tictoc)

mirror = 'https://gutenberg.pglaf.org/'

use_arrow = TRUE
if (use_arrow) {
    library(arrow)
    corpus_dir = file.path('data', 'realbooks')
}

## Build corpus ----
if (use_arrow && file.exists(file.path(corpus_dir,
                                       'author=H.G. Wells',
                                       'book=The War of the Worlds',
                                       'part-0.parquet'))) {
    data_ar = open_dataset(corpus_dir)
    dataf = collect(data_ar)
} else {
    austen_df = austen_books() |>
        unnest_tokens(term, text, token = 'words') |>
        mutate(author = 'Jane Austen') |>
        count(author, book, term)

    bronte = gutenberg_download(c(1260, 768, 969, 9182, 767),
                                meta_fields = c('title'),
                                mirror = mirror)

    bronte_df = bronte |>
        unnest_tokens(term, text, token = 'words') |>
        mutate(author = 'Brontë sisters') |>
        count(author, book = title, term)

    # gutenberg_authors |>
    #     filter(str_detect(author, 'Dickens'))
    # gutenberg_works(gutenberg_author_id == 37) |>
    #     view()
    dickens = gutenberg_download(c(98, 730, 766, 786),
                                 meta_fields = c('title'),
                                 mirror = mirror)

    dickens_df = dickens |>
        unnest_tokens(term, text, token = 'words') |>
        mutate(author = 'Charles Dickens') |>
        count(author, book = title, term)

    hgwells = gutenberg_download(c(35, 36, 5230, 159),
                                 meta_fields = c('title'),
                                 mirror = mirror)

    wells_df = hgwells |>
        unnest_tokens(term, text, token = 'words') |>
        mutate(author = 'H.G. Wells') |>
        count(author, book = title, term)

    dataf = bind_rows(austen_df, bronte_df, dickens_df, wells_df)

    if (use_arrow) {
        write_dataset(dataf,
                      file.path(corpus_dir),
                      partitioning = c('author', 'book'))
    }
}

## The books don't dramatically differ in lengths; at most about 1 magnitude
meta = dataf |>
    group_by(author, book) |>
    summarize(n = sum(n))
arrange(meta, desc(n))

nbooks = dataf |>
    pull(book) |>
    n_distinct()

## Vocabulary selection ----
if (!use_arrow) {
    ## 2.3 sec on my laptop
    tic()
    H_df = ndH(dataf, book, term, n)
    R_df = ndR(dataf, book, term, n) |>
        mutate(in_vocab = rank(desc(ndR)) <= 1000)
    toc()
} else {
    ## 0.38 sec on my laptop
    tic()
    H_df = tmfast:::ndH.ArrowObject(data_ar, book, term, n) |>
        collect()
    R_df = ndR(data_ar, book, term, n) |>
        collect() |>
        mutate(in_vocab = rank(desc(ndR)) <= 1000)
    toc()
}

## Because book lengths don't differ so much, two vocab selection methods don't differ that much
left_join(H_df, R_df, by = 'term') |>
    ggplot(aes(ndH, ndR, color = in_vocab)) +
    geom_point()

ggplot(R_df, aes(log10(n), dR, color = in_vocab)) +
    geom_point()

vocab = R_df |>
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


## Umap ---
library(umap)
dist = tidy(fitted_tmf, 4, matrix = 'gamma') |>
    hellinger(document, topic, gamma)

mapped = umap(dist, input = 'dist')

mapped$layout |>
    as_tibble() |>
    mutate(document = colnames(dist)) |>
    left_join(meta, by = c('document' = 'book')) |>
    ggplot(aes(V1, V2, color = author)) +
    geom_point()

dist_stm = tidy(fitted_stm, matrix = 'gamma') |>
    hellinger(id1 = document, prob1 = gamma)
mapped_stm = umap(dist_stm, input = 'dist')

mapped_stm$layout |>
    as_tibble() |>
    mutate(document = colnames(dist)) |>
    left_join(meta, by = c('document' = 'book')) |>
    ggplot(aes(V1, V2, color = author)) +
    geom_point()
