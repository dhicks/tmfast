## Comparison with STM ----
## STM has a better fit to the Zipfian distribution, slightly closer Hellinger distance for word-topic distributions around .07
library(stm)

## stm needs counts, not log-normalized
stm_fit = corpus |> 
    tidytext::cast_sparse(doc, word, n) |> 
    stm(K = k)

beta = tidytext::tidy(stm_fit, matrix = 'beta') |> 
    mutate(term = as.integer(term)) |> 
    complete(topic = 1:k, term = 1:vocab, fill = list(beta = 0))

## Compare Zipfian distributions
bind_rows({beta |> 
        rename(dim = 'topic', value = 'beta') |> 
        mutate(type = 'fitted', 
               dim = str_c('topic ', dim))},
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
hellinger_(phi, 
           {beta |> 
                   pivot_wider(names_from = 'term', values_from = 'beta') |> 
                   select(-topic) |> 
                   as.matrix()})
