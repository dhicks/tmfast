% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generators.R
\name{journal_specific}
\alias{journal_specific}
\title{"Journal-specific" simulation scenario}
\usage{
journal_specific(
  k = 5,
  Mj = 100,
  topic_peak = 0.8,
  topic_scale = 10,
  word_beta = 0.01,
  vocab = 10 * Mj * k,
  size = 3,
  mu = 300,
  bigjournal = FALSE,
  verbose = TRUE
)
}
\arguments{
\item{k}{Number of topics/journals}

\item{Mj}{Number of documents from each journal}

\item{word_beta}{Parameter for the symmetric Dirichlet prior for true word-doc distributions}

\item{vocab}{Size of the vocabulary}

\item{bigjournal}{Should the first journal have documents 10x as long (on average) as the others?}

\item{verbose}{When TRUE, sends messages about the progress of the simulation}

\item{{topic_peak, topic_scale}}{Parameters for the asymmetric Dirichlet prior for true topic-doc distributions}

\item{{size, mu}}{Parameters for the negative binomial distribution of document lengths}
}
\description{
Generates a corpus with \code{Mj} documents from \code{k} journals, each of which has a characteristic topic.  Fits a varimax topic model of rank \code{k}, rotates the word-topic distribution to align with the true values, and reports Hellinger distance comparisons for each topic (word-topic) and document (topic-doc).
}
\seealso{
Other generators: 
\code{\link{draw_corpus}()},
\code{\link{peak_alpha}()},
\code{\link{rdirichlet}()}
}
\concept{generators}
