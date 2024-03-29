% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/space.R
\name{umap.tmfast}
\alias{umap.tmfast}
\title{Discursive space with UMAP for tmfast topic models}
\usage{
\method{umap}{tmfast}(model, k, ...)
}
\arguments{
\item{model}{\code{tmfast} object}

\item{k}{Number of topics}

\item{...}{Other arguments, passed to \code{umap.matrix()}}
}
\value{
\code{umap} object or tidied dataframe; see \code{umap.matrix()} argument \code{df}
}
\description{
Construct a 2-dimensional "discursive space" embedding given a \code{tmfast} topic model
}
\examples{
umap(fitted, 10, verbose = TRUE)
}
