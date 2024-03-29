% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tmfast.R
\name{varimax_irlba}
\alias{varimax_irlba}
\title{Fit a varimax-rotated PCA using irlba}
\usage{
varimax_irlba(
  mx,
  n,
  prcomp_fn = irlba::prcomp_irlba,
  prcomp_opts = NULL,
  varimax_fn = stats::varimax,
  varimax_opts = NULL,
  retx = FALSE
)
}
\arguments{
\item{mx}{Matrix of interest}

\item{n}{Number of principal components / varimax factors to return; can take a vector of values}

\item{prcomp_fn}{Function to use to extract principal components}

\item{prcomp_opts}{List of options to pass to \code{prcomp_fn}}

\item{varimax_fn}{Function to use for varimax rotation}

\item{varimax_opts}{List of options to pass to \code{varimax_fn}}
}
\value{
A list of class \code{varimaxes}, with elements
\itemize{
\item \code{totalvar}: Total variance, from PCA
\item \code{sdev}:  Standard deviations of the extracted principal components
\item \code{rotation}:  Rotation matrix (variable loadings) from PCA
\item \code{varimaxes}: A list of class \code{varimaxes}, containing one fitted varimax model for each value of \code{n}, with further elements
\itemize{
\item \code{loadings}: Varimax-rotated standardized loadings
\item \code{rotmat}:  Varimax rotation matrix
\item \code{scores}:  Varimax-rotated observation scores
}
}
}
\description{
Extract \code{n} principal components from the matrix \code{mx} using \code{irlba}, then rotate the solution using \code{varimax}
}
