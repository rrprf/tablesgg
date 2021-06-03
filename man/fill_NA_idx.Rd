% Auto-generated documentation for function fill_NA_idx 
%  2021-06-02 11:12:19 
\name{fill_NA_idx}
\alias{fill_NA_idx}
\title{Indices to Impute NA Elements of a Vector by LOCF }
\description{
Indices to impute NA elements of a vector by the
last-observation-carried-forward method.  This is an internal function,
not intended to be called by package users.
}
\usage{
fill_NA_idx(y)
}
\arguments{
  \item{y}{A vector.
  }
}
\value{
A vector of indices with the same length as \code{y}.  If \code{x <-
y[fill_NA_idx(y)]}, then \code{x} will be \code{y} with NA values imputed
by the last-observation-carried-forward method.
}
\keyword{internal}
