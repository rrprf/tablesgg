% Auto-generated documentation for function plot.tabular 
%  2020-11-29 11:21:55 
\name{plot.tabular}
\alias{plot.tabular}
\title{A Plot Method for \code{tabular} Objects }
\description{
A plot method for \code{tabular} objects, which represent a 2D data
summary table.  The table is displayed using \code{ggplot2} graphics.
}
\usage{
\method{plot}{tabular}(x, ...)
}
\arguments{
  \item{x}{An object of class \code{tabular}, representing a 2D data summary table,
as produced by the \code{tables} package by Duncan Murdoch.
  }
  \item{...}{Additional arguments passed to \code{format.tabular} or
\code{plot.textTable}.  See the documentation for those functions.
  }
}
\value{
An object of S3 class \code{pltdTable}, inheriting from \code{ggplot}.
See the documentation for \code{plot.textTable} for more information.
}
\details{
This function is a simple wrapper that first converts \code{x} to a
\code{textTable} object, and then plots that object.
}
\seealso{
\code{\link{plot.textTable}}, \code{format.tabular},
\code{\link{textTable.tabular}}
}
