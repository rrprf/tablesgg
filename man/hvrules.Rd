% Auto-generated documentation for function hvrules 
%  2020-11-29 11:21:55 
\name{hvrules}
\alias{hvrules}
\title{Extract the \code{prhvrules} Object from a Plot-Ready or Plotted Table }
\description{
Extract the \code{prHvrules} object from a plot-ready or plotted table.
This is an internal function, not intended to be called by package users.
}
\usage{
hvrules(x, enabledOnly=TRUE)
}
\arguments{
  \item{x}{A plot-ready (\code{prTable}) or plotted (\code{pltdTable}) table object.
  }
  \item{enabledOnly}{Logical.  If TRUE, only the subset of table hvrules that are enabled is
extracted.
  }
}
\value{
A \code{prHvrules} object, containing plot-ready hvrules for a table, or
NULL if none have been defined.
}
\keyword{internal}
