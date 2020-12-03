% Auto-generated documentation for function entries 
%  2020-11-29 11:21:55 
\name{entries}
\alias{entries}
\title{Extract the \code{prentries} Object from a Plot-Ready or Plotted Table }
\description{
Extract the \code{prEntries} object from a plot-ready or plotted table.
This is an internal function, not intended to be called by package users.
}
\usage{
entries(x, enabledOnly=TRUE)
}
\arguments{
  \item{x}{A plot-ready (\code{prTable}) or plotted (\code{pltdTable}) table object.
  }
  \item{enabledOnly}{Logical.  If TRUE, only the subset of table entries that are enabled is
extracted.
  }
}
\value{
A \code{prEntries} object, containing plot-ready entries for a table.
}
\keyword{internal}
