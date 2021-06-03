% Auto-generated documentation for function blocks 
%  2021-06-02 11:12:19 
\name{blocks}
\alias{blocks}
\title{Extract the \code{prblocks} Object from a Plot-Ready or Plotted Table }
\description{
Extract the \code{prBlocks} object from a plot-ready or plotted table.
This is an internal function, not intended to be called by package users.
}
\usage{
blocks(x, enabledOnly=TRUE)
}
\arguments{
  \item{x}{A plot-ready (\code{prTable}) or plotted (\code{pltdTable}) table object.
  }
  \item{enabledOnly}{Logical.  If TRUE, only the subset of table blocks that are enabled is
extracted.
  }
}
\value{
A \code{prBlocks} object, containing blocks for a table, or NULL if none
have been defined.
}
\keyword{internal}
