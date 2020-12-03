% Auto-generated documentation for function prTable.pltdTable 
%  2020-11-29 11:21:55 
\name{prTable.pltdTable}
\alias{prTable.pltdTable}
\title{Extract/Update a Plot-Ready Table from a Plotted Table }
\description{
Extract the \code{prTable} (plot-ready table) object from a plotted table.
This is an internal function, not intended to be called by package users.
}
\usage{
\method{prTable}{pltdTable}(x, ...)
}
\arguments{
  \item{x}{A \code{pltdTable} object, containing a plotted table.
  }
  \item{...}{Ignored, with a warning.  (Included for compatibility with the generic.)
  }
}
\value{
An object of S3 class \code{prTable}, a plot-ready table.  See
\code{prTable.prEntries} for the structure of this object.
}
\keyword{internal}
