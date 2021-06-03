% Auto-generated documentation for function plot.tblEntries 
%  2021-06-02 11:12:19 
\name{plot.tblEntries}
\alias{plot.tblEntries}
\title{A Plot Method for \code{tblentries} Objects }
\description{
A plot method for \code{tblEntries} objects, displaying tables using
\code{ggplot2} graphics.  This is an internal function, not intended to be
called by package users.
}
\usage{
\method{plot}{tblEntries}(x, entryStyle=tablesggOpt("entryStyle"), ...)
}
\arguments{
  \item{x}{A \code{tblEntries} object, containing entries for a table.
  }
  \item{entryStyle}{Optional \code{entryStyle} object, describing the graphical properties
(color, size, font, etc) to be used for displaying entries in a table.
See the function of the same name for details.  If omitted, a default
style will be used.
  }
  \item{...}{Additional arguments passed to \code{plot.prEntries} and
\code{plot.prTable}.
  }
}
\value{
An object of S3 class \code{pltdTable}, inheriting from \code{ggplot}.
See \code{plot.textTable} for details about this object.  There is a print
(display) method for this class, or the object can be modified in the
usual \code{ggplot} way before display.
}
\details{
This function is a wrapper that simply converts \code{x} to "plot-ready"
table entries (a \code{prEntries} object) by applying the style specified
by \code{entryStyle}, and then calls the plot method for \code{prEntries}
objects.

This function assumes that the standard set of blocks and hvrules is to be
used when plotting the table entries in \code{x}.  For more control over
blocks and hvrules, create them directly (for example by functions
\code{tblBlocks} and \code{tblHvrules}) and then combine them with
\code{x} into a plot-ready table (functions \code{prEntries},
\code{prHvrules}, \code{prTable}).
}
\seealso{
\code{\link{plot.textTable}}, \code{\link{print.pltdTable}}
}
\keyword{internal}
