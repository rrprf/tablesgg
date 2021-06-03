% Auto-generated documentation for function plot.prEntries 
%  2021-06-02 11:12:19 
\name{plot.prEntries}
\alias{plot.prEntries}
\title{A Plot Method for \code{prentries} Objects }
\description{
A plot method for \code{prEntries} objects, displaying tables using
\code{ggplot2} graphics.  This is an internal function, not intended to be
called by package users.
}
\usage{
\method{plot}{prEntries}(x, hvruleStyle=tablesggOpt("hvruleStyle"), 
    blockStyle=tablesggOpt("blockStyle"), scale=attr(x, "current_scale"), 
    rowgroupSize=0, ...)
}
\arguments{
  \item{x}{A \code{prEntries} object, containing plot-ready entries for a table.
  }
  \item{hvruleStyle, blockStyle, scale, rowgroupSize, plot.margin, sizeAdjust}{Arguments used to control the appearance of the plot.  See the
documentation for \code{plot.textTable} for more details about these
arguments.
  }
  \item{...}{Additional arguments, passed to \code{plot.prTable}.
  }
}
\value{
An object of S3 class \code{pltdTable}, inheriting from \code{ggplot}.
See \code{plot.textTable} for details about this object.  There is a print
(display) method for this class, or the object can be modified in the
usual \code{ggplot} way before display.
}
\details{
This function assumes that the standard set of blocks and hvrules is to be
used when plotting the table entries in \code{x}.  For more control over
blocks and hvrules, create them directly (for example by functions
\code{tblBlocks} and \code{prHvrules}) and then combine them with \code{x}
into a plot-ready table (function \code{prTable}).
}
\seealso{
\code{\link{plot.textTable}}, \code{\link{print.pltdTable}}
}
\keyword{internal}
