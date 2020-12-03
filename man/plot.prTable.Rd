% Auto-generated documentation for function plot.prTable 
%  2020-11-29 11:21:55 
\name{plot.prTable}
\alias{plot.prTable}
\title{A Plot Method for \code{prtable} Objects }
\description{
A plot method for \code{prTable} objects, to display tables using
\code{ggplot2} graphics.  This is an internal function, not intended to be
called by package users.
}
\usage{
\method{plot}{prTable}(x, plot.margin=tablesggOpt("plot.margin"), sizeAdjust=c(1.0, 
    1.0), ...)
}
\arguments{
  \item{x}{A \code{prTable} object, containing a plot-ready table.
  }
  \item{plot.margin}{Numeric vector of length 4, giving the amount of padding to be added
outside the top, right, bottom, and left sides of the table, in
millimeters.
  }
  \item{sizeAdjust}{Two-element numeric vector containing multipliers to adjust the calculated
absolute size of table entries.  The first element is applied to the width
and the second to the height.  See DETAILS.
  }
  \item{...}{Other arguments, ignored with a warning.  (Included for compatibility with
the \code{plot} generic.)
  }
}
\value{
An object of S3 class \code{pltdTable}, inheriting from \code{ggplot}.
(However if \code{x} has no enabled entries to plot, the returned value is
NULL, with a warning.)  There is a print (display) method for this class,
or the object can be modified in the usual \code{ggplot} way before
display.

The object has attributes \code{plot.margin} and \code{sizeAdjust} (equal
to the arguments), and \code{size_mm} (the width and height of the plot,
in millimeters).  \code{size_mm} is calculated after applying
\code{sizeAdjust}, and includes both the table and any margins specified
by \code{plot.margin}.

The object also has attributes \code{colBoundaries} and
\code{rowBoundaries} giving the coordinates of the boundaries between
columns and between rows, again in mm, and after applying \code{scale} and
\code{sizeAdjust}.

The object also has attribute \code{prTable}, the plot-ready table object
after applying \code{scale}.  This is to allow convenient updating of the
display properties of the table.
}
\details{
The implicit coordinate system for the plot is in millimeters, with the
origin at the upper left corner of the _table_ (which may differ from the
upper left corner of the plot due to \code{plot.margin}), and
y-coordinates increasing downward.  All sizes and dimensions are in
millimeters except font size, which is in points.

The print method for \code{pltdTable} objects attempts to draw the table
at its natural size, as given by attribute \code{size_mm}.  (Function
\code{pltdSize} provides convenient access to this attribute, in
user-controllable units.) However the calculated size depends somewhat on
the graphics device being used (see \code{?Devices}).  \code{sizeAdjust}
may be used to compensate for this difference when a plotted table object
created using one device is to be displayed with a different one.

This function checks the \code{enabled} field of the \code{entries},
\code{blocks}, and \code{hvrules} components of \code{x}.  Elements for
which \code{enabled} is FALSE are not displayed, and their size is treated
as 0 when laying out the table.
}
\seealso{
\code{\link{prTable}}, \code{\link{pltdSize}},
\code{\link{print.pltdTable}}, \code{\link{Devices}}
}
\keyword{internal}
