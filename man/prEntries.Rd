% Auto-generated documentation for function prEntries 
%  2020-11-29 11:21:55 
\name{prEntries}
\alias{prEntries}
\title{Assign Graphical Properties to Table Entries }
\description{
Assign graphical properties to table entries, based on a style and scaling
factor.  This creates fully plot-ready (\code{pr}) entries.  This is an
internal function, not intended to be called by package users.
}
\usage{
prEntries(x, style=tablesggOpt("entryStyle"), scale=1.0)
}
\arguments{
  \item{x}{A \code{tblEntries} object, containing the contents, locations, and
possibly other descriptors, of table entries.
  }
  \item{style}{A \code{styleObj} object, specifying the graphical properties to assign to
entries based on their place in the structure of the table or other
descriptors in \code{x}.  The default is taken from \code{tablesggOpt()}.
  }
  \item{scale}{Numeric multiplier used to increase or decrease the displayed size of
entries, relative to the natural size implied by \code{style}.  The
default is 1.0.
  }
}
\value{
An object of S3 classes \code{prEntries}, \code{tblEntries}, and
\code{data.frame}.  It has the same rows and columns as \code{x} (one row
per entry), plus additional or updated columns containing the graphical
properties to use for each entry, plus a column \code{style_row} (the
number of the last row in \code{style} that the entry matched, or NA if it
matched none).

Row names for the data frame are set equal to block \code{id}.  The object
has several additional attributes: \code{current_scale} and \code{style},
based on the \code{scale} and \code{style} arguments after applying
defaults; and \code{mergeRuns}, \code{rowheadInside}, \code{rowhier}, and
\code{colhier}, copied from \code{x}.
}
\details{
The graphical properties available for an entry, and their types, are
defined in object \code{grProps}.  They include both properties for the
entry text itself (size, font, color, etc) and for the cell(s) containing
the entry (background color, border thickness and color).

Justification of text within a cell is specified by properties
\code{hjust} and \code{vjust}.  Their interpretation is with respect to
the boundaries of the cell: 0 means justification toward the left/top of
the cell, 1 means toward the right/bottom, and 0.5 means centered.

See \code{styleObj} for information about how a style is specified, and
how it is applied to entries.  Note that individual style specifications
(rows) in \code{style} are applied to \code{x} in order.  If an entry
matches more than one \code{style} row, the last matching row will
override the earlier ones.

One can also assign or modify graphical properties directly, without using
a style, by editing a \code{prEntries} object like a data frame.  See
\code{as.prEntries}.  Properties in an existing plot can also be modified
using the \code{props<-} function.
}
\seealso{
\code{\link{elements}} documents the descriptors and graphical properties
for blocks. \code{\link{tblEntries}}; \code{\link{styleObj}} creates
styles; \code{\link{as.prEntries}}.
}
\keyword{internal}
