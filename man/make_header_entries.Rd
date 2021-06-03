% Auto-generated documentation for function make_header_entries 
%  2021-06-02 11:12:19 
\name{make_header_entries}
\alias{make_header_entries}
\title{Convert a Matrix of Headers to Table Entries, with Merging of Runs }
\description{
Given a matrix of row or column headers, create a corresponding set of
table entries, optionally with runs of repeated values merged into single
entries.  This is an internal utility function, not intended to be called
by package users.
}
\usage{
make_header_entries(x, xhier, which_head, offset, mergeRuns)
}
\arguments{
  \item{x}{A character matrix containing header text.  The orientation and dimensions
should match those used for storing the corresponding header in
\code{textTable} objects (and also the way headers are displayed).  Thus
for row headers there is one column per header _layer_ (outermost layer to
innermost from left to right), and the number of rows matches the number
of rows in the table body.  For column headers there is one _row_ per
layer (outermost to innermost from top to bottom), and the number of
columns matches the table body.
  }
  \item{xhier}{List of data frames containing information about the hierarchical
structure of the header.  Function \code{headerRuns} creates such a list.
  }
  \item{which_head}{Character string indicating whether \code{x} contains "row" or "col"umn
headers.
  }
  \item{offset}{Two-element numeric vector.  The first element is the offset to be added
to row numbers of \code{x} to convert them to \code{arow*} values in the
table's expanded row-column grid.  The second element is the offset to be
added to column numbers of \code{x} to convert them to \code{acol*}
values.
  }
  \item{mergeRuns}{Numeric scalar, specifying how deeply in the header hierarchy runs of
repeated values will be merged into single entries.  Thus a value of 0
means no header cells will be merged, a value of 1 means only repeats in
the outermost header layer will be merged, 2 means repeats in the
outermost two layers will be merged, and so on.  (Determination of
repeated values respects the header hierarchy; see \code{headerRuns}.)
  }
}
\value{
A data frame with a row for each header entry, and most of the columns
needed for a \code{tblEntries} object.  It has an additional attribute
\code{i,j} which is a two-column matrix containing the row and column
numbers in \code{x} associated with each entry.  (Minimum i, j values are
used for entries that span multiple rows or columns.)
}
\details{
\code{headlayer} and \code{level_in_layer} in the returned data frame are
based on \code{xhier}, which is not affected by \code{mergeRuns}.  Thus
when \code{mergeRuns} is less than the number of header layers (so some
runs are not merged), there may be multiple entries with the same
\code{headlayer} and \code{level_in_layer} values.  Entry ID's will be
unique however.
}
\seealso{
\code{\link{headerRuns}}; \code{\link{tblEntries}} and
\code{\link{tblBlocks}} (which use this function)
}
\keyword{internal}
