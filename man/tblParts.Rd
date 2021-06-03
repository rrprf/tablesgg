% Auto-generated documentation for function tblParts 
%  2021-06-02 11:12:19 
\name{tblParts}
\alias{tblParts}
\title{Extract Dimensions of the Various Parts of a Table }
\description{
Extract information about the dimensions of the various parts of a table
represented by a \code{textTable} or \code{tblEntries} object.  This is an
internal function not intended to be called by package users.
}
\usage{
tblParts(x)
}
\arguments{
  \item{x}{A \code{textTable} object, representing a table.  May also be an object
containing table entries derived from a \code{textTable} object; for each
entry, columns \code{part}, \code{arow1}, \code{arow2}, \code{acol1}, and
\code{acol2} are required.  May also be NULL, which will be treated as a
table with no entries.
  }
}
\value{
Numeric matrix with one row per table part, and columns:
  \item{nr, nc}{Number of rows, columns in the part (\code{nc} equal to NA for annotation
parts since the number of columns they span is determined by other table
parts, rather than fixed)).
  }
  \item{arow1, arow2, acol1, acol2}{First and last rows, first and last columns occupied by the part within
the table's augmented row-column grid.  \code{arow*} should be NA if
\code{nr} is 0, \code{acol*} should be NA if \code{nc} is 0.
  }
Row names will be set to the part IDs.
}
\details{
Currently defined table parts are "title", "subtitle", "rowhead",
"colhead", "rowheadLabels", "body", "foot".

The dimensions of a part are based on the logical structure of the table,
as represented in a \code{textTable} object.  When \code{x} is a
\code{tblEntries} or other entry-containing object, the dimensions of
table parts are inferred from the numbers and positions of entries in each
part.  It is an error if the set of entries is not sufficiently complete
that reconstruction of dimensions is unambiguous.

When inferring dimensions from a \code{tblEntries} or other
entry-containing object, the display option \code{rowheadInside} is
implicitly set to FALSE.  Thus table parts can be assumed to be
rectangular, occupying continguous blocks of cells in the table, and
non-overlapping.

Internal consistency of dimensions is enforced: the number of rows for
"colhead" will always be at least as large as for "rowheadLabels"; the
number of columns for "rowheadLabels" and "rowhead" will be equal; the
number of rows in "rowhead" and "body" will be equal; and the number of
columns in "colhead" and "body" will be equal.

This function relies on \code{undo_rowhead_inside}, which uses
\code{adim}.
}
\seealso{
\code{\link{adim}} for discussion of the augmented row-column grid.
}
\keyword{internal}
