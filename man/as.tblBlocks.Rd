% Auto-generated documentation for function as.tblBlocks 
%  2021-06-02 11:12:18 
\name{as.tblBlocks}
\alias{as.tblBlocks}
\title{Promote a Data Frame to a \code{tblblocks} Object }
\description{
Promote a data frame to a \code{tblBlocks} object.  This is an internal
function not intended to be called by package users.
}
\usage{
as.tblBlocks(x)
}
\arguments{
  \item{x}{A data frame, with each row representing one table block.  See
\code{dfSpecs} for the required columns and types, and \code{tblBlocks}
for a description of these columns.
  }
}
\value{
An S3 object of class \code{tblBlocks}.  It is just the data frame
\code{x} with the additional class \code{tblBlocks} (after checking that
\code{x} contains the necessary columns), and attribute
\code{rowgroupSize}.  The latter is copied from \code{x} if present and
set to NA if not.
}
\details{
Normally a standard set of structure-based blocks is created from a
\code{tblEntries} object via function \code{tblBlocks}.  This function
allows blocks to be created or edited directly in a data frame.

A block is simply a rectangular set of contiguous table cells.  Any number
of blocks may be defined, and blocks may overlap.  A block may be empty
(\code{nr} and/or \code{nc} equal to 0, and corresponding \code{arow*} and
\code{acol*} values set to NA.)  \code{id} must not contain semicolons.

Columns \code{arow1}, \code{arow2}, \code{acol1} and \code{acol2} refer to
row and column numbers in the augmented row-column grid of the table; see
\code{adim} for more information.

In addition to the required columns listed above, \code{x} should contain
any other columns that will be used by a \code{blockStyle} to add
graphical properties to blocks.  These should be listed in the
\code{match_columns} attribute of the style.

If \code{x} is already a \code{tblBlocks} object, this function does some
validity checks.  If \code{x} is NULL, a valid but empty \code{tblBlocks}
object is returned.
}
\seealso{
\code{\link{tblBlocks}}
}
\keyword{internal}
