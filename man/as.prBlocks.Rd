% Auto-generated documentation for function as.prBlocks 
%  2020-11-29 11:21:55 
\name{as.prBlocks}
\alias{as.prBlocks}
\title{Promote a Data Frame to a \code{prblocks} Object }
\description{
Promote a data frame to a \code{prBlocks} object, containing plot-ready
blocks for a table.  This is an internal function not intended to be
called by package users.
}
\usage{
as.prBlocks(x)
}
\arguments{
  \item{x}{An object inheriting from \code{data.frame}.  Each row represents one
table block (a rectangular set of table cells).  Columns must include all
those required for a \code{tblBlocks} object, plus all the graphical
properties required to display a block (see \code{grProps}).
  }
}
\value{
An object of S3 classes \code{prBlocks}, \code{tblBlocks}, and
\code{data.frame}.  See function \code{prBlocks} for a full description of
the class.
}
\details{
Normally plot-ready blocks are created from a \code{tblBlocks} object via
function \code{prBlocks}.  This function allows them to be created or
edited directly in a data frame, without necessarily using a predefined
style.

Columns \code{arow1}, \code{arow2}, \code{acol1} and \code{acol2} refer to
row and column numbers in the augmented row-column grid of the table; see
\code{adim} for more information.

The graphical properties required for a plot-ready block, and their types,
are defined in object \code{grProps}.

If column \code{enabled} was not present in \code{x} it will be added,
with value FALSE for all blocks.  Attributes \code{current_scale},
\code{rowgroupSize}, and \code{rowheadInside} will be added if not
present, with values 1.0, NA, and NA, respectively.

If \code{x} is already a \code{prBlocks} object, this function does some
validity checks.  If \code{x} is NULL, a valid but empty \code{prBlocks}
object is returned.
}
\seealso{
\code{\link{prBlocks}}, \code{\link{tblBlocks}}
}
\keyword{internal}
