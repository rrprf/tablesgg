% Auto-generated documentation for function tblHvrules 
%  2020-11-29 11:21:55 
\name{tblHvrules}
\alias{tblHvrules}
\title{Create Horizontal and Vertical Rules for a Table }
\description{
Create horizontal and vertical rules (hvrules) for a table, based on
blocks of table entries.  This is an internal utility function, not
intended to be called directly by package users.
}
\usage{
tblHvrules(x)
}
\arguments{
  \item{x}{A \code{tblBlocks} object specifying blocks of cells in a 2D table.  If
NULL, no hvrules are created.
  }
}
\value{
An object of S3 class \code{tblHvrules}, which is a data frame with each
row specifying one hvrule for the table.  If there are no hvrules, the
data frame will have no rows.  Columns are:
  \item{id}{A unique identifier for the hvrule.  This function creates identifiers
using the format \code{<block>_<side>}.
  }
  \item{direction}{Either "hrule" for a horizontal rule or "vrule" for a vertical rule.
  }
  \item{block}{The identifier of the block along the side of which the hvrule runs.
  }
  \item{side}{Which side of the block the hvrule runs along, "top", "bottom", "left", or
"right".
  }
  \item{adjacent_blocks}{Character string containing the IDs of blocks that are adjacent to
\code{block} on the same side as the hvrule.  (I.e., blocks that are
separated from \code{block} by the hvrule.)  Block IDs within the string
are separated by semicolons.  If there are no adjacent blocks the string
will be empty ("").
  }
  \item{arow1, arow2, acol1, acol2}{Location of the hvrule with respect to the expanded row-column grid of the
table.  An hrule is inserted between table rows, and therefore
\code{arow1} and \code{arow2} are the same and equal to a half-integer.
For example, an hrule inserted between rows 3 and 4 has \code{arow1 =
arow2 = 3.5}.  \code{acol1} and \code{acol2} for the hrule are integers
indicating the range of columns that it spans.  Analogously, a vrule is
inserted between table columns, so \code{acol1} and \code{acol2} are
identical and equal to a half-integer, while \code{arow1} and \code{arow2}
are integers that indicate the range of rows spanned by the vrule.
  }
  \item{enabled}{Logical indicating whether the hvrule is to be displayed.  This function
sets \code{enabled} to FALSE for all hvrules.  See DETAILS for information
about enabling selected hvrules.
  }
Row names for the data frame are set equal to \code{id}.
}
\details{
This function creates four hvrules per block: two hrules that run along
the top and bottom sides of the block, and two vrules that run along the
left and right sides of the block.  All of the generated hvrules are
disabled by default.

No graphical properties of the hvrules, such as the size of the inserted
rectangle or the appearance of the line within it, are assigned by this
function.  hvrules can be selectively enabled and given graphical
properties by applying a style (see \code{styleObj}) to create plot-ready
hvrules (a \code{prHvrule} object).  Properties in an existing plot can
also be modified using the \code{props<-} function.
}
\seealso{
\code{\link{tblBlocks}} defines a default set of blocks that reflect the
logical structure of the table.  This function is called by internal
function \code{\link{apply_style}}.
}
\keyword{internal}
