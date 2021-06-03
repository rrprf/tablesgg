% Auto-generated documentation for function prHvrules 
%  2021-06-02 11:12:19 
\name{prHvrules}
\alias{prHvrules}
\title{Create Hvrules and Assign Them Graphical Properties }
\description{
Create horizontal and vertical rules (hvrules) for a table, based on
blocks of table entries.  hvrules also provide a way to insert extra space
between rows or columns of the table, without drawing a line.  Assign
graphical properties to the hvrules using a style and a scaling factor.
This creates fully plot-ready (\code{pr}) hvrules.  This is an internal
function, not intended to be called by package users.
}
\usage{
prHvrules(x, style=tablesggOpt("hvruleStyle"), scale=1.0)
}
\arguments{
  \item{x}{A \code{tblBlocks} object specifying blocks of cells in a 2D table.
  }
  \item{style}{A \code{styleObj} object, specifying the graphical properties to assign to
hvrules based on the nature of their associated blocks, in \code{x}.  The
default is taken from \code{tablesggOpt()}.
  }
  \item{scale}{Numeric multiplier used to increase or decrease the displayed size of
hvrules, relative to the natural size implied by \code{style}.  The
default is 1.0.
  }
}
\value{
An object of S3 class \code{prHvrules}, which is a data frame with each
row specifying one hvrule for the table.  If there are no hvrules, the
data frame will have no rows.  Column descriptions are in the
documentation for the \code{elements} function, except for:
  \item{style_row}{Row number in \code{style} used to assign graphical properties to the
hvrule, or NA if none was.
  }
Row names for the data frame are set equal to \code{id}.  The object also
has attributes \code{current_scale} and \code{style}, based on the
\code{scale} and \code{style} arguments after applying defaults.
}
\details{
A horizontal rule (hrule) consists of a narrow, horizontally-oriented
rectangle, possibly with a horizontal line drawn within it.  The rectangle
is inserted _between_ rows of the table, and therefore increases the size
of the table by an amount equal to its height.  Thus an hrule can be used
to insert extra space between table rows, with or without including a
line.  Similarly a vertical rule (vrule) is a narrow, vertically-oriented
rectangle, possibly with a vertical line drawn within it.  The rectangle
is inserted between columns of the table and increases the table's width.

This function creates four hvrules per non-empty block in \code{x}: two
hrules that run along the top and bottom sides of the block, and two
vrules that run along the left and right sides of the block.  It then
applies \code{style} to assign them graphical properties.  See
\code{styleObj} for details of how matching individual hvrules to
graphical properties is done.

The graphical properties available for an hvrule are defined in object
\code{grProps}.  They include the amount of space added to the table by
the hvrule, and properties for any line drawn within it (line type, color,
thickness, etc).

All hvrules are initially disabled; i.e., they will not be displayed and
will not take up any physical space when a table is plotted.  Once an
hvrule has been assigned graphical properties from \code{style}, its
\code{enabled} value will be set to TRUE.  However hvrules associated with
blocks that contain no enabled entries (as reported by
\code{x$had_enabled_entries}) always have \code{enabled} set to FALSE.

Two hvrules may occupy the same location in the table.  For example if the
right edge of block A is adjacent to the left edge of block B, and the
blocks span the same rows, then the vrule on the "right" side of A will
have the same location as the vrule on the "left" side of B.
}
\seealso{
\code{\link{elements}} documents the descriptors and graphical properties
for blocks; \code{\link{tblBlocks}} defines a default set of blocks that
reflect the logical structure of the table.  \code{\link{styleObj}}
creates styles.
}
\keyword{internal}
