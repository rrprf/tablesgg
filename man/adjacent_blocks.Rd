% Auto-generated documentation for function adjacent_blocks 
%  2020-11-29 11:21:55 
\name{adjacent_blocks}
\alias{adjacent_blocks}
\title{Identify Adjacent Blocks of Table Cells }
\description{
For each block of table cells in a \code{tblBlocks} object, identify the
other blocks that are adjacent to it on each side.  This is an internal
utility function, not intended to be called by package users.
}
\usage{
adjacent_blocks(x)
}
\arguments{
  \item{x}{A \code{tblBlocks} object, defining a collection of rectangular blocks of
cells in a table.
  }
}
\value{
A list array with one row per block in \code{x}, and four columns.  The
rows are named by the block \code{id}, and the columns as "top", "right",
"bottom", and "left".  Element \code{[[i, j]]} is a character vector
containing the ID's of all blocks that are adjacent to block \code{i} on
side \code{j}.  If there are no adjacent blocks, the element is
\code{character(0)}.
}
\details{
A block B is considered adjacent to block A if (a) the blocks do not
intersect (have no cells in common); and (b) the blocks touch along at
least part of one edge (they span overlapping sets of rows in adjacent
columns, or overlapping sets of columns in adjacent rows).

It requires storage and time proportional to the square of the number of
blocks.  For realistic tables this is not likely to be a problem.
}
\seealso{
\code{\link{tblHvrules}}
}
\keyword{internal}
