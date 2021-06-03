% Auto-generated documentation for function prBlocks 
%  2021-06-02 11:12:19 
\name{prBlocks}
\alias{prBlocks}
\title{Assign Graphical Properties to Table Blocks }
\description{
Assign graphical properties to table blocks (rectangular sets of table
cells), based on a style and scaling factor.  This creates plot-ready
(\code{pr}) blocks.  This is an internal function, not intended to be
called by package users.
}
\usage{
prBlocks(x, style=tablesggOpt("blockStyle"), scale=1.0)
}
\arguments{
  \item{x}{A \code{tblBlocks} object, containing the locations, and possibly other
descriptors, of table blocks.
  }
  \item{style}{A \code{styleObj} object, specifying the graphical properties to assign to
blocks based on their place in the structure of the table or other
descriptors in \code{x}.  The default is taken from \code{tablesggOpt()}.
  }
  \item{scale}{Numeric multiplier used to increase or decrease the displayed size of
certain block properties, relative to the natural size implied by
\code{style}.  The default is 1.0.
  }
}
\value{
An object of S3 classes \code{prBlocks}, \code{tblBlocks}, and
\code{data.frame}.  It has the same rows and columns as \code{x} (one row
per block), plus additional or updated columns containing the graphical
properties to use for each block (shading and borders); logical column
\code{enabled} (indicating whether the block is to be made visible when
the table is rendered, by shading and/or a border); and column
\code{style_row} (the number of the last row in \code{style} that the
block matched, or NA if it matched none).

Row names for the data frame are set equal to block \code{id}.  The object
has several additional attributes: \code{current_scale} and \code{style},
based on the \code{scale} and \code{style} arguments after applying
defaults; and \code{rowgroupSize} and \code{rowheadInside}, copied from
\code{x}.
}
\details{
The graphical properties available for a block, and their types, are
defined in object \code{grProps}.  They include both color and alpha used
to fill the area occupied by the block, and the color and thickness of the
border drawn around the block.  When rendering a table, blocks are drawn
first so that they will not obscure entries or hvrules.

See \code{styleObj} for information about how a style is specified, and
how it is applied to blocks.  Note that individual style specifications
(rows) in \code{style} are applied to \code{x} in order.  If a block
matches more than one \code{style} row, the last matching row will
override the earlier ones.

All blocks are disabled by default; i.e., they will not be displayed when
a table is plotted.  Selected blocks can be enabled by editing a
\code{prBlocks} object like a data frame, or by modifying an existing plot
using the \code{props<-} function.
}
\seealso{
\code{\link{elements}} documents the descriptors and graphical properties
for blocks. \code{\link{tblBlocks}} defines a default set of blocks that
reflect the logical structure of the table.  \code{\link{styleObj}}
creates styles.
}
\keyword{internal}
