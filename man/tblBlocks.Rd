% Auto-generated documentation for function tblBlocks 
%  2020-11-29 11:21:55 
\name{tblBlocks}
\alias{tblBlocks}
\title{Define Blocks of Cells that Reflect the Logical Structure of a Table }
\description{
Define a collection of rectangular blocks of cells that reflect the
logical structure of a table.  This is an internal function, not intended
to be called by package users.
}
\usage{
tblBlocks(x, rowgroupSize=0, ...)
}
\arguments{
  \item{x}{A \code{tblEntries} object, or something that can be converted to one
(e.g., a \code{textTable} object).
  }
  \item{rowgroupSize}{Numeric scalar.  If greater than 0, blocks will be created by grouping
consecutive rows of the table body into sets of this size.  See DETAILS.
  }
  \item{...}{Additional arguments passed to \code{tblEntries} when \code{x} is not
already a \code{tblEntries} object (e.g., \code{rowheadInside}).
  }
}
\value{
An S3 object of class \code{tblBlocks}.  This is a data frame with one row
per block.  It contains information about the nature and position of each
block.  Column descriptions are in the documentation for the
\code{elements} function.  Also see DETAILS for more information about
columns \code{id}, \code{type}, and \code{subtype}.

Row names for the data frame are set equal to \code{id}.
\code{rowgroupSize} and \code{rowheadInside} (the latter taken from
\code{x}, after converting it to a \code{tblEntries} object if necessary)
will be included as attributes.
}
\details{
A block is simply a rectangular set of contiguous table cells.  Any number
of blocks may be defined for a given table, and blocks may overlap.  A
block may be empty (\code{nr} and/or \code{nc} equal to 0, and
corresponding \code{arow*} and \code{acol*} values set to NA.)

This function defines a standard set of blocks based on the logical
structure of a table (but independent of the _content_ of table cells).
The following blocks are defined:

"table" : The whole table (all cells). "title", "subtitle", "colhead",
"rowhead", "rowheadLabels", "body", "foot" : The standard table parts.
(If there are interior row header entries, \code{rowhead} and \code{body}
are omitted because the interleaving of headers and body means neither are
valid blocks.) "titles" : The union of the \code{title} and
\code{subtitle} parts. "stub" : If we exclude the title/subtitle and foot
annotations, a table has four quadrants: the body at the lower right, the
row headers at the lower left, the column headers at the upper right, and
a stub at the upper left.  That is, the stub consists of the cells above
the row headers and to the left of the column headers.  (If there are row
header labels--block \code{rowheadLabels}--they will be in the bottom row
of the stub.) "colhead_and_stub", "rowhead_and_stub" : The unions of
\code{stub} with \code{colhead} and \code{rowhead}, respectively.
"colhead_and_body", "rowhead_and_body" : The unions of \code{body} with
\code{colhead} and \code{rowhead}, respectively.

Each of these blocks is unique and so their \code{id} and \code{type} are
the same string.  \code{subtype}, \code{headlayer}, and
\code{level_in_layer} are set to NA for them.

Blocks representing the hierarchical structure of headers

Additional blocks, with types "rowblock" and "colblock", are defined to
represent the hierarchical structure of row and column headers.  To
describe these blocks, some terminology is needed.  For concreteness, the
description is in terms of column headers; analogous comments apply to row
headers.

When a table is displayed, each _row_ of column headers (corresponding to
a row of the \code{colhead} matrix in a \code{textTable} object), defines
one _layer_ of the header.  Layers are numbered from innermost (closest to
the table body) to outermost.  Structurally, layers form a hierarchy:
header values at a lower numbered (inner) layer are nested within values
at higher numbered (outer) layers.  This hierarchy implies a
tree-structured partitioning of table columns according to values of the
header variables.  A set of contiguous columns that share the same header
value for a layer, and for all layers above it in the hierarchy, belong to
a single _level_ of that layer.  Levels are numbered from 1 to the number
of levels in a layer.

Then for each combination of layer number \code{i} and level number
\code{j} in a header, three blocks are defined.  A block with subtype "A"
consists of just the cells in header layer \code{i} whose value
corresponds to level number \code{j}.  It will thus come from a single row
in the column headers of the table.  And since all the cells in the block
have the same value, those cells will typically be merged into a single
entry when displaying the table.

A block with subtype "B" is bigger: it consists of the cells in the
subtype "A" block, plus the header cells with _smaller_ layer numbers in
the same columns.  So it extends from layer \code{i} down through the rest
of the header rows.  And finally a block with subtype "C" is bigger yet:
it consists of the cells in the subtype "B" block plus the cells in the
table body in the same columns.  That is, it spans the same set of columns
as the subtype "A" and "B" blocks but adds rows down through the table
body.  Block ID's have the form \code{colblock/<subtype>/i/j}.

Sets of blocks for the row headers are defined analogously.  Each _column_
in the row headers of a displayed table (corresponding to a column in the
\code{rowhead} matrix of a \code{textTable} object) represents one layer.
The layer closest to the table body is numbered 1 and layer number
increases toward the left edge of the table.  A subtype "A" block consists
of the cells in layer number \code{i} whose value corresponds to level
number \code{j} in that layer.  It will thus come from a single column in
the row headers of the table.  A subtype "B" block consists of the cells
in the "A" block, plus the header cells with _smaller_ layer numbers in
the same rows.  A subtype "C" block further adds the cells in the table
body in the same rows.  That is, it spans the same set of rows as "A" and
"B" blocks but adds columns across through the table body.  Block ID's
have the form \code{rowblock/<subtype>/i/j}.

Blocks when \code{rowheadInside} is TRUE

Moving the outermost layer of row header entries into the interior of the
table, where they separate and label groups of rows, changes the shape of
table parts.  Specifically, since row headers and body are interleaved,
neither forms a valid rectangular block.  However their union is a valid
block, with ID and type \code{rowhead_and_body}.

When a row header layer is moved inside, its layer number is set to 0.
(Conceptually, since it is interleaved with the table body, it is interior
even to header layer 1.)  Blocks \code{rowblock/<subtype>/i/j}, where
\code{i} indicates layer number, are different when i=0 than for other
layers.  Subtype "A", \code{rowblock/A/0/j}, has one row and spans all
table columns.  It contains the label for the \code{j}-th level.  Subtype
"B", \code{rowblock/B/0/j}, contains all row header entries (if any)
nested within level \code{j}.  Subtype "C", \code{rowblock/C/0/j},
combines \code{rowblock/B/0/j} with all the body rows associated with
level \code{j}.  Thus \code{rowblock/C/0/j} spans all table columns.
Unlike when \code{i} is not equal to 0, neither \code{rowblock/B/0/j} nor
\code{rowblock/C/0/j} contain \code{rowblock/A/0/j}.

Blocks representing groups of rows

When a table has many rows within a given level of the row header
hierarchy, the table may be easier to read if rows are grouped into
smaller sets of fixed size (groups of 5, for example), with some extra
space inserted between groups.  To facilitate this, this function creates
blocks representing such groups when \code{rowgroupSize} is positive.  The
block type is "rowblock" and subtype is "G".

Grouping respects the row header hierarchy: the innermost header layer
that has runs of repeated values is identified (layer \code{i} say), and
grouping is done separately within each of its levels.  The block
representing a row group spans all columns of the table body as well as
row header layers out to layer \code{i-1}.  Block ID's have the form
\code{rowblock/G/i/j/k}, where \code{j} is the level number (within layer
\code{i}) that contains the group, and \code{k} is the group number within
that level.  Thus \code{i}, \code{j}, and \code{k} are the values of
\code{headlayer}, \code{level_in_layer}, and \code{group_in_level} for the
block.

However if the table has no row headers, or none of the row header layers
have runs of repeated values, table rows are simply grouped into sets of
size \code{rowgroupSize}.  \code{headlayer} and \code{level_in_layer} will
be NA for the group blocks, and block ID's will have the form
\code{rowblock/G///k}, where \code{k} is the group number (and value of
\code{group_in_level}).
}
\seealso{
\code{\link{tblEntries}}, \code{\link{tblParts}}, \code{\link{elements}};
\code{\link{as.tblBlocks}} converts a data frame to a \code{tblBlocks}
object.
}
\keyword{internal}
