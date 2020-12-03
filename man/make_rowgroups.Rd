% Auto-generated documentation for function make_rowgroups 
%  2020-11-29 11:21:55 
\name{make_rowgroups}
\alias{make_rowgroups}
\title{Group Rows of a Table into Sets of Fixed Size }
\description{
Create blocks representing groups of consecutive rows of a table, of
specified size.  (This is an internal utility function, not intended to be
called directly by package users.)
}
\usage{
make_rowgroups(stdblks, rowheadruns, rowgroupSize)
}
\arguments{
  \item{stdblks}{A data frame containing the standard set of table blocks created by
\code{tblBlocks} (without row groups).  Specifically, the row header
hierarchy blocks \code{rowblock/A/i/j} and \code{rowblock/C/i/j} are
required, along with \code{rowhead_and_body}.
  }
  \item{rowheadruns}{A list with one data frame per layer of row headers (from outer to inner),
containing information about the hierarchical structure of the row
headers.
  }
  \item{rowgroupSize}{Numeric scalar.  If not 0 or NA, blocks will be created by grouping
consecutive rows of the table body into sets of this size.
  }
}
\value{
An S3 object of class \code{tblBlocks}, containing (just) the newly
created row group blocks.  These have type "rowblock" and subtype "G".

If \code{rowgroupSize} is not a finite positive value, the returned object
will have no blocks (no rows).
}
\details{
This is an internal support function for \code{tblBlocks} and
\code{tblHvrules}.  The code relies on the standard naming of table blocks
in \code{tblBlocks}.

When a table has many rows within a given level of the row header
hierarchy, the table may be easier to read if rows are grouped into
smaller sets of fixed size (groups of 5, for example), with some extra
space inserted between groups.  To facilitate this, this function creates
blocks representing such groups.  The block type is "rowblock" and subtype
is "G".

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
\code{\link{tblBlocks}}; \code{\link{headerRuns}} creates the list
required by argument \code{rowheadruns}.
}
\keyword{internal}
