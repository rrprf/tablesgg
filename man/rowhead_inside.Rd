% Auto-generated documentation for function rowhead_inside 
%  2021-06-02 11:12:19 
\name{rowhead_inside}
\alias{rowhead_inside}
\title{Move the Outermost Layer of Row Headers Inside a Table }
\description{
Move the outermost layer of row headers \code{inside} a table, by dividing
table rows into groups, with one group per level of the header.  A table
row is inserted above each group, containing the value of the header for
that level as a label.  This is an internal function, not intended to be
called by package users.
}
\usage{
rowhead_inside(x, paste_rhiLabel)
}
\arguments{
  \item{x}{A \code{tblEntries} object containing table entries.
  }
  \item{paste_rhiLabel}{Logical scalar.  If TRUE the rowheadLabel for the outermost row header
layer is prefixed to its levels when creating the interior labels.
  }
}
\value{
If \code{x} has at least one layer of row headers, an updated
\code{tblEntries} object, and with attribute \code{rowheadInside} set to
TRUE.

If \code{x} has no row headers, or it already has row headers moved
inside, it is returned unchanged.
}
\details{
This function changes the expanded row-column grid of the table, so must
be called early in processing, before table blocks or hvrules are created.

The first column of row headers (containing the outermost row header
layer) is removed.  The entries for that layer are moved to new rows
inserted into the table, so instead of appearing at the left side of the
table, they appear as labels (spanning all columns of the table) above
groups of rows.  The \code{id}, \code{part}, \code{subpart},
\code{level_in_layer}, \code{text}, \code{type}, and \code{enabled} values
for those entries do not change.  However \code{headlayer} is now set to 0
(since the header is now interleaved with the body of the table),
\code{multirow} to FALSE, and \code{multicolumn} to TRUE (if the table has
more than one column).

The \code{textspec} values of the moved entries may change if
\code{paste_rhiLabel} is TRUE; an error will be raised if pasting of
markdown and non-markdown text is attempted.

The \code{rowheadLabels} entry (if any) for the moved row header layer
also has its \code{headlayer} set to 0, and is disabled.

Graphical properties for displaying the moved header entries can be set in
the usual ways.  For example, an \code{entryStyle} or \code{props}
specification for entries with \code{part == "rowhead"} and
\code{headlayer == 0} will apply just to row headers that have been moved
inside the table.
}
\seealso{
\code{\link{tblEntries}}, \code{\link{undo_rowhead_inside}}
}
\keyword{internal}
