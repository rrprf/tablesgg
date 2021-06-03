% Auto-generated documentation for function ids 
%  2021-06-02 11:12:18 
\name{ids}
\alias{ids}
\title{Get the Identifier Strings for Parts or Elements of a Table }
\description{
Get the unique identifier strings for elements of a plotted table, or for
parts of a \code{textTable}.
}
\usage{
ids(x, type, enabledOnly=TRUE)
}
\arguments{
  \item{x}{A \code{textTable} or a plotted table (\code{pltdTable}) object.
  }
  \item{type}{Character scalar indicating the type of elements to get ID's for.  May be
"entry", "block", or "hvrule" for a plotted table, or "part" for a
\code{textTable}.  Optional for a \code{textTable}.
  }
  \item{enabledOnly}{Logical scalar.  If TRUE, ID's are returned only for elements that are
currently enabled in \code{x}.  (Ignored for a \code{textTable}.)
  }
}
\value{
A character vector of identifiers.
}
\details{
A plotted table (\code{pltdTable} object) has three types of elements:
entries, blocks, and hvrules.  Entries are the text strings (and
associated properties) displayed in table cells.  Blocks are rectangular
sets of contiguous table cells.  And hvrules are spacers, with or without
a visible line (or "rule"), used to separate or group table rows and
columns.

Each element has an ID string, unique within an element type, and this
function returns a vector of those strings.

A \code{textTable} has parts ("title", "subtitle", "rowhead", etc.), and
this function just returns the vector of the part ID's.
}
\seealso{
\code{\link{elements}}, which returns a data frame with full information
about each element of a plotted table.
}
\examples{
  ttbl <- textTable(iris2_tab)
  # Just the names of the standard table parts:
  ids(ttbl)
  
  ptbl <- plot(ttbl)
  # ID's of all the blocks defined for the table:
  ids(ptbl, type="block", enabledOnly=FALSE)
  # ID's of the blocks that are enabled for display (by default, none):
  ids(ptbl, type="block", enabledOnly=TRUE)
  }
