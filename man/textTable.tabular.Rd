% Auto-generated documentation for function textTable.tabular 
%  2021-06-02 11:12:19 
\name{textTable.tabular}
\alias{textTable.tabular}
\title{Create a \code{texttable} from a \code{tabular} Object }
\description{
Convert a \code{tabular} object, representing a 2D data summary table,
into a \code{textTable} object, which can be plotted.
}
\usage{
\method{textTable}{tabular}(x, title=character(0), subtitle=character(0), 
    foot=character(0), rowheadLabels=TRUE, ...)
}
\arguments{
  \item{x}{An object of class \code{tabular}, representing a 2D data summary table,
as produced by the \code{tables} package.
  }
  \item{title, subtitle, foot}{Optional character vectors providing annotation for the table.  May be
empty (i.e., \code{character(0)}, the default).
  }
  \item{rowheadLabels}{Character vector or logical scalar specifying labels for the row header
columns of the table.  FALSE or \code{character(0)} means no labels, TRUE
will attempt to extract labels from \code{x}.
  }
  \item{...}{Additional arguments passed to \code{format.tabular}.
  }
}
\value{
An object with S3 class \code{textTable}.  See the documentation for the
generic function for a description of the structure of this object.
}
\details{
\code{tabular} objects are produced by the \code{tabular} function in
package \code{tables}.  This function converts them to \code{textTable}
objects to enable plotting.  It can also add table annotation.

Row headers and column headers are derived from the \code{rowLabels} and
\code{colLabels} attributes of \code{x}, respectively.  It appears that
\code{tabular} objects always have "rowLabels", "colLabels" and a body
with non-zero dimensions (although this is not required for
\code{textTable} objects in general).  In addition, runs of duplicated
values in \code{rowLabels} and \code{colLabels} are replaced by NA; the
NAs are changed back to the original values by this function.  The
\code{dropcells} attribute is a character matrix matching the table body.
If not NA, the value in \code{dropcells} is used to replace the cell
content after formatting.

In the returned object, components \code{body}, \code{rowhead}, and
\code{colhead} will each have an attribute \code{type}.  For \code{body}
the attribute is a character matrix containing a string describing the
type of value represented in each cell of the table body; namely, the
first element of the vector returned by function \code{class()} as applied
to each element of \code{x}.  For \code{rowhead} and \code{colhead},
\code{type} is a character vector with one element per header variable
(i.e., per column of \code{rowhead} or row of \code{colhead}).  Since
\code{tabular} objects do not retain the classes of the variables that
define row and column dimensions of a table, \code{type} will be set to
NA.

Components of the returned object will also have an attribute
\code{justification}.  It will be a character matrix or vector of the same
size and shape as the component; a value of NA means that the type of
justification is not specified.  Values for the table body and row and
column headers will be taken from \code{x}.  Values for table annotation
will be NA.
}
\seealso{
\code{tables::tabular}, \code{tables::format.tabular},
\code{\link{plot.textTable}}
}
\examples{
# 'iris2_tab' is a 'tabular' object created using 'tables::tabular'.
class(iris2_tab)
# 'tables' package provides a 'print' method for such objects:
if (requireNamespace("tables", quietly=TRUE)) {
print(iris2_tab)
}
# This package provides 'textTable' and 'plot' methods for such objects:
ttbl <- textTable(iris2_tab)
plot(ttbl)
# ... or just
#plot(iris2_tab)  # same
  }
