% Auto-generated documentation for function acol 
%  2020-11-29 11:21:55 
\name{acol}
\alias{acol}
\title{Column Numbers Within the Augmented Row-Column Grid for a Table }
\description{
Return the column numbers associated with a specified table part or
element, or with a set of column header values, within the augmented
row-column grid of a table.
}
\usage{
acol(x, id=NULL, hpath=NULL)
}
\arguments{
  \item{x}{A \code{textTable} or a plotted table (\code{pltdTable}) object.
  }
  \item{id}{Character scalar containing the ID of a single table part, block, entry,
or hvrule.  (If \code{x} is a \code{textTable}, only the ID of a table
part is allowed.)
  }
  \item{hpath}{Character vector with length between 0 and the number of layers in the
column header.  The \code{i}-th element should be one of the values in the
\code{i}-th header row, or NA.  See DETAILS.  Only one of \code{id} and
\code{hpath} should be specified.
  }
}
\value{
A numeric vector containing column numbers within the table's augmented
row-column grid.  The column numbers are those partially or completely
occupied by the cells associated with \code{id} or \code{hpath}.  They
will be increasing but not necessarily consecutive.

The returned vector may have length 0 if \code{id} refers to a table part
or block that spans no columns, or if \code{hpath} matches no set of
column header values.

Note that for a vertical hvrule (vrule), the "column number" is actually a
half-integer, bracketed by the table column numbers between which the
vrule runs.  For example, if the vrule runs between table columns 3 and 4,
the returned value will be \code{c(3.5)}.
}
\details{
See the documentation for \code{adim} for more information about the
augmented row-column grid of a table.

Only one of arguments \code{id} and \code{hpath} should be specified.
\code{id} is searched for first among table parts (the only thing
available for a \code{textTable}), then blocks, entries, and hvrules, in
that order.  The search stops at the first match.  It is an error if
\code{id} is not found in any of these.

\code{hpath} is short for "header path".  It is used to obtain column
numbers associated with specified combinations of values of the column
header variables.  Suppose there are \code{L} layers of column headers.
If the length of \code{hpath} is less than \code{L}, NA values are added
at the end to reach that length.  The function returns the intersection of
the column numbers for which the \code{i}-th outermost of the header
layers equals the \code{i}-th element of \code{hpath}.  An NA in
\code{hpath} is taken to match all values in the correponding layer of
column headers.  Thus, if \code{L == 4} and \code{hpath=c("a", NA, "c")},
the function will return the column numbers for which the outermost column
header has a value of "a" _and_ the third outermost has a value of "c".
If no column has the combination of values specified by \code{hpath} then
the returned vector will have length 0.

Since \code{hpath} refers to values of column header variables, it cannot
be used to get column numbers associated with table annotation, or with
the row header.  (Use \code{id} instead.)
}
\seealso{
\code{\link{adim}} to get the dimensions of the augmented row-column grid;
\code{\link{arow}} for the corresponding operation on rows;
\code{\link{ids}}
}
\examples{
ttbl <- textTable(iris2_tab, title=c("Title 1", "2nd title"), foot="Foot")
plt <- plot(ttbl)

acol(plt, id="title")  # block "title" spans all columns
acol(plt, id="body,4,2")  # single entry
# Remove the columns for "Petal" measurements (a value in column 
# header layer 2):
plot(ttbl[, -acol(ttbl, hpath=c(NA, "Petal"))])
# Remove the "Length" measurements (a value in column header layer 3):
plot(ttbl[, -acol(ttbl, hpath=c(NA, NA, "Length"))])
# Remove the "Length" measurements just for "Petal":
plot(ttbl[, -acol(ttbl, hpath=c(NA, "Petal", "Length"))])
  }
