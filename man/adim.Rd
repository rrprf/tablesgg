% Auto-generated documentation for function adim 
%  2021-06-02 11:12:18 
\name{adim}
\alias{adim}
\title{Dimensions of the Augmented Row-Column Grid for a Table }
\description{
Return the dimensions of the augmented row-column grid for a table.  Along
with rows and columns associated with the body of the table, the augmented
grid includes rows for each title, subtitle, and foot line, and rows and
columns associated with the column and row headers.
}
\usage{
adim(x)
}
\arguments{
  \item{x}{A table, in any of the forms used within the \code{tablesgg} package.
This includes \code{textTable} and \code{pltdTable} objects.
  }
}
\value{
A two-element numeric vector containing (number of rows, number of
columns).
}
\details{
It is common to think of the number of rows and columns in a table as
referring to the _body_ of the table.  However in this package the grid of
rows and columns in the body of a table is expanded into an _augmented
row-column grid_, by adding a row for each title, subtitle, and foot line;
a row for each layer of column headers; a row for each _interior_ row
header entry; and a column for each (non-interior) layer of row headers.
The augmented grid is numbered from the upper left, so column numbers
increase from left to right, and row numbers from top to bottom.  Title,
subtitle, and foot lines, and interior row header entries span all the
columns of the grid.

This function returns the dimensions of the augmented grid.

Disabled entries are included in counting rows and columns.

The function \code{summary} gives the dimensions of individual parts
within a table.
}
\seealso{
\code{\link{summary.textTable}}, \code{\link{summary.pltdTable}}
}
\examples{
ttbl <- textTable(iris2_tab, title="Summary statistics for the iris data")
adim(ttbl)
plt <- plot(ttbl)
adim(plt)
  }
