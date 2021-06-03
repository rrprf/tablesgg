% Auto-generated documentation for function [.textTable 
%  2021-06-02 11:12:19 
\name{[.textTable}
\alias{[.textTable}
\title{Extract a Subset of a \code{texttable} Object }
\description{
Extract a subset of a \code{textTable} object, creating a new table with
fewer and/or rearranged rows and columns.
}
\usage{
\method{[}{textTable}(x, i, j, drop=FALSE)
}
\arguments{
  \item{x}{An object of S3 class \code{textTable}, representing a 2D table.
  }
  \item{i, j}{Logical or numeric indexing arguments used as subscripts with respect to
the augmented row-column grid of the table.  See DETAILS.
  }
  \item{drop}{Ignored (always treated as FALSE).
  }
}
\value{
An object of S3 class \code{textTable}.
}
\details{
This function extracts, deletes, or rearranges subsets of the rows and
columns of a table.  It is similar to subsetting an ordinary matrix, but
with restrictions required to ensure that the resulting object is still a
valid \code{textTable}:

1.  Indexing is with respect to the augmented row-column grid of the
table, in which all parts of the table (body, headers, and annotation) are
included.  See \code{?textTable} for a description of table parts, and
\code{?adim} for a description of the augmented grid.  The \code{summary}
method for a \code{textTable} shows the dimensions of each part.

2.  The first index argument, \code{i}, cannot itself be a matrix.

3.  Indexing cannot be used to move rows or columns between different
parts of the table (e.g. between body and headers, or between headers and
annotation).

Helper functions \code{arow} and \code{acol} can be used to get the
augemented row and column numbers spanned by different table parts.  See
the examples.
}
\seealso{
\code{\link{textTable}}, \code{\link{adim}}, \code{\link{arow}},
\code{\link{acol}}
}
\examples{
ttbl <- textTable(iris2_tab)
plot(ttbl)

# Remove the first column header row ("Flower part"), and reverse the 
# order of the "Sepal" and "Petal" sets of columns:
subttbl <- ttbl[-1, c(1,2,5,6,3,4)]
plot(subttbl)

# Use helper functions 'arow', 'acol' to specify indices based on 
# table structure:
i <- arow(ttbl, "colhead")[1]  # row number of first column header row
j1 <- acol(ttbl, "rowhead")    # column numbers for row header
j2 <- acol(ttbl, "colhead")    # column numbers for column header
subttbl2 <- ttbl[-i, c(j1, j2[c(3,4,1,2)])]
identical(subttbl, subttbl2)
  }
