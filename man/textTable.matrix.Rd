% Auto-generated documentation for function textTable.matrix 
%  2021-06-02 11:12:19 
\name{textTable.matrix}
\alias{textTable.matrix}
\title{Create a \code{texttable} from a Matrix }
\description{
Create a \code{textTable} object from a matrix.
}
\usage{
\method{textTable}{matrix}(x, rcnames=c(TRUE, TRUE), title=character(0), 
    subtitle=character(0), foot=character(0), na="NA", ...)
}
\arguments{
  \item{x}{A matrix.
  }
  \item{rcnames}{A logical or character vector of length 2.  The first element applies to
rows, the second to columns.  If FALSE, row/column names are not included.
If TRUE and \code{x} has row/column names, they are included as a
row/column header.  Further, if the row/column dimension itself has a
non-empty name, it is included as an additional, outer row/column header
layer.  A character string is treated the same as TRUE, except that the
string is used as the dimension name (and thus an empty string will not
create an outer header layer).
  }
  \item{title, subtitle, foot}{Optional character vectors providing annotation for the table.  May be
empty (i.e., \code{character(0)}, the default).
  }
  \item{na}{Character string used to represent missing values (NAs) in the body of the
table.
  }
  \item{...}{Additional arguments passed to \code{format(x, ...)}.
  }
}
\value{
An object with S3 class \code{textTable}.  The body of the table will
contain the matrix values, after formatting \code{x} with \code{format(x,
...)}.  Row and column names may be included as headers, depending on
argument \code{rcnames}.
}
\examples{
data(iris, package="datasets")
mat <- data.matrix(subset(iris, Species == "setosa")[, 1:4])
ttbl <- textTable(cor(mat), digits=3, title="Correlations for setosa irises")
summary(ttbl)
plt <- plot(ttbl)
# Make hvrules invisible:
propsd(plt, subset=enabled) <- element_hvrule(color=NA)
print(plt)
  }
