% Auto-generated documentation for function summary.textTable 
%  2020-11-29 11:21:55 
\name{summary.textTable}
\alias{summary.textTable}
\title{Summarize the Dimensions of a Table and Its Parts }
\description{
Summarize the dimensions of a table and its parts.
}
\usage{
\method{summary}{textTable}(object, ...)
}
\arguments{
  \item{object}{A \code{textTable} object, representing a 2D data summary table.
  }
  \item{...}{Additional arguments, ignored with a warning.  (Included for compatibility
with the generic.)
  }
}
\value{
An object of S3 class \code{summary.textTable}.  It is a list with
components
  \item{adim}{Dimensions of the augmented row-column grid for the table.  See
\code{?adim} for details about this grid.
  }
  \item{parts}{Data frame with one row for each table part, giving the dimensions of the
part, in columns \code{nr}, \code{nc}.
  }
}
\details{
There is a print method for objects of the returned class.
}
\seealso{
\code{\link{adim}}, \code{\link{textTable}}
}
\examples{
ttbl <- textTable(iris2_tab, title="Summary statistics for the iris data")
summary(ttbl)
  }
