% Auto-generated documentation for function summary.pltdTable 
%  2020-11-29 11:21:55 
\name{summary.pltdTable}
\alias{summary.pltdTable}
\title{Summarize the Dimensions and Options of a Plotted Table }
\description{
Summarize the dimensions and display options of a plotted table.
}
\usage{
\method{summary}{pltdTable}(object, ...)
}
\arguments{
  \item{object}{A \code{pltdTable} object, a plotted 2D data summary table.
  }
  \item{...}{Additional arguments, ignored with a warning.  (Included for compatibility
with the generic.)
  }
}
\value{
An object of S3 class \code{summary.pltdTable}.  It is a list with
components
  \item{adim}{Dimensions of the augmented row-column grid for the table.  See
\code{?adim} for details about this grid.
  }
  \item{parts}{Data frame with one row for each table part, giving the dimensions of the
part, in columns \code{nr}, \code{nc}.
  }
  \item{mergeRuns, rowheadInside, rowgroupSize, scale, plot.margin, sizeAdjust}{Display options used by the table.  See \code{plot.textTable} for their
meaning.
  }
}
\details{
There is a print method for objects of the returned class.
}
\seealso{
\code{\link{adim}}, \code{\link{plot.textTable}}
}
\examples{
ttbl <- textTable(iris2_tab, title="Summary statistics for the iris data")
plt <- plot(ttbl, rowheadInside=TRUE)
summary(plt)
  }
