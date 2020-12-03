% Auto-generated documentation for function update.textTable 
%  2020-11-29 11:21:56 
\name{update.textTable}
\alias{update.textTable}
\title{Update a \code{texttable} Object }
\description{
Update a \code{textTable} object with new annotation or rowheadLabels.
}
\usage{
\method{update}{textTable}(object, title=NULL, subtitle=NULL, foot=NULL, 
    rowheadLabels=NULL, ...)
}
\arguments{
  \item{object}{A \code{textTable} object, representing a 2D table with all cells
formatted as character strings.
  }
  \item{title, subtitle, foot}{Optional character vectors of annotation for the table.  NULL means to
leave the current annotation unchanged (the default); \code{character(0)}
means to omit/remove it.
  }
  \item{rowheadLabels}{Optional character vector or 1-row matrix specifying labels for the row
header columns of the table.  NULL means to leave the current value
unchanged (the default); \code{character(0)} means to omit/remove it.
  }
  \item{...}{Ignored, with a warning.  (Present for compatibility with the generic.)
  }
}
\value{
A \code{textTable} object with annotation set or changed based on the
provided arguments.
}
\details{
To indicate that a string in \code{title}, \code{subtitle}, \code{foot},
or \code{rowheadLabels} is to be interpreted as a \code{plotmath}
expression, prefix it with \code{MATH_}.
}
\seealso{
\code{\link{textTable}}
}
\examples{
ttbl <- textTable(iris2_tab, title="The iris data", 
                  foot="sd = standard deviation")
# Change annotation:
ttbl <- update(ttbl, title=c("The iris data", "Summary statistics by species"), 
               foot=character(0))
plot(ttbl)
# Change row header labels:
ttbl <- update(ttbl, rowheadLabels=c("Species", "Summary\nstatistic"))
plot(ttbl)
  }
