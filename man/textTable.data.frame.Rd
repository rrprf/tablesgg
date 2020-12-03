% Auto-generated documentation for function textTable.data.frame 
%  2020-11-29 11:21:55 
\name{textTable.data.frame}
\alias{textTable.data.frame}
\title{Create a \code{texttable} as a Simple Listing of a Data Frame }
\description{
Create a \code{textTable} object representing a simple listing of a data
frame.
}
\usage{
\method{textTable}{data.frame}(x, title=character(0), subtitle=character(0), 
    foot=character(0), row.names="", na="NA", ...)
}
\arguments{
  \item{x}{A data frame.
  }
  \item{title, subtitle, foot}{Optional character vectors providing annotation for the table.  May be
empty (i.e., \code{character(0)}, the default).
  }
  \item{row.names}{A logical scalar or a character string.  If FALSE, the row names of
\code{x} are not included in the table.  If TRUE, the row names are
included as row headers, with a row header label of "row.names".  If a
character string, row names are included as row headers, and if the string
is not empty, it is used as the row header label.
  }
  \item{na}{Character string used to represent missing values (NAs) in the body of the
table.
  }
  \item{...}{Additional arguments passed to \code{format.data.frame}.
  }
}
\value{
An object with S3 class \code{textTable}.  The body of the table will
contain the values of the data frame variables, after formatting \code{x}
with \code{format(x, ...)}.  The variable names will be used as the column
header, and if \code{row.names} is not FALSE, the row names will form the
row header.
}
\details{
This function processes a data frame into a table that is simply a listing
of the data.  There is one row in the body of the table per observation in
\code{x}, and one column per variable in \code{x}.  There is at most one
layer of row headers (depending on argument \code{row.names}), and exactly
one layer of column headers (the variable names in \code{x}).
}
\examples{
data(iris, package="datasets")
ttbl <- textTable(head(iris, 10), row.names="Obs. #", 
                  title=c("The iris data", "(First 10 observations)"))
summary(ttbl)
plot(ttbl)
  }
