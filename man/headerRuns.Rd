% Auto-generated documentation for function headerRuns 
%  2020-11-29 11:21:55 
\name{headerRuns}
\alias{headerRuns}
\title{Runs of Repeated Values in a Header Matrix }
\description{
Identify runs of repeated values in a matrix of row or columns headers.
Detection of runs respects the hierarchical structure of headers.  This is
an internal utility function, not intended to be called by package users.
}
\usage{
headerRuns(x, which_head)
}
\arguments{
  \item{x}{A matrix containing table row or column headers.
  }
  \item{which_head}{Character scalar, "row" or "col", indicating whether \code{x} contains row
headers or column headers.  "row" means that each column corresponds to
one header variable or layer, ordered from outermost to innermost, left to
right (i.e., as the header would be displayed).  "col" means that each
_row_ corresponds to one header variable, ordered from outermost (top) to
innermost (bottom), again as the header would be displayed.
  }
}
\value{
A list with one component per header variable in \code{x}.  Each component
is a data frame.  The \code{i}-th data frame has one row per run of values
in the \code{i}-th header variable, with columns:
  \item{headlayer}{Header layer number, increasing from innermost (most deeply nested) to
outermost.  Thus for "row" headers it is the reverse of column number in
\code{x} (\code{ncol(x) - i + 1}), and for "col" headers it is the reverse
of row number in \code{x} (\code{nrow(x) - i + 1}).
  }
  \item{level_in_layer}{Run number within the layer.  Levels are numbered from 1, so this column
is just the sequence from 1 to the number of rows in the data frame.
  }
  \item{value}{The value in \code{x} associated with the run.
  }
  \item{start}{Position in \code{x} where the run starts (row number for "row" headers,
column number for "col" headers).
  }
  \item{runlen}{Length of the run (number of rows or columns).
  }
If \code{x} is NULL, the returned value is also NULL.  If \code{x} has no
header variables, the returned value is an empty list, \code{list()}.
}
\details{
Runs are defined cumulatively across header variables.  For the outermost
variable, runs consist of consecutive duplicated values in the first
column/row of \code{x}; for the second-outermost variable, runs are
defined by consecutive duplicated values in the first two columns/rows of
\code{x}, and so on.  (That is, the header variables are treated as a
hierarchy, with inner variables nested within outer ones.)
}
\keyword{internal}
