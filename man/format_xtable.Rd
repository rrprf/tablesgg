% Auto-generated documentation for function format_xtable 
%  2021-06-02 11:12:19 
\name{format_xtable}
\alias{format_xtable}
\title{Format Entries in an \code{xtable} into Character Strings }
\description{
Format entries in an \code{xtable} object into character strings.  (This
is essentially a \code{format} method for \code{xtable} objects.)  This is
an internal function not intended to be called by package users.
}
\usage{
format_xtable(x, row.names=TRUE, na="NA", mathExponents=TRUE, ...)
}
\arguments{
  \item{x}{An \code{xtable} object.
  }
  \item{row.names}{Logical scalar.  If FALSE, the row names of \code{x} are not included in
the returned formatted table.
  }
  \item{na}{String to be used to represent missing values in \code{x}.  The default
value is the empty string "".
  }
  \item{mathExponents}{Logical scalar.  If TRUE, then numerical values in \code{x} that are
formatted into scientific notation (i.e., strings like "3.14e-02",
specified by values of \code{e} or \code{E} in the \code{display}
attribute of \code{x}) will be plotted in math style, with the power of 10
shown as a superscript.
  }
  \item{...}{Additional named arguments passed to \code{formatC}, when converting
values in \code{x} to character strings.  They must not include
\code{digits} or \code{format}, which are specified within \code{x}
itself.
  }
}
\value{
A character matrix with one row per row in \code{x} (not counting column
names), and one column per column in \code{x} (including a column of row
names, if \code{row.names} is TRUE).
}
\details{
This is mostly code extracted from \code{print.xtable} in version 1.8-4 of
the \code{xtable} package by David B.  Dahl et al.
}
\keyword{internal}
