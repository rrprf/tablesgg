% Auto-generated documentation for function add_refmark 
%  2020-11-29 11:21:55 
\name{add_refmark}
\alias{add_refmark}
\title{Add a Reference Mark to a Character Vector or Matrix }
\description{
Utility function to add a reference mark to a character vector or matrix,
given the current plotmath status of each element.  This is an internal
function, not intended to be called by package users.
}
\usage{
add_refmark(text, is_math=NULL, mark, side, raise)
}
\arguments{
  \item{text}{Character vector or matrix of text strings to be marked.
  }
  \item{is_math}{Logical vector or matrix, parallel to \code{text}, indicating whether text
was already a \code{plotmath} expression.  If this argument is omitted or
NULL, it will be inferred by looking for a leading \code{MATH_} string at
the beginning of each element in \code{text}.
  }
  \item{mark}{Character string to be used as the reference mark.
  }
  \item{side}{On which side of the text should the reference mark be placed, "before" or
"after".
  }
  \item{raise}{Logical scalar.  If TRUE, the reference mark will be displayed as a
superscript, using \code{plotmath} notation.
  }
}
\value{
Character vector or matrix like \code{text}, updated to include the
reference mark in each string.  If \code{is_math} was missing or NULL, any
string to be interpreted as a \code{plotmath} expression will be prefixed
by \code{MATH_}.
}
\details{
If \code{raise} is TRUE, every element in the returned text is to be
interpreted as plotmath notation.  If \code{raise} is FALSE, the plotmath
status of each string is the same as on input.
}
\keyword{internal}
