% Auto-generated documentation for function add_refmark 
%  2021-06-02 11:12:18 
\name{add_refmark}
\alias{add_refmark}
\title{Add a Reference Mark to a Character Vector or Matrix }
\description{
Utility function to add a reference mark to each element of a character
vector or matrix.  This is an internal function, not intended to be called
by package users.
}
\usage{
add_refmark(text, textspec, mark, side, raise)
}
\arguments{
  \item{text}{Character vector or matrix of text strings to be marked.
  }
  \item{textspec}{Character vector or matrix, parallel to \code{text}, indicating whether
text already contains a \code{plotmath} expression ("plotmath") or
markdown ("markdown"), or is plain text ("plain").
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
List with components \code{text} and \code{textspec}.  The first will be a
character vector or matrix like input \code{text}, updated to include the
reference mark in each string.

Component \code{textspec} will be a character vector or matrix with the
same shape as \code{text}, and value "plain", "plotmath", or "markdown",
according to the contents of \code{text} after adding the reference mark.
}
\details{
If \code{raise} is TRUE, every element in the returned text will contain
either plotmath or markdown notation.  If a text string was originally
"plain", the default is to use markdown if the package option
\code{allowMarkdown} is TRUE, and plotmath otherwise.

It is an error if a "plain" text string contains newline (\code{\\n})
characters but needs to be converted to plotmath to add a reference mark.
(\code{plotmath} ignores newline characters.)

If \code{raise} is FALSE, the \code{textspec} value for each string is the
same as on input.
}
\keyword{internal}
