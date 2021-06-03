% Auto-generated documentation for function textTable.tblEntries 
%  2021-06-02 11:12:19 
\name{textTable.tblEntries}
\alias{textTable.tblEntries}
\title{Reconstruct a \code{texttable} Object from a \code{tblentries} Object }
\description{
Reconstruct a \code{textTable} object from a \code{tblEntries} object.
This is an internal utility function, not intended to be called by package
users.
}
\usage{
\method{textTable}{tblEntries}(x, ...)
}
\arguments{
  \item{x}{A \code{tblEntries} object.  The columns used are \code{part},
\code{partrow}, \code{partcol}, \code{arow1}, \code{arow2}, \code{acol1},
\code{acol2}, \code{text}, \code{textspec}, and optionally \code{hjust}
and \code{type}.
  }
  \item{...}{Ignored, with a warning.  (Present for compatibility with the generic.)
  }
}
\value{
A \code{textTable} object.
}
\seealso{
\code{\link{tblParts}}
}
\keyword{internal}
