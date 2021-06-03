% Auto-generated documentation for function undo_rowhead_inside 
%  2021-06-02 11:12:19 
\name{undo_rowhead_inside}
\alias{undo_rowhead_inside}
\title{Reverse the Effect of \code{rowhead_inside} }
\description{
Reverse the effect of \code{rowhead_inside} for a \code{tblEntries}
object: Move row headers that are inside a table back to the outermost
layer of row headers, at the left side.  This is an internal function, not
intended to be called by package users.
}
\usage{
undo_rowhead_inside(x)
}
\arguments{
  \item{x}{A \code{tblEntries} object containing table entries.
  }
}
\value{
An updated \code{tblEntries} object, and with attribute
\code{rowheadInside} set to FALSE.
}
\details{
This function changes the expanded row-column grid of the table, so must
be called early in processing, before table blocks or hvrules are created.

See \code{rowhead_inside} for what happens when the outermost row header
is moved inside the table; this function reverses that.  (However the
order of entries may be changed from the original.)
}
\seealso{
\code{\link{tblEntries}}, \code{\link{rowhead_inside}}
}
\keyword{internal}
