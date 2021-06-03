% Auto-generated documentation for function as.dfObj 
%  2021-06-02 11:12:18 
\name{as.dfObj}
\alias{as.dfObj}
\title{Promote a Data Frame to Another Object Class }
\description{
Promote a data frame to another object class, or do validity checking of
such an object.  This is an internal function not intended to be called by
package users.
}
\usage{
as.dfObj(x, objClass)
}
\arguments{
  \item{x}{An object inheriting from \code{data.frame}.  Each row contains
information about the content, location, and/or graphical properties of
one table entry, hvrule, or block.  Alternatively, \code{x} may be NULL,
in which case a valid but empty object (no entries, hvrules, or blocks) is
returned.
  }
  \item{objClass}{Character string giving the class of the object to be returned/ checked.
One of: "tblEntries", "tblBlocks", "prEntries", "prHvrules", "prBlocks".
  }
}
\value{
An object with S3 classes given by \code{objClass} and "data.frame".
}
\details{
This is a backend to the \code{as.<objClass>} functions.

The purpose of this function is to reduce code repetition by factoring out
common tasks when promoting a data frame to one of the data frame-based
objects in the package (or checking the validity of such objects).

For all objects, row names are set equal to the \code{id} column.
}
\seealso{
\code{\link{tblEntries}}, \code{\link{tblBlocks}},
\code{\link{prEntries}}, \code{\link{prHvrules}}, \code{\link{prBlocks}};
\code{\link{dfSpecs}} returns info about the fields required for each
object class.
}
\keyword{internal}
