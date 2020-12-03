% Auto-generated documentation for function as.prEntries 
%  2020-11-29 11:21:55 
\name{as.prEntries}
\alias{as.prEntries}
\title{Promote a Data Frame to a \code{prentries} Object }
\description{
Promote a data frame to a \code{prEntries} object, containing plot-ready
table entries.  This is an internal function not intended to be called by
package users.
}
\usage{
as.prEntries(x)
}
\arguments{
  \item{x}{An object inheriting from \code{data.frame}.  Each row contains the
content, location, and graphical properties of one table entry.  Columns
must include all those required for a \code{tblEntries} object, plus all
the graphical properties required to display an entry (see
\code{grProps}).
  }
}
\value{
An object of S3 classes \code{prEntries}, \code{tblEntries}, and
\code{data.frame}.  See function \code{prEntries} for a full description
of the class.
}
\details{
Normally plot-ready entries are created from a \code{tblEntries} object
via function \code{prEntries}.  This function allows them to be created or
edited directly in a data frame, without necessarily using a predefined
style.

Columns \code{arow1}, \code{arow2}, \code{acol1} and \code{acol2} refer to
row and column numbers in the augmented row-column grid of the table; see
\code{adim} for more information.

The graphical properties required for a plot-ready entry, and their types,
are defined in object \code{grProps}.

If not present, logical column \code{enabled} will be added with value
TRUE for all entries whose \code{text} is not NA or empty.  If attribute
\code{current_scale} is not present, it will be added with a value of 1.0.
If attribute \code{rowheadInside} is not present, it will be added with a
value of NA.

If \code{x} is already a \code{prEntries} object, this function does some
validity checks.  If \code{x} is NULL, a valid but empty \code{prEntries}
object is returned.
}
\seealso{
\code{\link{prEntries}}, \code{\link{tblEntries}}
}
\keyword{internal}
