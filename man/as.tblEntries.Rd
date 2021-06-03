% Auto-generated documentation for function as.tblEntries 
%  2021-06-02 11:12:18 
\name{as.tblEntries}
\alias{as.tblEntries}
\title{Promote a Data Frame to a \code{tblentries} Object }
\description{
Promote a data frame to a \code{tblEntries} object.  This is an internal
function not intended to be called by package users.
}
\usage{
as.tblEntries(x)
}
\arguments{
  \item{x}{An object inheriting from \code{data.frame}.  Each row represents one
table entry, with information about its content, location, and structural
role in the table.  See \code{dfSpecs} for the required columns and types,
and \code{tblEntries} for a description of these columns.  \code{x} must
also have attributes \code{rowhier} and \code{colhier} with information
about the row and column header hierarchies (see \code{headerRuns}).
  }
}
\value{
An object of S3 class \code{tblEntries} and \code{data.frame}.
}
\details{
Normally table entries are created from a \code{textTable} object via
function \code{tblEntries}.  This function allows them to be created or
edited directly in a data frame.

Columns \code{arow1}, \code{arow2}, \code{acol1} and \code{acol2} refer to
row and column numbers in the augmented row-column grid of the table; see
\code{adim} for more information.

If \code{x} is already a \code{tblEntries} object, this function does some
validity checks.  If \code{x} is NULL, a valid but empty \code{tblEntries}
object is returned.
}
\seealso{
\code{\link{tblEntries}}, \code{\link{headerRuns}}
}
\keyword{internal}
