% Auto-generated documentation for function as.prHvrules 
%  2020-11-29 11:21:55 
\name{as.prHvrules}
\alias{as.prHvrules}
\title{Promote a Data Frame to a \code{prhvrules} Object }
\description{
Promote a data frame to a \code{prHvrules} object, containing plot-ready
hvrules for a table.  This is an internal function not intended to be
called by package users.
}
\usage{
as.prHvrules(x)
}
\arguments{
  \item{x}{An object inheriting from \code{data.frame}.  Each row represents one
horizontal or vertical rule (hvrule) to apppear in a table.  Columns must
include \code{id} (character, a unique identifier for each hvrule);
\code{direction} (character, either "hrule" or "vrule"); \code{arow1},
\code{arow2}, \code{acol1}, \code{acol2} (numeric position of the hvrule
within the table, in terms of row and column numbers); \code{enabled}
(logical, whether the hvrule should be displayed); and all the graphical
properties required to display an hvrule (see \code{grProps}).
  }
}
\value{
An object of S3 class \code{prHvrules} and \code{data.frame}.  See
function \code{prHvrules} for a full description of the class.
}
\details{
Normally plot-ready hvrules are created from a \code{tblBlocks} object via
function \code{prHvrules}.  This function allows them to be created or
edited directly in a data frame, without necessarily using a predefined
style.

Columns \code{arow1}, \code{arow2}, \code{acol1} and \code{acol2} refer to
row and column numbers in the augmented row-column grid of the table; see
\code{adim} for more information.

For an hrule (horizontal rule), \code{arow1} and \code{arow2} should be
equal to each other and to a half-integer; the hrule will be inserted
between table row numbers that bracket the value.  (For example,
\code{arow1 = arow2 = 3.5} means the hrule will be inserted between rows 3
and 4.)  \code{acol1} and \code{acol2} should be integers that indicate
the first and last columns to be spanned by the hrule.

For a vrule (vertical rule) the specification is similar, but with the
roles of \code{arow*} and \code{acol*} reversed.

If column \code{enabled} was not present in \code{x} it will be added,
with value FALSE for all hvrules.

If \code{x} is already a \code{prHvrules} object, this function does some
validity checks.  If \code{x} is NULL, a valid but empty \code{prHvrules}
object is returned.
}
\seealso{
\code{\link{prHvrules}}, \code{\link{tblBlocks}}
}
\keyword{internal}
