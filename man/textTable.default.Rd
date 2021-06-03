% Auto-generated documentation for function textTable.default 
%  2021-06-02 11:12:19 
\name{textTable.default}
\alias{textTable.default}
\title{Create a \code{texttable} Object from a Suitable List}
\description{
Create a \code{textTable} object from a list with one component per table
part, with validity checking.  Components \code{partdim}, \code{rowhier},
\code{colhier} are added/re-derived from the table parts.  Missing
\code{type} and \code{justification} attributes are added.  Components are
put in standard order.  This is an internal function not intended to be
called by package users.
}
\usage{
\method{textTable}{default}(x, ...)
}
\arguments{
  \item{x}{A list with components for each of the table parts.  See \code{?textTable}
for a description of the table components.  May also be an atomic vector,
which will be converted to a 1-column matrix and passed to the
\code{matrix} method.
  }
  \item{...}{Ignored, with a warning.  (Present for compatibility with the generic.)
  }
}
\value{
An object with S3 class \code{textTable}.  Table parts will be in standard
order: "title", "subtitle", "rowhead", "rowheadLabels", "colhead", "body",
"foot".
}
\details{
Methods to create a \code{textTable} from other object classes should
generally finish by calling this function.  It will do validity checking
and fill in or update the necessary components and attributes.
}
\seealso{
\code{\link{textTable}} for documentation about the class.
}
\keyword{internal}
