% Auto-generated documentation for function grSpecs 
%  2021-06-02 11:12:19 
\name{grSpecs}
\alias{grSpecs}
\title{Specifications for Graphical Properties for an Element Type }
\description{
Return a data frame with specifications for graphical properties
associated with an element type.  This is an internal function, not
intended to be called by package users.
}
\usage{
grSpecs(type)
}
\arguments{
  \item{type}{Element type, one of \code{entry}, \code{block}, \code{hvrule}.
  }
}
\value{
A data frame with one row per graphical property.  Columns are \code{name}
(the name of the property); \code{mode} (mode/type of values taken by the
property); \code{NA_ok} (logical, TRUE if NA is a valid value for the
property); and \code{scalable} (logical, TRUE if the property is to be
treated as scalable).

Property names are used as the row names of the data frame.
}
\details{
Graphical properties are documented in \code{?elements}.
}
\seealso{
\code{\link{df_from_spec}}, which uses the output from this function to
create a template data frame for graphical properties.
}
\keyword{internal}
