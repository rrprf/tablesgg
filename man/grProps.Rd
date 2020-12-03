% Auto-generated documentation for function grProps 
%  2020-11-29 11:21:55 
\name{grProps}
\alias{grProps}
\title{List with Graphical Properties for Each Element Type }
\description{
List with 1-row data frames of graphical properties for each element type.
This is an internal function, not intended to be called by package users.
}
\usage{
grProps()
}
\value{
A list with components \code{entry}, \code{block}, and \code{hvrule}.
Each component is a 1-row data frame giving the names and modes for the
graphical properties of that type.  (All values are NA, of the appropriate
mode.)
}
\details{
This function exists only because it is so !@#$\%&* clumsy to define and
install bare lists in a package.

Graphical properties are documented in \code{?elements}.
}
\keyword{internal}
