% Auto-generated documentation for function apply_scale 
%  2021-06-02 11:12:18 
\name{apply_scale}
\alias{apply_scale}
\title{Apply a Scale Factor to Table Elements }
\description{
Apply a scale factor to table elements, to change their overall displayed
size.  This is an internal function, not intended to be called by package
users.
}
\usage{
apply_scale(x, type, scale)
}
\arguments{
  \item{x}{A data frame containing table elements (entries, hvrules, or blocks).
  }
  \item{type}{Character string with the element type for \code{x}, one of \code{entry},
\code{block}, or \code{hvrule}.
  }
  \item{scale}{Numeric scalar; a multiplier used to increase or decrease the displayed
size of all the elements in \code{x}.
  }
}
\value{
A data frame like \code{x}, with the columns containing scalable graphical
properties multiplied by \code{scale}.
}
\seealso{
\code{\link{grSpecs}} specifies which graphical properties are scalable
for each element type.
}
\keyword{internal}
