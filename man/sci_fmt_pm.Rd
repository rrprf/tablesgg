% Auto-generated documentation for function sci_fmt_pm 
%  2020-11-29 11:21:55 
\name{sci_fmt_pm}
\alias{sci_fmt_pm}
\title{Convert ASCII Scientific Notation into \code{plotmath} Form }
\description{
Convert numbers formatted as ASCII scientific notation (e.g., "1.2e-01")
into \code{plotmath} form that will display the exponent as "10" with a
superscript.  This is an internal function not intended to be called by
package users.
}
\usage{
sci_fmt_pm(x)
}
\arguments{
  \item{x}{A character vector.  Each element should contain text representing a
single number.  Those elements for which the text indicates a number in
scientific notation will be converted.
  }
}
\value{
A character vector with the same length as \code{x}.  Any elements that
are to be treated as \code{plotmath} expressions will begin with the
characters \code{MATH_}
}
\seealso{
\code{\link{plotmath}}, \code{\link{paste_pm}}
}
\keyword{internal}
