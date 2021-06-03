% Auto-generated documentation for function spec_from_text 
%  2021-06-02 11:12:19 
\name{spec_from_text}
\alias{spec_from_text}
\title{\code{textspec} Values Based on String Prefixes }
\description{
Return a vector of \code{textspec} values based on special prefixes in
strings of \code{x}.  This is an internal utility function, not intended
to be called by package users.
}
\usage{
spec_from_text(x)
}
\arguments{
  \item{x}{Character vector.
  }
}
\value{
Character vector with the same length as \code{x}, containing
\code{textspec} values.  Values are determined by looking for special
prefixes in the text strings.
}
\details{
Currently supported \code{textspec} values and the associated prefixes
are: "plain" (no recognized prefix); "plotmath" (prefix \code{MATH_}); and
"markdown" (prefix \code{MKDN_}).

The returned value will be "plain" for elements of \code{x} that are NA.
}
\seealso{
\code{\link{prefix_text}} to add or remove special prefixes to text
strings based on \code{textspec}.
}
\keyword{internal}
