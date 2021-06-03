% Auto-generated documentation for function prefix_text 
%  2021-06-02 11:12:19 
\name{prefix_text}
\alias{prefix_text}
\title{Add or Remove the Prefix Associated with \code{textspec} Values }
\description{
Add or remove special, \code{textspec}-related prefixes from text strings.
This is an internal utility function, not intended to be called by package
users.
}
\usage{
prefix_text(x, spec, action)
}
\arguments{
  \item{x}{Character vector to which special prefixes are to be added or removed.
  }
  \item{spec}{Character vector of valid \code{textspec} values.  Recycled to match the
length of \code{x} if \code{action} is "add".  Ignored if \code{action} is
"remove".
  }
  \item{action}{Character scalar, either "add" or "remove", depending on whether the
special prefix is to be added or removed from \code{x}.
  }
}
\value{
Character vector like \code{x}, with prefixes added or removed.  NA values
in \code{x} remain NA.
}
\details{
Currently supported \code{textspec} values and the associated prefixes
are: "plain" (no prefix); "plotmath" (prefix \code{MATH_}); and "markdown"
(prefix \code{MKDN_}).
}
\seealso{
\code{\link{spec_from_text}} to derive \code{spec} by looking for special
prefixes in text strings.
}
\keyword{internal}
