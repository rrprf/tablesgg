% Auto-generated documentation for function paste_pm 
%  2020-11-29 11:21:55 
\name{paste_pm}
\alias{paste_pm}
\title{Paste Strings that may Include \code{plotmath} Expressions }
\description{
Paste together text strings, where one or more of the strings may
represent \code{plotmath} expressions.  This is an internal function, not
intended to be called by package users.
}
\usage{
paste_pm(x, ..., sep=c("", "*"))
}
\arguments{
  \item{x, ...}{One or more data frames.  Each should contain columns \code{text},
\code{math}, and optionally \code{fontface} (character, logical, numeric,
respectively).  \code{math} should be TRUE if \code{text} already contains
a plotmath expression.  \code{fontface} should be 1 for plain text, 2 for
bold, 3 for italic, and 4 for bold italic; if not present a value of 1 is
assumed.  Each data frame will have its rows recycled to match the number
of rows in the longest.  Then \code{text} from each will be concatenated
row-by-row into single strings.
  }
  \item{sep}{Character string inserted between text from different arguments when
pasting them together.  If of length 2, the first element is used when
pasting non-plotmath texts, and the second is used when at least one of
the texts is plotmath.  Note that in plotmath, "*" juxtaposes two
expressions with no space between them; "~" inserts some space; and "~~"
inserts more.
  }
}
\value{
A data frame with columns \code{text} and \code{math}, and number of rows
equal to the number in the longest data frame argument.  \code{text}
contains the pasted-together text from the arguments, and \code{math} is
TRUE if the combined text contains a plotmath expression, and FALSE
otherwise.
}
\details{
If a set of text strings to be pasted together do not contain plotmath but
use different font faces, the final text will use plotmath to accomodate
the mixed faces.  However \code{plotmath} does not offer different font
faces for Greek letters or math notation.  So \code{fontface} will have no
effect for \code{text} for which \code{math} is already TRUE.
}
\seealso{
\code{\link{plotmath}}
}
\keyword{internal}
