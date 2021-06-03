% Auto-generated documentation for function paste_text 
%  2021-06-02 11:12:19 
\name{paste_text}
\alias{paste_text}
\title{Paste Strings that may Include \code{plotmath} or Markdown }
\description{
Paste together text strings, where one or more of the strings may contain
\code{plotmath} expressions or markdown/HTML tags.  This is an internal
function, not intended to be called by package users.
}
\usage{
paste_text(x, ..., sep=c("plain"=" ", "plotmath"="~", "markdown"=" "))
}
\arguments{
  \item{x, ...}{One or more character vectors or data frames.  Data frames should contain
columns \code{text}, and optionally \code{textspec} and \code{fontface}.
(A character vector is treated as a data frame with \code{text} as its
only column.)  \code{textspec} should indicate the content type of
\code{text}; if not present it is inferred by looking for special prefixes
in \code{text}.  \code{fontface} should be 1 for plain text, 2 for bold, 3
for italic, and 4 for bold italic; if not present a value of 1 is assumed.
Each data frame will have its rows recycled to match the number of rows in
the longest.
  }
  \item{sep}{Character vector containing strings to be inserted between text from
different arguments when pasting them together.  It may have one string
per valid \code{textspec} value, named by that value.  Note that in
plotmath, "*" juxtaposes two expressions with no space between them; "~"
inserts some space; and "~~" inserts more.
  }
}
\value{
A data frame with columns \code{text}, \code{textspec}, and
\code{fontface}.  The number of rows equals the number in the longest data
frame argument.  \code{text} contains the row-by-row pasted-together text
from the arguments, and \code{textspec} specifies the content type of each
row.  \code{fontface} is NA when \code{textspec} is "plotmath", 1 when
\code{textspec} is "markdown", and taken from \code{x} when
\code{textspec} is "plain".
}
\details{
"plotmath" and "markdown" strings cannot be pasted together.  When a
"plain" string is pasted together with one of the other types, the result
will have the \code{textspec} value of the non-plain text.

If a set of "plain" text strings to be pasted together use different
fontfaces, the final text will use plotmath or markdown to accomodate the
mixed faces.
}
\note{
Changing "plain" text to "markdown" is incomplete.  For example, literal
\code{*} or \code{_} will be misinterpreted as markdown codes.
}
\seealso{
\code{\link{plotmath}}
}
\keyword{internal}
