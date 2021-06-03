% Auto-generated documentation for function df_from_spec 
%  2021-06-02 11:12:18 
\name{df_from_spec}
\alias{df_from_spec}
\title{Create a Data Frame from a Specification }
\description{
Create a data frame from a specification that describes its column names
and modes.  All entries are NA (of the appropriate mode).  This is an
internal function, not intended to be called by package users.
}
\usage{
df_from_spec(x, n)
}
\arguments{
  \item{x}{A data frame with (at least) columns \code{name} and \code{mode}.
  }
  \item{n}{The number of rows desired for the returned data frame.  May be 0.
  }
}
\value{
A data frame with as many columns as \code{x} has rows.  Column names are
taken from \code{x$name} and modes from \code{x$mode}.  (See
\code{?mode}).
}
\keyword{internal}
