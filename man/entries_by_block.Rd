% Auto-generated documentation for function entries_by_block 
%  2020-11-29 11:21:55 
\name{entries_by_block}
\alias{entries_by_block}
\title{Find the Entries that are Contained Within a Set of Table Blocks }
\description{
For each table block, find the entries that are completely or partially
contained within it.  This is an internal utility function, not intended
to be called by package users.
}
\usage{
entries_by_block(x, blocks, strict)
}
\arguments{
  \item{x}{An object containing table entries, such as \code{tblEntries} or
\code{prEntries} objects.
  }
  \item{blocks}{A \code{tblBlocks} object containing table blocks.
  }
  \item{strict}{Logical scalar.  If TRUE, only entries completely contained with a given
block are included.  If FALSE, entries that overlap the block but are not
completely contained within it will also be included.
  }
}
\value{
A list with one component per block in \code{blocks}.  Component \code{i}
contains a numeric vector with the indices of the entries in \code{x} that
belong to block \code{i}.
}
\keyword{internal}
