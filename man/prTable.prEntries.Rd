% Auto-generated documentation for function prTable.prEntries 
%  2020-11-29 11:21:55 
\name{prTable.prEntries}
\alias{prTable.prEntries}
\title{Create a Plot-Ready Table from Plot-Ready Entry, Block, and Hvrule Objects }
\description{
Create a plot-ready table from plot-ready entry, block, and hvrule
objects.  This is an internal function, not intended to be called by
package users.
}
\usage{
\method{prTable}{prEntries}(x, entries=x, blocks=prBlocks(tblBlocks(x)), hvrules, ...)
}
\arguments{
  \item{x}{A \code{prEntries} object, containing plot-ready table entries.
  }
  \item{entries}{An alias for \code{x}.
  }
  \item{blocks}{A \code{prBlocks} object, containing plot-ready blocks of table cells.
May be NULL.  The default is to create the standard set of blocks by
calling \code{tblBlocks(x)}, and style (but not enable) them using the
default block style.
  }
  \item{hvrules}{A \code{prHvrules} object, containing plot-ready hvrules for the table.
May be NULL if there are no hvrules.  The default is to create hvrules by
calling \code{prHvrules(blocks)} (which uses the default hvrule style) if
\code{blocks} is not NULL, and NULL otherwise.
  }
  \item{...}{Ignored, with a warning.  (Included for compatibility with the generic.)
  }
}
\value{
An object of S3 class \code{prTable}, a plot-ready table.  This is a list
with components:
  \item{entries}{A \code{prEntries} object with the content, position, and graphical
properties for each entry in the table.
  }
  \item{blocks}{A \code{prBlocks} object with the positions, sizes, and graphical
properties of blocks of table cells.  May be empty if no blocks are
defined.
  }
  \item{hvrules}{A \code{prHvrules} object with the content, position, and graphical
properties for each horizontal or vertical rule (hvrule) in the table.
May be empty if there are no hvrules.
  }
It has attribute \code{current_scale}, a numeric vector of length 2.  The
first element is for \code{entries}, the second for \code{hvrules}.
}
\details{
This function allows the creation of a plot-ready table directly from its
entry, block, and hvrule components.  A \code{prTable} object can be
updated with new styles or scales for its components by calling the
\code{.prTable} method.
}
\seealso{
\code{\link{prEntries}}, \code{\link{prBlocks}}, \code{\link{prHvrules}}.
}
\keyword{internal}
