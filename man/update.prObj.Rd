% Auto-generated documentation for function update.prObj 
%  2020-11-29 11:21:55 
\name{update.prObj}
\alias{update.prObj}
\title{Update Style or Scale for Plot-Ready Table Elements }
\description{
Update a \code{prEntries}, \code{prBlocks}, or \code{prHvrules} object
with a new style and/or new scale.  This is an internal utility function,
not intended to be called directly by package users.
}
\usage{
\method{update}{prObj}(object, style=NULL, scale=NULL, ...)
}
\arguments{
  \item{object}{A \code{prEntries}, \code{prBlocks}, or \code{prHvrules} object,
containing plot-ready table elements.
  }
  \item{style}{Optional \code{styleObj} object, specifying a new style for assigning
graphical properties to table elements in \code{object}.  The default
value of NULL leaves the style of \code{object} unchanged.  When
\code{object} is a \code{prHvrules} object, this must be NULL; see
DETAILS.
  }
  \item{scale}{Optional numeric multiplier used to increase or decrease the displayed
size of table elements, relative to the natural size implied by the
(updated) style.  The default is to use a value of 1.0 if a \code{style}
is specified, and to not change the scaling if the style is not being
updated.
  }
  \item{...}{Ignored, with a warning.  (Included for compatibility with the generic.)
  }
}
\value{
An object with the same class as \code{object}.  See \code{prEntries},
\code{prBlocks}, or \code{prHvrules} for the structure of these objects.
}
\details{
The \code{enabled} field is not changed for any elements.  (Changing
\code{enabled} for entries could have cascading effects on blocks and
hvrules.)

To provide a mechanism to hold graphical properties fixed for certain
elements, \code{style} is not applied to any elements for which
\code{style_row} is 0.  This will be true for elements created manually
via \code{addBlock} or \code{addHvrule} for example.  Graphical properties
for such elements can be changed using \code{props<-}.

The style of a \code{prHvrules} object cannot be updated directly, because
the object does not contain the block information generally needed to
apply styles.  Instead, a new set of hvrules should be created from a
\code{tblBlocks} object with the desired style, using function
\code{prHvrules}.
}
\seealso{
\code{\link{styleObj}}, \code{\link{update.prTable}}
}
\keyword{internal}
