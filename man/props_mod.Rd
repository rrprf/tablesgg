% Auto-generated documentation for function props_mod 
%  2021-06-02 11:12:19 
\name{props_mod}
\alias{props_mod}
\title{Update Graphical Properties for Selected Table Elements }
\description{
Update the graphical properties for selected table entries, hvrules, or
blocks in a plotted table.  This is an internal function, not intended to
be called by package users.
}
\usage{
props_mod(x, value, id=NULL, e=NULL, arows=NULL, acols=NULL, 
    setEnabled=TRUE)
}
\arguments{
  \item{x}{A \code{pltdTable} object containing a plotted table.
  }
  \item{value}{An \code{element_entry}, \code{element_refmark}, \code{element_hvrule}, or
\code{element_block} object that contains the new values for graphical
properties.
  }
  \item{id, e, arows, acols}{Arguments that identify which elements in \code{x} are to have their
graphical properties updated by \code{value}.  Exactly one of \code{id},
\code{e}, or the pair (\code{arows}, \code{acols}) should be specified.
See \code{props<-} for documentation for \code{id}, \code{propsd<-} for
\code{e} (where it is derived from argument \code{subset}), and
\code{propsa<-} for documentation for \code{arows} and \code{acols}.
  }
  \item{setEnabled}{Logical scalar.  If TRUE then any element whose properties are updated by
this function will have its \code{enabled} value set to TRUE (and thus
will be displayed in a plot).  \code{enabled} will not be changed for
elements that are not updated.  If \code{setEnabled} is FALSE,
\code{enabled} is not changed for any elements.
  }
}
\value{
An object like \code{x}, with updated graphical properties for the
selected elements.
}
\details{
This is a backend for public functions \code{props<-}, \code{propsa<-},
and \code{propsd<-}.  The latter are wrappers that accept different sets
of arguments to specify which elements are to be updated.  See their
documentation for the details.

This function overrides graphical properties in \code{x} that may have
been set by a style.  Therefore the value of \code{style_row} is set to NA
for any elements whose graphical properties are updated by this function.
}
\seealso{
\code{\link{props<-}}, \code{\link{propsa<-}}, \code{\link{propsd<-}}
}
\keyword{internal}
