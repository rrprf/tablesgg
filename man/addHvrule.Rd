% Auto-generated documentation for function addHvrule 
%  2021-06-02 11:12:18 
\name{addHvrule}
\alias{addHvrule}
\title{Add a Horizontal or Vertical Rule (Hvrule) to a Table }
\description{
Add a horizontal or vertical rule (hvrule) to a table.  The location,
span, and graphical properties of the hvrule are specified explicitly,
rather than being generated automatically from the logical structure of
the table and a style.
}
\usage{
addHvrule(x, direction, arows, acols, id, props=NULL, enabled=TRUE)
}
\arguments{
  \item{x}{A plotted table (\code{pltdTable}) object.
  }
  \item{direction}{Character string specifying whether the rule is to be horizontal
(\code{hrule}) or vertical (\code{vrule}).
  }
  \item{arows, acols}{Numeric vectors specifying the location and span of the hvrule, with
respect to the augmented row-column grid of the table.  For an hrule,
\code{arows} should be a single value: the half-integer bracketed by the
table rows between which the rule runs.  For example, an hrule running
between rows 3 and 4 should have \code{arows} equal to 3.5.  \code{acols}
should be a vector of integers whose range specifies the column numbers
spanned by the rule.  For a vrule the roles of \code{arows} and
\code{acols} are reversed: \code{arows} is a vector of integers whose
range specifies the row numbers spanned by the rule, and \code{acols} is
the half-integer bracketed by the table columns between which it runs.
  }
  \item{id}{Character string giving the ID to be assigned to the new hvrule.  It is an
error if there is already an hvrule with this ID in \code{x}.  The default
is to generate an ID of the form \code{hvrule*}, where \code{*} is an
integer.
  }
  \item{props}{Optional \code{element_hvrule} object with graphical properties to assign
to the new hvrule.  Any graphical properties not specified in \code{props}
will be taken from \code{hvruleStyle_pkg_base} in \code{styles_pkg}.
  }
  \item{enabled}{Logical scalar, whether the new hvrule is to be enabled for display.  The
default is TRUE.
  }
}
\value{
A plotted table object like \code{x}, with the new hvrule added.
}
\details{
Normally hvrules are generated automatically, based on the logical
structure of the table and the style selected by the user.  This function
allows additional hvrules to be added "manually", explicitly specifying
their position and span in terms of row and column numbers.

Row and column numbers are with respect to the augmented row-column grid
of the table.  See \code{?adim} for more more information about this grid.
The helper functions \code{arow} and \code{acol} can be used to specify
arguments \code{arows} and \code{acols} in terms of table parts or
previously defined blocks.

For an \code{hrule}, the default for \code{acols} is to span all table
columns.  For a \code{vrule}, the default for \code{arows} is to span the
rows containing the body and column headers, but not the annotation.

Graphical properties for hvrules defined by this function will not be
changed if a new hvrule style is applied to the plotted table.  Use one of
the \code{props<-} functions instead.

There is no way to remove an hvrule, other than recreating the plotted
table object from scratch.  However they can be disabled using a
\code{props<-} function, and then will not be displayed or take up any
space.
}
\seealso{
\code{\link{arow}}, \code{\link{acol}}, \code{\link{adim}},
\code{\link{element_hvrule}}
}
\examples{
plt <- plot(iris2_tab, title="The iris data")
plt <- addHvrule(plt, direction="vrule", acols=4.5, arows=arow(plt, "body"), 
                 id="new_vrule", 
                 props=element_hvrule(linetype=2, color="red"), enabled=TRUE)
plt
# Can refer to the new hvrule by its ID:
props(plt, id="new_vrule") <- element_hvrule(enabled=FALSE)  # don't display it
plt
  }
