% Auto-generated documentation for function element_hvrule 
%  2021-06-02 11:12:18 
\name{element_hvrule}
\alias{element_hvrule}
\title{Specify Visual Properties for Table Rules }
\description{
Specify a set of graphical properties that can be used to display
horizontal or vertical rules (hvrules) in a table.
}
\usage{
element_hvrule(colour=NULL, alpha=NULL, linetype=NULL, size=NULL, 
    fill=NULL, fill_alpha=NULL, space=NULL, color=NULL, enabled=NULL, 
    inherit.blank=FALSE)
}
\arguments{
  \item{color, alpha, linetype, size}{Scalar values for the graphical properties that are used to display the
horizontal or vertical line of an hvrule.  Values will be passed to
\code{ggplot2::geom_segment}.
  }
  \item{fill, fill_alpha, space}{Scalar values for the graphical properties of the (long, thin) rectangle
that encloses an hvrule.  \code{space} is the height (for a horizontal
rule) or width (for a vertical rule) of the rectangle, in millimeters.
\code{fill} and \code{fill_alpha} are passed to \code{ggplot2::geom_rect}
as arguments \code{fill} and \code{alpha} respectively.
  }
  \item{colour}{Alias for \code{color}.
  }
  \item{enabled}{Logical scalar, controlling whether the hvrule is displayed (TRUE) or not
(FALSE).  This applies to both the actual rule (line) and the rectangle
containing it.
  }
  \item{inherit.blank}{Ignored.
  }
}
\value{
An object of S3 classes \code{element_hvrule} and \code{element}.
}
\details{
This function is modeled on the \code{element_*} functions used in
\code{ggplot2} to specify graphical properties in themes.  It is primarily
used to create the value on the right-hand side of an assignment involving
the \code{props<-} group of setter functions.

A horizontal or vertical rule (hvrule) is actually drawn as long, narrow
rectangle, with a line centered inside it.  The narrowness of the
rectangle, and thus how much space the hvrule adds to the table, is
controlled by \code{space}.  The thickness of the line inside the
rectangle is controlled by \code{size}.  Setting \code{linetype} to 0
means no line will be drawn, but the enclosing rectangle will be.  In that
way hvrules can be used to insert extra blank space between rows or
columns of a table.

Quantitative properties \code{size} and \code{space} may be specified
using the \code{ggplot2} function \code{rel()}.  This function indicates
that the value is to be interpreted as a multiplier to be applied to
whatever the current value of the property is.  For example
\code{space=rel(1.2)} specifies that the \code{space} property of an entry
is to be increased by 20\% from its current value.
}
\seealso{
\code{\link{element_entry}}, \code{\link{element_block}};
\code{\link{elements}} for more detail about the available graphical
properties; \code{\link{props<-}}, \code{\link{propsa<-}},
\code{\link{propsd<-}}.
}
\examples{
plt <- plot(iris2_tab, title="Summary statistics for the iris data")
plt

# Enable the vertical rule between rowhead and body, and set its
# properties:
props(plt, id="rowhead_right") <- element_hvrule(enabled=TRUE, linetype=1, 
                                                 color="black", space=10)
plt
# Change the properties of all enabled hvrules:
propsd(plt, subset=(enabled)) <- element_hvrule(color="red")
plt
  }
