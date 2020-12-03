% Auto-generated documentation for function element_entry 
%  2020-11-29 11:21:55 
\name{element_entry}
\alias{element_entry}
\title{Specify Visual Properties for Table Entries and their Cells }
\description{
Specify a set of graphical properties that can be used to display table
entries and the cells that contain them.
}
\usage{
element_entry(text=NULL, family=NULL, fontface=NULL, colour=NULL, 
    alpha=NULL, size=NULL, hjust=NULL, vjust=NULL, angle=NULL, 
    lineheight=NULL, color=NULL, hpad=NULL, vpad=NULL, fill=NULL, 
    fill_alpha=NULL, border_size=NULL, border_colour=NULL, 
    border_color=NULL, enabled=NULL, math=NULL, inherit.blank=FALSE)
}
\arguments{
  \item{text}{Scalar character string, the text to be displayed for the entries.
  }
  \item{family, fontface, color, alpha, size, hjust, vjust, angle, lineheight}{Scalar values for the graphical properties that are used to display the
text of table entries.  Values will be passed to
\code{ggplot2::geom_text}.
  }
  \item{hpad, vpad}{Amount of blank space to add on the left and right (\code{hpad}) and top
and bottom (\code{vpad}) of the entry text, in millimeters.  This is to
keep entry text from being too close to the cell borders.
  }
  \item{fill, fill_alpha, border_size, border_color}{Scalar graphical properties for the _cells_ that contain table entries.
These will be passed to \code{ggplot2::geom_rect} under the names
\code{fill}, \code{alpha}, \code{size}, and \code{colour}, respectively.
  }
  \item{colour, border_colour}{Aliases for \code{color}, \code{border_color}.
  }
  \item{enabled}{Logical scalar, controlling whether the entry is displayed (TRUE) or not
(FALSE).  This applies to both the entry text and the cells that contain
it.
  }
  \item{math}{Logical scalar.  If TRUE the entry text will be treated as a
\code{plotmath} expression rather than simple text.
  }
  \item{inherit.blank}{Ignored.
  }
}
\value{
An object of S3 classes \code{element_entry} and \code{element}.
}
\details{
This function is modeled on the \code{element_*} functions used in
\code{ggplot2} to specify graphical properties in themes.  It is primarily
used to create the value on the right-hand side of an assignment involving
the \code{props<-} group of setter functions.

The text content of an entry is perhaps not strictly a graphical property,
but it is convenient to have an easy way to modify it.  Note that like all
other properties, the provided value must be a single value (character
string), not a longer vector of values.

Justification of text within a cell is specified by properties
\code{hjust} and \code{vjust}.  Their interpretation is with respect to
the boundaries of the cell: 0 means justification toward the left/top of
the cell, 1 means toward the right/bottom, and 0.5 means centered.  This
is different from the interpretation in \code{?ggplot2::geom_text}, where
the justification is with respect to an (x, y) point.  Note that padding
(\code{hpad}, \code{vpad}) is added _after_ justification, so for example
\code{hjust=0} will put the text at a distance \code{hpad} from the left
border of its cell.

Quantitative properties \code{size}, \code{angle}, \code{lineheight},
\code{hpad}, \code{vpad}, and \code{border_size} may be specified using
the \code{ggplot2} function \code{rel()}.  This function indicates that
the value is to be interpreted as a multiplier to be applied to whatever
the current value of the property is.  For example
\code{lineheight=rel(1.2)} specifies that the lineheight property of an
entry is to be increased by 20\% from its current value.
}
\seealso{
\code{\link{element_hvrule}}, \code{\link{element_block}},
\code{\link{element_refmark}}; \code{\link{elements}} for more detail
about the available graphical properties; \code{\link{props<-}},
\code{\link{propsa<-}}, \code{\link{propsd<-}}.
}
\examples{
plt <- plot(iris2_tab, title="Summary statistics for the iris data", 
            subtitle="Shown with default graphical properties")
plt

props(plt, id="body") <- element_entry(fontface=3, fill="gray")
props(plt, id="subtitle,1") <- 
      element_entry(text="Entry properties changed by 'props<-'",
                    fill="gray", color="red")
plt
  }
