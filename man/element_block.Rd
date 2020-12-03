% Auto-generated documentation for function element_block 
%  2020-11-29 11:21:55 
\name{element_block}
\alias{element_block}
\title{Specify Visual Properties for Table Blocks }
\description{
Specify a set of graphical properties that can be used to highlight a
rectangular block of table cells.
}
\usage{
element_block(fill=NULL, fill_alpha=NULL, border_size=NULL, 
    border_colour=NULL, border_color=NULL, enabled=NULL, 
    inherit.blank=FALSE)
}
\arguments{
  \item{fill, fill_alpha, border_size, border_color}{Scalar graphical properties for the table blocks.  These will be passed to
\code{ggplot2::geom_rect} under the names \code{fill}, \code{alpha},
\code{size}, and \code{colour}, respectively.
  }
  \item{border_colour}{Alias for \code{border_color}.
  }
  \item{enabled}{Logical scalar, controlling whether the block is displayed (TRUE) or not
(FALSE).
  }
  \item{inherit.blank}{Ignored.
  }
}
\value{
An object of S3 classes \code{element_block} and \code{element}.
}
\details{
This function is modeled on the \code{element_*} functions used in
\code{ggplot2} to specify graphical properties in themes.  It is primarily
used to create the value on the right-hand side of an assignment involving
the \code{props<-} group of setter functions.

To display a table block means to highlight its rectangular region with
shading (as specified by \code{fill}, a color, and \code{fill_alpha})
and/or a border (with thickness specified by \code{border_size}, in mm,
and color \code{border_color}).  Blocks are drawn before entries and
hvrules, so the latter are drawn "on top" of blocks and will not be
hidden.

Quantitative property \code{border_size} may be specified using the
\code{ggplot2} function \code{rel()}.  This function indicates that the
value is to be interpreted as a multiplier to be applied to whatever the
current value of the property is.  For example \code{border_size=rel(1.2)}
specifies that the thickness of the border around a block is to be
increased by 20\% from its current value.
}
\seealso{
\code{\link{element_entry}}, \code{\link{element_hvrule}};
\code{\link{elements}} for more detail about the available graphical
properties; \code{\link{props<-}}, \code{\link{propsa<-}},
\code{\link{propsd<-}}.
}
\examples{
plt <- plot(iris2_tab, title="Summary statistics for the iris data")
props(plt, id="rowhead") <- element_block(fill="lightblue")
props(plt, id="colblock/C/2/1") <- element_block(border_color="red")
      # (Default fill color for blocks is 'gray85')
plt
  }
