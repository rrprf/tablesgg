% Auto-generated documentation for function coord_justif 
%  2021-06-02 11:12:19 
\name{coord_justif}
\alias{coord_justif}
\title{Calculate X, y Positions and H|vjust Values for Entries }
\description{
Calculate coordinates and h|vjust values for entry text strings, to
achieve the desired justification and alignment with respect to cell
boundaries.  This is an internal utility function, not intended to be
called by package users.
}
\usage{
coord_justif(df, x0, x1, y0, y1, size, align=df$align)
}
\arguments{
  \item{df}{Data frame with one row per entry.  Required columns are \code{hjust},
\code{vjust}, \code{hpad}, \code{vpad}, \code{angle}.  \code{angle} must
be a multiple of 90 degrees.
  }
  \item{x0, x1, y0, y1}{Numeric vectors with one element per entry, giving the coordinates (in mm)
of the boundaries of the cell(s) containing each entry.  Note that the
origin is at the top left of the table.
  }
  \item{size}{Data frame with one row per entry, and columns \code{hsize}, \code{vsize},
and \code{descender}.  The first two are the dimensions, in mm, of an
unrotated rectangle just enclosing the (possibly rotated) entry text.  (So
\code{hsize} is always measured parallel to the x-axis and \code{vsize}
parallel to the y-axis.)  \code{descender} is the amount of space included
in \code{hsize} or \code{vsize} to allow for text descenders.  (This is 0
for a \code{textGrob}, but larger for \code{richtext_grob} and
\code{textbox_grob}.)  Functions \code{entrySize_mm} and
\code{calc_rcsize} return such a data frame (the latter in the
\code{sizeInfo} component).
  }
  \item{align}{Optional numeric vector with one element per entry.  It specifies
horizontal alignment of multiple lines within an entry *with respect to
each other*.  0 means all lines in the entry are aligned at their left
edge ("left justified"), 1 means they are all aligned at their right edge
("right justified"), 0.5 means they are aligned at their centers.  The
default is to use column \code{align} in \code{df} if present and not NA.
Otherwise it depends on text rotation specified by \code{df$angle}; for
unrotated text it is set equal to \code{df$hjust}.
  }
}
\value{
Data frame with one row per row of \code{df}.  Columns are:
  \item{x, y}{Coordinates to give to a \code{geom_*} function when drawing the entry
text.
  }
  \item{gg_hjust, gg_vjust}{\code{hjust} and \code{vjust} values to give to a \code{geom_*} function.
  }
}
\details{
The need to jointly calculate \code{x}, \code{y}, \code{gg_hjust}, and
\code{gg_vjust} is due to a difference between \code{tablesgg} and
\code{ggplot2} in their interpretations of text justification.
\code{ggplot2} interprets justification parameters as indicating position
of text relative to the (x,y) coordinates used to draw the text: 0 means
to the right/above (x,y), 1 means to the left/below (x,y).
\code{tablesgg} interprets justification parameters as position relative
to the boundaries of the table cell(s) containing the entry text: 0 means
at the left/top boundary, 1 means at the right/bottom boundary.

This function takes cell boundary coordinates \code{x0}, \code{x1},
\code{y0}, \code{y1} and \code{tablesgg}-type justification parameters and
converts them to the \code{ggplot2}-type (x,y) coordinates and
justification parameters.  In doing so it is also necessary to take any
text rotation (specified by graphical property \code{angle}) into account.

Values provided to \code{geom_*} functions should be \code{x}, \code{y},
\code{hjust=gg_hjust}, and \code{vjust=gg_vjust}.  If the \code{geom_*}
function has an \code{halign} argument (e.g., for \code{geom_textbox}),
set it to \code{gg_hjust}.

Justification values in [0, 1] will never position text closer to cell
boundaries than the padding specified by \code{df$hpad} and
\code{df$vpad}.
}
\seealso{
\code{\link{entrySize_mm}}, \code{\link{calc_rcsize}}
}
\keyword{internal}
