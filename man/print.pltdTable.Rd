% Auto-generated documentation for function print.pltdTable 
%  2021-06-02 11:12:19 
\name{print.pltdTable}
\alias{print.pltdTable}
\title{Print (Draw) a \code{pltdtable} Object }
\description{
Print method for \code{pltdTable} objects, to display the table on the
currently active graphics device.  It draws the table at its natural size,
as determined by the font size and dimensions specified by the styles used
to create the table, and after applying any scale factors.
}
\usage{
\method{print}{pltdTable}(x, scale=NULL, newpage=TRUE, position=c(0.5, 0.5), 
    vpx=grid::unit(0.5, "npc"), vpy=grid::unit(0.5, "npc"), 
    just="center", ...)
}
\arguments{
  \item{x}{A \code{pltdTable} object, representing a table.
  }
  \item{scale}{Optional numeric multiplier used to increase or decrease the displayed
size of the table, relative to the natural size implied by its styles.
The default is to keep the current scaling in \code{x}.
  }
  \item{newpage}{Whether to draw the table on the current page of the graphics device, or
on a new blank page.
  }
  \item{position}{Two-element numeric or character vector specifying the horizontal and
vertical position of the table on the page.  A value of 0 means
left/bottom justification, 1 means right/top justification, and
intermediate values shift the table linearly between those limits.  If a
character vector, valid values are "left", "center", "right" for
horizontal position, and "bottom", "center", "top" for vertical position.
The default is to center the table on the page.
  }
  \item{vpx, vpy, just}{Alternatives to \code{position} to specify the position of the table on
the page.  These are passed to \code{grid::viewport}.  (\code{vpx} is
passed as argument \code{x}, and \code{vpy} as argument \code{y}.)
Ignored with a warning if \code{position} is specified.
  }
  \item{...}{Optional additional arguments passed to \code{grid::viewport}.
  }
}
\value{
\code{x}, invisibly.
}
\details{
The purpose of a special print method for \code{pltdTable} objects is to
set a particular viewport size, so that the table is displayed at its
natural size, adjusted for \code{scale}.  Once the viewport is set, the
table is drawn using the usual \code{ggplot} print method.

Note that scaling of table size is not cumulative.  If \code{x} has
already been scaled (say, by a factor of 0.8), and argument \code{scale}
is set to 0.9, then the table will be displayed at 0.9 times its natural
size, not 0.9*0.8 = 0.72.

The default is that the table is drawn centered in the current graphics
viewport (usually the whole graphics device surface).  This can be changed
using either the \code{position} or the \code{just}, \code{vpx} and
\code{vpy} arguments. \code{grid::viewport} uses the latter four numbers
to specify position; see its documentation for details.  The
\code{position} argument simplifies this to use just two numbers, each in
[0, 1].  It assumes one rarely wants to have parts of the table outside
the boundary of the page, so 0 corresponds to putting the table snug
against the left or bottom edge, 1 corresponds to putting it against the
right or top edge, and intermediate values simply linearly interpolate
between those limits.
}
\seealso{
\code{\link{plot.textTable}}, \code{grid::viewport},
\code{ggplot2::print.ggplot}
}
\examples{
# Start with different ways of arranging the Titanic data:
data(Titanic, package="datasets")
ftbl1 <- ftable(Titanic, row.vars=c("Class", "Sex"), col.vars="Survived")
ftbl2 <- ftable(Titanic, row.vars=c("Age", "Sex", "Survived"), 
                col.vars="Class")
ftbl3 <- ftable(Titanic, row.vars=c("Sex", "Class"), 
                col.vars=c("Age", "Survived"))
plt1 <- plot(textTable(ftbl1))
plt2 <- plot(textTable(ftbl2))
plt3 <- plot(textTable(ftbl3))
plt4 <- plot(textTable(ftbl3), rowheadInside=TRUE)

# Center plots in the four quadrants of the page:
print(plt1, vpx=0.25, vpy=0.75)
print(plt2, vpx=0.75, vpy=0.75, newpage=FALSE)
print(plt3, vpx=0.25, vpy=0.25, newpage=FALSE)
print(plt4, vpx=0.75, vpy=0.25, newpage=FALSE)

# Single plot at different sizes, pushed to corners of the page:
print(plt2, scale=0.8, position=c("left", "top"))
print(plt2, scale=1.2, position=c("right", "bottom"), newpage=FALSE)
  }
