% Auto-generated documentation for function pltdSize 
%  2020-11-29 11:21:55 
\name{pltdSize}
\alias{pltdSize}
\title{Width and Height of a \code{pltdtable} Object }
\description{
Width and height of a \code{pltdTable} object.
}
\usage{
pltdSize(x, units=c("mm", "inches", "cm"))
}
\arguments{
  \item{x}{An object of class \code{pltdTable}, the result of plotting a
\code{textTable} object.
  }
  \item{units}{String specifying the units in which size is to be returned.  May be
abbreviated.
  }
}
\value{
Two-element numeric vector containing the (width, height) of the plot.  It
has attributes \code{units} and \code{device}.
}
\details{
This function returns the size, after any scaling, of a plotted table, in
physical units.  The default units are millimeters to be consistent with
other dimensions used in plotting tables.  Inches may be useful because
that is what R's graphics device functions use.

The size of a plot can depend slightly on the graphics device used to
create or render it.  The device name used internally to create the plot
is included as an attribute of the returned value.
}
\seealso{
\code{\link{plot.textTable}}
}
\examples{
plt <- plot(iris2_tab, title="Summary statistics for the iris data")
pltdSize(plt)  # width, height in millimeters

# Open a graphics device just the right size to hold the table:
sz <- pltdSize(plt, units="in")
dev.new(width=sz[1], height=sz[2], noRStudioGD=TRUE)
plt
  }
