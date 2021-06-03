% Auto-generated documentation for function plot.textTable 
%  2021-06-02 11:12:19 
\name{plot.textTable}
\alias{plot.textTable}
\title{A Plot Method for \code{texttable} Objects }
\description{
A plot method for \code{textTable} objects, displaying tables using
\code{ggplot2} graphics.
}
\usage{
\method{plot}{textTable}(x, title=NULL, subtitle=NULL, foot=NULL, rowheadLabels=NULL, 
    entryStyle=tablesggOpt("entryStyle"), 
    hvruleStyle=tablesggOpt("hvruleStyle"), 
    blockStyle=tablesggOpt("blockStyle"), scale=1.0, mergeRuns=c(TRUE, 
    TRUE), rowheadInside=FALSE, rowgroupSize=0, 
    plot.margin=tablesggOpt("plot.margin"), sizeAdjust=c(1.0, 1.0), ...)
}
\arguments{
  \item{x}{A \code{textTable} object, containing a table.
  }
  \item{title, subtitle, foot}{Optional character vectors of annotation for the table.  NULL means to
leave the current annotation unchanged (the default); \code{character(0)}
means to omit/remove it.
  }
  \item{rowheadLabels}{Optional character vector or 1-row matrix specifying labels for the row
header columns of the table.  NULL means to leave the current value
unchanged (the default); \code{character(0)} means to omit/remove it.
  }
  \item{entryStyle, hvruleStyle, blockStyle}{Optional \code{styleObj} objects.  These describe the graphical properties
(color, size, font, line type, etc) to be used for displaying table
entries, horizontal/vertical rules, or blocks, respectively.  See
\code{?styleObj} for details.  If omitted, default styles from
\code{tablesggOpt()} will be used.
  }
  \item{scale}{Numeric multiplier used to increase or decrease the displayed size of the
table, with all elements scaled proportionately.  If it has length two,
the first element applies to table entries and blocks, and the second to
hvrules.
  }
  \item{mergeRuns}{Two-element logical vector, the first element applying to row headers, the
second to column headers.  If TRUE (the default) then header cells that
contain runs of the same value will be merged into a single entry that
spans multiple rows or columns.
  }
  \item{rowheadInside}{Logical scalar.  If TRUE, the outermost layer of row headers is moved
inside the table by using its levels to divide the table into groups of
rows, with each group labeled by their level.
  }
  \item{rowgroupSize}{Numeric scalar.  If greater than 0, consecutive rows of the table body
will be grouped into sets of this size, and extra space may be inserted
between groups to improve readability.  See DETAILS.
  }
  \item{plot.margin}{Numeric vector of length 4, giving the amount of padding to be added
outside the top, right, bottom, and left sides of the table, in
millimeters.  The default is taken from \code{tablesggOpt()}.
  }
  \item{sizeAdjust}{Two-element numeric vector containing multipliers to adjust the calculated
absolute size of table text.  The first element is applied to the
horizontal dimension and the second to the vertical dimension.  (It should
not be necessary to change this.)
  }
  \item{...}{Other arguments, ignored with a warning.  (Included for compatibility with
the \code{plot} generic.)
  }
}
\value{
An object of S3 class \code{pltdTable}, inheriting from \code{ggplot}.

The object has attributes \code{plot.margin} and \code{sizeAdjust} (equal
to the arguments), and \code{size_mm} (the width and height of the plot,
in millimeters).  \code{size_mm} is calculated after applying \code{scale}
and \code{sizeAdjust}, and includes both the table and any margins
specified by \code{plot.margin}.

The object also has attributes \code{colBoundaries} and
\code{rowBoundaries} giving the coordinates of the boundaries between
columns and between rows, again in millimeters, and after applying
\code{scale} and \code{sizeAdjust}.

The object also has attribute \code{prTable}.  This a "plot-ready" version
of the table, after applying the arguments provided to this function to
\code{x}, but before processing by \code{ggplot}.  It is included to allow
convenient updating of the display properties of the table via
\code{props<-} functions.
}
\details{
\code{textTable} objects are the fundamental structure for representing
tables in the \code{tablesgg} package.  This function creates a
publication-quality plot of such a table that can be displayed on any of
R's graphics devices.  Plotting is based on the \code{ggplot2} graphics
system, and the resulting plot object can be saved or manipulated in the
usual \code{ggplot} way.

A feature of the plotted table produced by this function is that it has a
well-defined physical size, in millimeters, given by the attribute
\code{size_mm}.  This natural size is determined by the graphical
properties specified with arguments \code{entryStyle} and
\code{hvruleStyle}, and can be rescaled up or down with the \code{scale}
argument.  There is a special print (display) method for \code{pltdTable}
objects that ensures that the table is displayed at the correct size,
independent of the size of the graphics device on which it is drawn.

The plotted table can be modified by using the \code{props<-} set of
functions to update graphical properties of selected table entries,
hvrules, or blocks.  This includes disabling them (so that they are
excluded from the plot), or re-enabling disabled elements.  For broader
changes there is an \code{update} method for \code{pltdTable} objects to
change styles or plot scaling.  See \code{update.pltdTable} for more
information.
}
\seealso{
\code{\link{textTable}}, \code{\link{styleObj}},
\code{\link{tablesggOpt}}, \code{\link{styles_pkg}},
\code{\link{print.pltdTable}}, \code{\link{props<-}},
\code{\link{update.pltdTable}}
}
\examples{
# Start with a 'textTable':
ttbl <- textTable(iris2_tab)
# Default display:
plot(ttbl)
# Add annotation:
plot(ttbl, title="The iris data", subtitle="Summary statistics by species", 
     foot="sd = standard deviation")
# Smaller version:
plot(ttbl, title="The iris data", subtitle="Summary statistics by species", 
     foot="sd = standard deviation", scale=0.8)
# Use a more "spread out" style for table entries:
plot(ttbl, entryStyle=styles_pkg$entryStyle_pkg_2)
# Internal row header labels:
plot(ttbl, rowheadInside=TRUE)
# Show effect of 'plot.margin' by putting a box around the table:
# -- default
plt <- plot(ttbl) + 
       ggplot2::theme(plot.background=ggplot2::element_rect(color="blue", size=1))
plt
# -- wider margin
plt <- plot(ttbl, plot.margin=c(15, 15, 15, 15)) + 
       ggplot2::theme(plot.background=ggplot2::element_rect(color="blue", size=1))
plt

# Data frame listing with rows in groups of 5:
plot(textTable(head(iris2, 15)), rowgroupSize=5)
  }
