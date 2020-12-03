% Auto-generated documentation for function propsd<- 
%  2020-11-29 11:21:55 
\name{propsd<-}
\alias{propsd<-}
\title{Update Graphical Properties for Selected Table Elements }
\description{
Update the graphical properties for table elements selected based on the
values of element descriptors.
}
\usage{
propsd(x, subset=NULL, setEnabled=TRUE) <- value
}
\arguments{
  \item{x}{A \code{pltdTable} object containing a plotted table.
  }
  \item{subset}{An expression that when evaluated gives a logical vector indicating which
elements should be updated; missing values are taken as FALSE.  See
DETAILS.
  }
  \item{setEnabled}{Logical scalar.  If TRUE then any element whose properties are updated by
this function will have its \code{enabled} value set to TRUE (and thus
will be displayed in a plot).  \code{enabled} will not be changed for
elements that are not updated.  If \code{setEnabled} is FALSE,
\code{enabled} is not changed for any elements.
  }
  \item{value}{An \code{element_entry}, \code{element_refmark}, \code{element_hvrule}, or
\code{element_block} object that contains the new values for graphical
properties.
  }
}
\value{
An object like \code{x}, with updated graphical properties for the
selected elements.
}
\details{
There are three similar functions that can be used to modify the graphical
properties of table elements: \code{props<-}, \code{propsa<-}, and
\code{propsd<-}.  They differ only in how one specifies which elements are
to be modified.  \code{props<-} uses element or block ID's, or searches
the text content of entries.  \code{propsa<-} uses explicit row and column
numbers within the augmented row-column grid.  \code{propsd<-} uses the
values of element descriptors (as described in \code{?elements}).

The type of elements that are updated is determined by \code{value}: if
\code{value} is an \code{element_entry} or \code{element_refmark} object
then entries are updated; if it is an \code{element_hvrule} object then
hvrules are; if it is an \code{element_block} object then blocks are.  See
the documentation for \code{props<-} for discussion of their use.

Internally entries, hvrules, and blocks are represented as data frames,
with one row per element, and columns describing their content, position,
and structural role in the table.  (See \code{?elements} for a list of the
descriptor columns for each type of element.)  Argument \code{subset} is
an expression involving those columns that evaluates to a logical vector;
the elements for which this vector is TRUE will be selected.  (NA in the
logical vector is treated as FALSE.)  Thus the \code{subset} argument
works in the same way as R's built-in \code{subset} function to select
rows from a data frame.  For example \verb{subset=(part=="body" &
type=="numeric")} will update graphical properties for all entries in the
table body that have been tagged as representing numbers.  And
\code{subset=(direction=="hrule")} will update graphical properties for
horizontal rules but not vertical rules.

To update every element (of the type implied by \code{value}) in the
table, set \code{subset=TRUE}.  To update just the currently enabled
elements, set \code{subset=enabled}.

Note: Standard processing generates hvrules for all four sides of every
block in the table.  Since generally one doesn't want to display the
blocks themselves nor most of those hvrules, they are all disabled by
default.  Therefore if using \code{subset=TRUE} when updating hvrules or
blocks, also set \code{setEnabled} to FALSE to avoid enabling and
displaying them all.

This function overrides graphical properties in \code{x} that may have
been set by a style.  Therefore the value of \code{style_row} is set to NA
for any elements whose graphical properties are updated by this function.
}
\seealso{
\code{\link{element_entry}}, \code{\link{element_refmark}},
\code{\link{element_hvrule}}, \code{\link{element_block}},
\code{\link{elements}}, \code{\link{props<-}}, \code{\link{propsa<-}}
}
\examples{
ttbl <- textTable(iris2_tab, title="The iris data", 
                  subtitle=c("Summary statistics by species"), 
                  foot="sd = standard deviation")
plt <- plot(ttbl)
propsd(plt, subset=(enabled)) <- element_hvrule(color="red")
propsd(plt, subset=(part == "colhead" & headlayer == 1)) <- 
  element_entry(angle=90, hjust=0.5, vjust=0.5)
plt
  }
