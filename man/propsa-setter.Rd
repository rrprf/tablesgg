% Auto-generated documentation for function propsa<- 
%  2020-11-29 11:21:55 
\name{propsa<-}
\alias{propsa<-}
\title{Update Graphical Properties for Selected Table Elements }
\description{
Update the graphical properties of table elements in selected rows and
columns of a plotted table.
}
\usage{
propsa(x, arows=NULL, acols=NULL, setEnabled=TRUE) <- value
}
\arguments{
  \item{x}{A \code{pltdTable} object containing a plotted table.
  }
  \item{arows, acols}{Numeric vectors of row and column numbers in the table's augmented
row-column grid.  The default value of NULL means all rows or columns.
Use half-integer values to refer to the locations of horizontal or
vertical rules running between rows or columns.
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
to be modified.  \code{props<-} uses element or block ID, or searches the
text content of entries.  \code{propsa<-} uses explicit row and column
numbers within the augmented row-column grid.  \code{propsd<-} uses the
values of element descriptors (as described in \code{?elements}).

The type of elements that are updated is determined by \code{value}: if
\code{value} is an \code{element_entry} or \code{element_refmark} object
then entries are updated; if it is an \code{element_hvrule} object then
hvrules are; if it is an \code{element_block} object then blocks are.  See
the documentation for \code{props<-} for discussion of their use.

For this function the selected elements are those which are (a) completely
contained within the rows listed in \code{arows} and the columns listed in
\code{acols}; and (b) of a type (entry, block, or hvrule) corresponding to
the class of \code{value}.  For (a), \code{arows} and \code{acols} are
treated as _sets_ of row/column numbers, not _ranges_.  For example, a
horizontal rule running between rows 3 and 4 will not be selected by
\code{arows=c(3, 4)}; the value 3.5 must be included in \code{arows}.

The helper functions \code{arow} and \code{acol} provide a partial bridge
between this function and \code{props<-}.  They return row and column
numbers associated with particular element IDs, or with specified values
of the row and column headers.  For example, \verb{propsa(plt,
arows=arow(plt, id="body"), acols=acol(plt, id="body"))} is equivalent to
\verb{props(plt, id="body")}.  See the examples below for cases where the
combination of \code{propsa<-} and \code{arow}/\code{acol} is more
convenient than \code{props<-} alone.

This function overrides graphical properties in \code{x} that may have
been set by a style.  Therefore the value of \code{style_row} is set to NA
for any elements whose properties are updated by this function.
}
\seealso{
\code{\link{element_entry}}, \code{\link{element_refmark}},
\code{\link{element_hvrule}}, \code{\link{element_block}},
\code{\link{arow}}, \code{\link{acol}}, \code{\link{props<-}},
\code{\link{propsd<-}}
}
\examples{
plt <- plot(iris2_tab, title="Summary statistics for the iris data")

plt2 <- plt
# Change the title to italics:
propsa(plt2, arows=1) <- element_entry(fontface=3)
# Change the vertical rule between row header and body from empty to 
# a solid line:
propsa(plt2, acols=2.5) <- element_hvrule(linetype=1, color="black", 
                                          size=0.5)
# Put all the mean values in bold face:
propsa(plt2, arows=arow(plt2, hpath=c(NA, "mean")), 
       acols=acol(plt2, id="body")) <- element_entry(fontface=2)

plt3 <- plt
# Use shading to highlight the rows for the 'versicolor' species:
propsa(plt3, arows=arow(plt3, hpath=c("versicolor"))) <- 
       element_block(fill="gray85")
# Compare tables before and after modification:
print(plt, vpx=0.25, vpy=0.75)
print(plt2, vpx=0.75, vpy=0.75, newpage=FALSE)
print(plt3, vpx=0.5, vpy=0.25, newpage=FALSE)

# Striping every other row in a data frame listing (must include row names):
data(mtcars, package="datasets")
plt <- plot(textTable(head(mtcars, 10), 
                      title="Partial listing of the 'mtcars' data frame"))
ar <- arow(plt, id="body")
propsa(plt, arows=ar[ar \%\% 2 == 0]) <- element_block(fill="gray85")
plt
  }
