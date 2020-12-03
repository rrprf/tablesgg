% Auto-generated documentation for function addBlock 
%  2020-11-29 11:21:55 
\name{addBlock}
\alias{addBlock}
\title{Define a New Block of Cells in a Table }
\description{
Define a new block (rectangular set of cells) in a table.  The location
and graphical properties of the block are specified explicitly, rather
than being generated automatically from the logical structure of the table
and a style.
}
\usage{
addBlock(x, arows, acols, id, props=NULL, enabled=FALSE)
}
\arguments{
  \item{x}{A plotted table (\code{pltdTable}) object.
  }
  \item{arows, acols}{Numeric vectors specifying the cells contained in the block, with respect
to the augmented row-column grid of the table.  The block includes the
cells in row numbers from \code{min(arows)} to \code{max(arows)}, and
column numbers from \code{min(acols)} to \code{max(acols)}.
  }
  \item{id}{Optional character string giving the ID to be assigned to the new block.
It is an error if there is already a block with this ID in \code{x}.  The
default is to generate an ID of the form \code{block*}, where \code{*} is
an integer.
  }
  \item{props}{Optional \code{element_block} object with graphical properties to assign
to the new block.  Any graphical properties not specified in \code{props}
will be taken from \code{blockStyle_pkg_base} in \code{styles_pkg}.
  }
  \item{enabled}{Logical scalar, whether the new block is to be enabled for display.  The
default is FALSE.
  }
}
\value{
A plotted table object like \code{x}, with the new block defined.
}
\details{
Normally blocks are defined automatically, based on the logical structure
of the table and the style selected by the user.  This function allows
additional blocks to be defined "manually", explicitly specifying their
position and span in terms of row and column numbers.

There are two typical situations in which one would want to define a new
block.  The first is to highlight a specific set of cells in the table
visually, by shading or a border.  For that purpose one should specify
\code{enabled=TRUE} (so the block will be displayed) and perhaps
\code{props} (for non-default graphical properties).

The second reason to define a new block is to use its ID as a quick way to
refer to the entries within it, for example to set their graphical
properties using \code{props<-}.  In that case \code{enabled} for the
block should be FALSE, since the block itself is not to be displayed.

Row and column numbers are with respect to the augmented row-column grid
of the table.  See \code{?adim} for more more information about this grid.
The helper functions \code{arow} and \code{acol} can be used to specify
arguments \code{arows} and \code{acols} in terms of table parts or
previously defined blocks.

Graphical properties for blocks defined by this function will not be
changed if a new block style is applied to the plotted table.  Use one of
the \code{props<-} functions instead.

There is no way to remove or undefine a block, other than recreating the
plotted table object from scratch.  However they can be disabled using a
\code{props<-} function, and then will not be displayed.
}
\seealso{
\code{\link{arow}}, \code{\link{acol}}, \code{\link{adim}},
\code{\link{element_block}}
}
\examples{
plt <- plot(iris2_tab, title="The iris data", 
            subtitle="Summary statistics by species")
plt <- addBlock(plt, arows=c(8, 9), acols=c(3, 4), id="new_block", 
                props=element_block(border_color="red", border_size=1.0), 
                enabled=TRUE)
plt
# Can refer to the new block by its ID:
props(plt, id="new_block") <- element_entry(fontface=3)  # italics
plt
  }
