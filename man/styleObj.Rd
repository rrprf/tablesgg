% Auto-generated documentation for function styleObj 
%  2020-11-29 11:21:55 
\name{styleObj}
\alias{styleObj}
\title{Create a Style Object }
\description{
Create a style object that can be used to assign graphical properties to
table elements (entries, hvrules, or blocks).  The properties are used
when displaying the elements.  Styles allow assignment of graphical
properties to be based on element descriptors.
}
\usage{
styleObj(x, type, scalable_properties, match_columns=character(0))
}
\arguments{
  \item{x}{Data frame with (a) column(s) that specify patterns to be used to identify
and select a subset of table elements; and (b) columns specifying the
graphical properties to be used for elements in the selected subset.  See
DETAILS.  May also be a string with the path to a \code{.csv} file that
will be read to create such a data frame.
  }
  \item{type}{Character string, one of "entry", "hvrule", or "block".  This specifies
the type of element to which the style will apply.
  }
  \item{scalable_properties}{Optional character vector (possibly empty) specifying the graphical
properties that are to be modified when changing the displayed size of a
table by a scale factor.  If not provided, a set of default properties
(depending on \code{type}) will be used.
  }
  \item{match_columns}{Optional character vector (possibly empty) with the names of element
descriptors that are required in order to evaluate the subset selector
expressions in \code{x}.  See DETAILS.
  }
}
\value{
An object of S3 classes \code{styleObj} and \code{data.frame}.  It will
have the same number of rows and all the columns in \code{x}.

The object will have attributes \code{element_type},
\code{scalable_properties} and \code{match_columns}, equal to the
corresponding arguments.
}
\details{
A style specifies graphical properties to be used in displaying one of the
element types in a table (i.e., entries, hvrules, or blocks).  A style is
similar to a theme in \code{ggplot2} in that it can be applied to any
table, not just a particular table.

A \code{styleObj} object is a data frame.  Each row can be thought of as a
pattern plus a set of graphical properties.  Table elements that are to be
styled are compared to the patterns.  If the pattern in a style row
matches a table element, the graphical properties in that row are assigned
to the element.  If more than one style row matches an element, the
properties from the last matching row override the earlier ones.

The graphical property columns that must be present in a \code{styleObj}
data frame are described in \code{?elements}.

Specification of style patterns and how they are matched to elements is
similar for table entries and blocks, and is described first.  The process
for hvrules is more complicated and is described second.  It will be
easier to follow the descriptions if one also looks at an example, such as
\verb{View(styles_pkg$entryStyle_pkg_1)} and
\verb{View(styles_pkg$hvruleStyle_pkg_1)}.

\emph{Style specification and matching: Entry and block styles}

First note that table entries and blocks internally are stored in objects
that are themselves data frames, with one row per element.  (These data
frames can be accessed using the \code{elements} function.)  Columns
include element descriptors such as the table part associated with the
element, its position in the table, whether the element spans multiple
rows or columns, and other information.  See \code{?elements} for lists of
the standard descriptors.

In styles for table entries and blocks, the pattern part of the
\code{styleObj} object consists of a single column named \code{condition}.
\code{condition} should contain character strings that can be interpreted
as expressions involving the element descriptors mentioned in the previous
paragraph.  Each \code{condition} expression, when evaluated within an
\code{entries} or \code{blocks} data frame, should produce a logical
vector with one value per element.  (Vectors of length 1 are recycled to
the necessary length.)  Examples of such strings are \verb{part ==
"rowhead" & multirow} for entries and \verb{type == "colblock" & subtype
== "A" & headlayer > 1} for blocks.

Elements for which the \code{condition} expression in a style row
evaluates to TRUE are considered to match that row of the style, and are
assigned the graphical properties in that row.

An NA value (or equivalently an empty string) as a style row's
\code{condition} is treated specially: it matches _any_ element.  The
row's graphical properties will be applied to all elements, unless
overridden by a later style row.

\emph{Style specification and matching: hvrule styles}

The creation and styling of hvrules is closely tied to table blocks: by
default, four hvrules are created for each block, one running along each
side.  (They are initially disabled.)  Style specification for hvrules is
more complicated than for table blocks because hvrules effectively
*separate* blocks.  Therefore one may want their appearance to depend on
characteristics of the blocks on *both* sides of the hvrule.

Similar to entries and blocks, hvrules are represented internally as a
data frame with one row per hvrule.  Columns include: \code{block}, the ID
of the block that generated the hvrule; \code{side}, the side of
\code{block} along which the hvrule runs ("top", "bottom", "left", or
"right"); and \code{adjacent_blocks}, a string listing the ID's of all the
blocks adjacent to \code{block} on the same side as the hvrule.  That is,
the hvrule separates \code{block} and the blocks in
\code{adjacent_blocks}.  Note that \code{adjacent_blocks} may be empty.

In styles for hvrules, the pattern part of the \code{styleObj} object
consists of three columns: \code{block_condition}, \code{side}, and
\code{adjacent_condition}. \code{side} is one of "top", "bottom", "left"
or "right".  \code{block_condition} and \code{adjacent_condition} are like
the \code{condition} column for block styles: they should contain
character strings that can be interpreted as expressions involving block
descriptors.  Each expression will be evaluated within the data frame of
blocks that generated the hvrules.  (Not the data frame containing the
hvrules themselves.)  It should produce a logical vector with one element
per block; if the value is TRUE for a block, the block satisfies that
expression.

An hvrule matches a given style row if (a) its generating block satisfies
the style row's \code{block_condition}; (b) they have the same value of
\code{side}; and (c) one or more of the hvrule's \code{adjacent_blocks}
satisfies the style row's \code{adjacent_condition}.

Any of \code{block_condition}, \code{side}, and \code{adjacent_condition}
in a style row may also be set to NA (or equivalently, to an empty
string).  In that case the corresponding criterion (a), (b), or (c) is
considered to be satisfied for all hvrules, and so does not limit matches.
Note that setting \code{adjacent_condition} to NA is the only way to
satisfy criterion (c) if an hvrule's \code{adjacent_blocks} is empty.  In
all other cases, an empty \code{adjacent_blocks} will never satisfy
criterion (c).
}
\seealso{
\code{\link{styles_pkg}} contains predefined styles provided by the
package.  They can be examined as illustrations of how styles are
specified, or edited to create new styles.  \code{\link{elements}} lists
the descriptors and graphical properties available for each element type.
}
\examples{
# Built-in default styles:
if (interactive()) {
  View(styles_pkg$entryStyle_pkg_1)
  View(styles_pkg$blockStyle_pkg_1)
  View(styles_pkg$hvruleStyle_pkg_1)
}
  }
