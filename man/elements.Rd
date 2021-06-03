% Auto-generated documentation for function elements 
%  2021-06-02 11:12:18 
\name{elements}
\alias{elements}
\title{Extract Table Elements from a Plotted Table }
\description{
Extract table elements (entries, blocks, or hvrules) from a plotted table,
as a simple data frame with one row per element.
}
\usage{
elements(x, type=c("entry", "block", "hvrule"), enabledOnly=TRUE)
}
\arguments{
  \item{x}{A \code{pltdTable} object, containing a plotted table.
  }
  \item{type}{Character scalar indicating the type of elements to extract: one of
"entry", "block", or "hvrule".
  }
  \item{enabledOnly}{Logical scalar.  If TRUE, only elements that are currently enabled in
\code{x} are returned.
  }
}
\value{
A data frame with one row per element.  Columns include element
descriptors and graphical properties assigned to each element.  The row
names of the data frame will be the element ID's.
}
\details{
A plotted table (\code{pltdTable} object) has three types of elements:
entries, blocks, and hvrules.  \emph{Entries} are the text strings (and
associated properties) displayed in table cells.  \emph{Blocks} are
rectangular sets of contiguous table cells.  And \emph{hvrules} are
spacers, with or without a visible line (or "rule"), used to separate or
group table rows and columns.  See the package vignette for more
information.

This function allows one to inspect these elements.  The purpose is
primarily informational: an easy way to view all the elements of a table,
their descriptors (e.g., as used by styles and the \code{propsd<-}
function), and the graphical properties they have been assigned.  It is
not intended as a way to edit or modify elements.  For that, see
\code{update} methods for \code{textTable} and \code{pltdTable} objects,
the \code{props<-} set of functions, and section 4 of the package
vignette.

The remainder of this section describes the columns in the returned data
frame, for each element type.

\strong{Columns in \code{elements(x, type="entry")}}

\emph{Entry descriptors:}

\describe{ \item{id}{Character string that uniquely identifies the entry.
The format is \code{<part>,<partrow>,<partcol>} for table parts that are
matrices (\code{rowhead}, \code{rowheadLabels}, \code{colhead}, and
\code{body}), and \code{<part>,<partrow>} for table parts that are vectors
(annotation parts \code{title}, \code{subtitle}, and \code{foot}).}
\item{part}{Character string identifying the part of the table to which
the entry belongs: one of "body", "rowhead", "colhead", "rowheadLabels",
"title", "subtitle", or "foot".} \item{subpart}{Character string with
further information about the nature of of the entry within its part of
the table.  May be NA.} \item{partrow, partcol}{Row number, column number
of the entry within its table part.  For parts that are vectors rather
than matrices, \code{partrow} will be the element number within the
vector, and \code{partcol} will be NA.  If an entry spans more than one
row or column, the minimum row/column number of the spanned cells is
used.} \item{headlayer}{How far (in number of rows or columns) from the
body of the table the entry is.  By definition this is 0 for entries in
the body of the table.  It is 1 for row/column headers immediately
adjacent to the body, 2 for headers one row/column further out, and so on.
The layer numbers for row header labels match those for the corresponding
columns of row headers.  The layers for titles, subtitles, and foot lines
are the number of _rows_ from the body of the table.  When a table is
created with \code{rowheadInside} set to TRUE, the \code{headlayer} value
for the outermost layer of row headers (and for its label, if any) is
changed to 0, since the headers become interleaved with the table body.}
\item{level_in_layer}{Numbering of entries within a given part and
headlayer.  For row and column headers this is based their hierarchical
structure (see the descriptors for blocks, below).  For other table parts
it is just an integer from 1 to the number of entries in that layer of
that part.} \item{multirow, multicolumn}{Logicals indicating whether the
entry spans multiple rows or columns of the table.} \item{text}{Character
string containing the formatted content of the entry, for display.  It may
use \code{plotmath} notation for mathematical notation, or markdown/HTML
tags; see \code{textspec} below.} \item{type}{Character string identifying
the type of value the entry represents (e.g., "numeric").  May be NA.  The
default for table annotation (\code{title}, \code{subtitle}, \code{foot})
and \code{rowheadLabels} is "character".} \item{textspec}{Character string
indicating any special features of the text in \code{text}.  Currently
supported values are "plain", "plotmath", and "markdown".  "plotmath"
indicates that the entry text contains mathematical notation as described
in \code{?plotmath}.  "markdown" means the text may contain markdown or
HTML tags to control its appearance; this requires the \code{ggtext}
package.} \item{enabled}{Logical indicating whether the entry is to be
displayed when the table is plotted.  If FALSE the entry is ignored when
laying out the table or determining its size.} \item{arow1, arow2, acol1,
acol2}{Range of row and column numbers occupied by an entry, with respect
to the augmented row-column grid for the table.  \code{arow1} <
\code{arow2} and/or \code{acol1} < \code{acol2} means the entry spans
multiple rows and/or columns.} }

\emph{Graphical properties for entries}.  Values for these are assigned by
a \emph{style}, either a default style (see \code{?tablesggOpt}) or
user-specified.

\describe{ \item{hjust}{Numeric horizontal justification for entry text
(0=left, 0.5=center, 1=right).} \item{vjust}{Numeric vertical
justification for entry text (0=top, 0.5=center, 1=bottom).}
\item{color}{Character string; color for entry text.} \item{alpha}{Numeric
value in [0, 1] specifying transparency of entry text (0=transparent,
1=opaque).} \item{size}{Font size for entry text, in points (72.27 points
= 1 inch, 2.845 points = 1 mm).} \item{family}{Character string, the font
to use for entry text.  "serif", "sans", and "mono" will work for all
graphics devices.  Other fonts may or may not be available on a particular
device.} \item{fontface}{Numeric indicating 1=plain, 2=bold, 3=italic,
4=bold and italic.} \item{lineheight}{Numeric multiplier that adjusts
interline spacing for multi-line entry text. 1.0 gives the default
spacing.} \item{angle}{Rotation angle for entry text, in degrees
counter-clockwise from horizontal.} \item{hpad, vpad}{Padding added around
the sides of entry text to keep it from touching cell borders, in
millimeters.  \code{hpad} is added on both the left and right sides of the
text, and \code{vpad} is added on both the top and bottom.}
\item{fill}{Character string; background color for the cell(s) containing
the entry.  NA means no background color.} \item{fill_alpha}{Numeric value
in [0, 1] specifying transparency of the cell background color
(0=transparent, 1=opaque).} \item{border_size}{Thickness of the border to
draw around the cell(s) containing the entry text, in millimeters.}
\item{border_color}{Character string; color for the border around entry
text.  NA means no border.} \item{minwidth, maxwidth}{Minimum and maximum
width for the cell(s) spanned by the entry.  (Here \emph{width} is with
respect to the text itself; i.e., the direction of reading for English
text, and therefore measured vertically if the text is rotated by 90 or
270 degrees.)  They may be expressed in two forms.  Positive values are
interpreted as absolute widths in millimeters, and should include the
amount of padding specified by \code{hpad} (when \code{angle} is 0 or 180
degrees) or \code{vpad} (when \code{angle} is 90 or 270 degrees).
Negative values are interpreted as multiples of the natural width of the
text itself, \emph{without} including padding.  Thus setting
\code{minwidth} for an entry to \code{-1} will guarantee that the width of
the spanned cell(s) will be at least enough to contain the text without
wrapping. \itemize{ \item An NA value for \code{minwidth} means there is
no constraint on minimum width, and is equivalent to 0.  An \code{Inf}
value for \code{maxwidth} means there is no constraint on maximum width.
(However in the absence of constraints, the internal algorithm favors
widths as close as possible to the natural width of the entry text,
without wrapping.) \item An NA value for \code{maxwidth} means the maximum
width will be determined passively from the \code{maxwidth} values of
other entries in the same table column(s) (if \code{angle} is 0 or 180) or
row(s) (if \code{angle} is 90 or 270).  (It will never be less than
\code{minwidth}.) This may be useful for table titles and footnotes, where
long text should be wrapped to fit widths implied by the other table
entries. \item Setting \code{maxwidth} to NA or to a finite value \code{>
-1} and \code{< Inf} means the spanned cell(s) may not be wide enough to
hold the text without wrapping it into multiple lines.  Therefore option
\code{tablesggOpt("allowWrap")} must be TRUE, and a warning will be raised
and \code{maxwidth} will be ignored if not. \item The general effect of
setting \code{minwidth} to a non-zero value is to reduce or prevent text
wrapping, while the general effect of setting \code{maxwidth} to NA or a
finite value is to encourage wrapping.  Settings for one entry may affect
the width and wrapping of other entries, because column widths and row
heights for the table as a whole must satisfy the constraints for all
their entries. \item Text representing \code{plotmath} expressions cannot
be wrapped, so \code{maxwidth} should be \code{Inf} or \code{<= -1} for
such entries. } } }

\strong{Columns in \code{elements(x, type="block")}}

\emph{Block descriptors:}

\describe{ \item{id}{Character string that uniquely identifies the block.
The format is just \code{<type>} for blocks types that are unique.  For
blocks associated with row or column headers, or with row groups formed by
the plot option \code{rowgroupSize}, ID's begin with
\code{<type>/<subtype>/<headlayer>/<level_in_layer>}.  See Appendix B of
the package vignette for details about the definitions and ID's of these
blocks.} \item{type}{Character string that specifies the nature or
structural role of the block.  One of \describe{ \item{"table"}{The whole
table (all cells).} \item{"title", "subtitle", "colhead", "rowhead",
"rowheadLabels", "body", "foot"}{ The standard table parts.  (If there are
interior row header entries, "rowhead" and "body" are omitted because the
interleaving of headers and body means neither are valid blocks.)}
\item{"titles"}{The union of the \code{title} and \code{subtitle} parts.}
\item{"stub"}{If we exclude the title/subtitle and foot annotations, a
table has four quadrants: the body at the lower right, the row headers at
the lower left, the column headers at the upper right, and a stub at the
upper left.  That is, the stub consists of the cells above the row headers
and to the left of the column headers.  (If there are row header
labels--block "rowheadLabels"--they will be in the bottom row of the
stub.)} \item{"colhead_and_stub", "rowhead_and_stub"}{The unions of "stub"
with "colhead" and "rowhead", respectively.} \item{"colhead_and_body",
"rowhead_and_body"}{The unions of "body" with "colhead" and "rowhead",
respectively.} \item{"rowblock", "colblock"}{Collections of blocks
associated with row and column headers, reflecting their hierarchical
structure.  "rowblock" is also the type for blocks representing row groups
formed by plot option \code{rowgroupSize}.  See Appendix B of the package
vignette for details.} } } \item{subtype}{Character string refining block
type.  For types "rowblock" and "colblock", subtypes are "A", "B", "C",
and (for "rowblock" only) "G".  See Appendix B of the package vignette for
their meaning.  For other block types the subtype is set to missing (NA).}
\item{headlayer}{For "rowblock" and "colblock" blocks, the header layer
number with which the block is associated.  (Layer numbers increase from
innermost to outermost layer.)  For other block types, \code{headlayer} is
NA.} \item{level_in_layer}{For "rowblock" and "colblock" blocks, the level
number (within the header layer) with which the block is associated.
Levels are numbered from 1 to the maximum number of levels in that layer.
For other block types, \code{level_in_layer} is NA.}
\item{group_in_level}{For "rowblock" blocks of subtype "G" (representing
sets of rows grouped according to \code{rowgroupSize}), the group number
within a header layer and level.  NA for other block types/subtypes.}
\item{had_enabled_entries}{Logical, set to TRUE if there were _enabled_
table entries in \code{x} that intersect the block.  This is set at the
time the plotted table (\code{pltdTable} object) is created.  It is not
updated if entries are later enabled/disabled using \code{props<-}, for
example.} \item{nr, nc}{The number of rows and columns, respectively, that
the block spans.  May be 0 for empty blocks.} \item{arow1, arow2, acol1,
acol2}{First and last row and column numbers spanned by the block, with
respect to the augmented row-column grid for the table.  Empty blocks,
with no rows or no columns (\code{nr} or \code{nc} equal to 0), will have
the corresponding \code{arow*} or \code{acol*} set to NA.}
\item{enabled}{Logical indicating whether the block is to be displayed
when the table is plotted.  This applies only to highlighting the
rectangular region occupied by the block using a fill color or border.  It
has no effect on display of entries or hvrules contained within the
block.} }

\emph{Graphical properties for blocks}.  Values for these are assigned by
a \emph{style}, either a default style (see \code{?tablesggOpt}) or
user-specified.

\describe{ \item{fill}{Character string; color used to fill the
rectangular region contained in the block.  NA means the region is not
colored.  (Blocks are drawn before table entries or hvrules, so the block
fill color will not hide those elements even if it is opaque.)}
\item{fill_alpha}{Numeric value in [0, 1] specifying transparency of the
block fill color (0=transparent, 1=opaque).} \item{border_size}{Thickness
of the border to draw around the block, in millimeters.}
\item{border_color}{Character string; color for the border around the
block.  NA means no border.} }

\strong{Columns in \code{elements(x, type="hvrules")}}

\emph{hvrule descriptors:}

\describe{ \item{id}{Character string that uniquely identifies the hvrule.
The format is \code{<block>_<side>}.} \item{direction}{Character string,
either "hrule" for a horizontal rule or "vrule" for a vertical rule.}
\item{block}{The ID of the block along the side of which the hvrule runs.}
\item{side}{Which side of the block the hvrule runs along, "top",
"bottom", "left", or "right".} \item{adjacent_blocks}{Character string
containing the IDs of blocks that are adjacent to \code{block} on the same
side as the hvrule.  (I.e., blocks that are separated from \code{block} by
the hvrule.)  Block IDs within the string are separated by semicolons.  If
there are no adjacent blocks the string will be empty ("").} \item{arow1,
arow2, acol1, acol2}{Location of the hvrule with respect to the augmented
row-column grid of the table.  An hrule is inserted between table rows,
and therefore \code{arow1} and \code{arow2} are the same and equal to a
half-integer.  For example, an hrule inserted between rows 3 and 4 has
\code{arow1 = arow2 = 3.5}.  \code{acol1} and \code{acol2} for the hrule
are integers indicating the range of columns that it spans.  Analogously,
a vrule is inserted between table columns, so \code{acol1} and
\code{acol2} are identical and equal to a half-integer, while \code{arow1}
and \code{arow2} are integers that indicate the range of rows spanned by
the vrule.} \item{enabled}{Logical indicating whether the hvrule is to be
displayed when the table is plotted.  If FALSE the hvrule is ignored when
laying out the table or determining its size.} }

\emph{Graphical properties for hvrules}.  Values for these are assigned by
a \emph{style}, either a default style (see \code{?tablesggOpt}) or
user-specified.

\describe{ \item{linetype}{Integer indicating the type of line to display.
1 = solid line; 2 = dashed; 3 = dotted.  (See the documentation of
\code{lty} in \code{?par} for the full set of choices.)  A line type of 0
means no line will be drawn, so the hvrule just inserts empty space
between table rows or columns.} \item{size}{Thickness of the line, in
millimeters.} \item{color}{Character string; the color of the line.}
\item{alpha}{Numeric value in [0, 1] specifying transparency of the line
color (0=transparent, 1=opaque).} \item{space}{The width (for a vertical
rule) or height (for a horizontal rule) of the rectangle inserted between
columns or rows by the hvrule.  (A line, if any, is drawn within this
rectangle.)  This is the amount of space the rule adds to the width or
height of the table, in millimeters.} \item{fill}{Character string; color
used to fill the rectangle containing the hvrule.  NA means the region is
not colored.} \item{fill_alpha}{Numeric value in [0, 1] specifying
transparency of the fill color (0=transparent, 1=opaque).} }
}
\seealso{
\code{\link{tablesggOpt}}, \code{\link{styleObj}}, \code{\link{ids}}
}
\examples{
plt <- plot(iris2_tab)
str(elements(plt, type="entry"))
str(elements(plt, type="block"))  # 0 rows, none are enabled for display
str(elements(plt, type="block", enabledOnly=FALSE))
str(elements(plt, type="hvrule"))
  }
