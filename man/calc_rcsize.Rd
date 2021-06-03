% Auto-generated documentation for function calc_rcsize 
%  2021-06-02 11:12:18 
\name{calc_rcsize}
\alias{calc_rcsize}
\title{Constrained Display Sizes of Each Table Row, Column and Entry }
\description{
Calculate the display widths (in mm) of each table column, heights of each
table row, and horizontal and vertical dimensions of each table entry.
Calculations may use the constraints imposed by \code{minwidth} and
\code{maxwidth} for each entry.  This is an internal utility function, not
intended to be called by package users.
}
\usage{
calc_rcsize(entryInfo, vrule_hsize, hrule_vsize, sizeAdjust, 
    nominal_rcmin, method)
}
\arguments{
  \item{entryInfo}{A \code{prEntries} data frame with one row per table entry.  (Currently
disabled entries are not allowed.)
  }
  \item{vrule_hsize}{Numeric vector with length (# table columns) + 1, giving the horizontal
dimensions, in mm, of vertical rules in the table.
  }
  \item{hrule_vsize}{Numeric vector with length (# table rows) + 1, giving the vertical
dimensions, in mm, of horizontal rules in the table.
  }
  \item{sizeAdjust}{Two-element numeric vector containing multipliers to adjust the calculated
absolute size of table text, when determining row and column sizes.
Essentially fudge factors.  The first element is applied to the horizontal
dimension and the second to the vertical dimension.
  }
  \item{nominal_rcmin}{A nominal minimum size for any row or column, in mm.  It is used to avoid
division by 0 when calculating the proportional change in size due to
constraints, when \code{method} is 2-5.  Large size adjustments are
disfavored for rows or columns whose actual size is less than this value.
  }
  \item{method}{Which algorithm/objective function is used for the constrained
optimization problem. \itemize{ \item 0 : \code{minwidth}, \code{maxwidth}
constraints on entries are ignored; no wrapping. \item 1 : \code{minwidth}
constraints are respected but \code{maxwidth} is ignored; no wrapping.
\item 2 : Proportional size changes are calculated with \code{max(<natural
width>, nominal_rcmin)} replacing \code{<natural width>} in the
denominator. \item 3 : Proportional size changes are calculated with
\code{max(<natural width>, nominal_rcmin)} replacing \code{<natural
width>} in both the numerator and denominator. \item 4 or 5 : Same as 2 or
3 but with the sum \code{<natural width> + nominal_rcmin} instead of their
max. }
  }
}
\value{
List with components:
  \item{hsizeCol}{Numeric vector of display widths for each table column, in mm.
  }
  \item{vsizeRow}{Numeric vector of display heights for each table row, in mm.
  }
  \item{sizeInfo}{Data frame with one row per table entry.  Columns are \code{hsize} and
\code{vsize} (the horizontal and vertical dimensions of entry text, as it
would be displayed by \code{geom_text}, \code{geom_richtext}, or
\code{geom_textbox} after applying any min or max contraints);
\code{horiz} (logical, TRUE if entry angle is 0 or 180 degrees);
\code{descender} (amount of space included in \code{vsize} (if
\code{horiz} is TRUE) or \code{hsize} (if \code{horiz} is FALSE) to allow
for descenders); and \code{wrap} (logical; TRUE if text wrapping is needed
to fit the text into a box with dimensions \code{hsize} and \code{vsize}).
Row names will be the entry ID's.  \code{hsize}, \code{vsize}, and
\code{descender} do not include the effect of \code{sizeAdjust}, and do
not include any padding specified by \code{hpad} or \code{vpad}.  All
columns will contain NA for disabled entries.  (Not
implemented--currently, disabled entries are not allowed.)
  }
}
\details{
Entries can span multiple columns and rows, and can have constraints on
their minimum and maximum widths.  This function calculates row and column
dimensions that satisfy the constraints while minimizing an objective
function that measures deviations from their natural sizes.  They are the
solution to a quadratic programming problem.  An error will be raised if
the QP has no solution (e.g., if \code{minwidth} and \code{maxwidth}
collectively, over all entries, are inconsistent).

\code{minwidth} and \code{maxwidth} are in the text's frame of reference,
and may be expressed in two forms.  Positive values are interpreted as
absolute widths in millimeters, and *should* include the amount of any
padding specified by \code{hpad} (when \code{angle} is 0 or 180 degrees)
or \code{vpad} (when \code{angle} is 90 or 270 degrees), as well as
scaling by \code{sizeAdjust}.  Negative values are interpreted as
multiples of the natural text width without wrapping and *without*
including padding or \code{sizeAdjust}.  Thus setting \code{minwidth} for
an entry to -1 will guarantee that the width of the spanned cell(s) will
be at least enough to contain it without wrapping. \itemize{ \item An NA
value for \code{minwidth} means there is no constraint on minimum width,
and is equivalent to 0.  An \code{Inf} value for \code{maxwidth} means
there is no constraint on maximum width.  In the absence of constraints,
the internal algorithm favors widths as close as possible to the natural
width of the entry text, without wrapping. \item An NA value for
\code{maxwidth} means the maximum width will be determined passively from
the \code{maxwidth} values of other entries in the same table column(s)
(if \code{angle} is 0 or 180) or row(s) (if \code{angle} is 90 or 270).
(It will never be less than \code{minwidth}.) This may be useful for table
titles and footnotes, where long text should be wrapped to fit widths
implied by the other table entries. \item Setting \code{maxwidth} to NA or
to a finite value \code{> -1} and \code{< Inf} means the spanned cell(s)
may not be wide enough to hold the text without wrapping it into multiple
lines. \item The general effect of setting \code{minwidth} to a non-zero
value is to reduce or prevent text wrapping, while the general effect of
setting \code{maxwidth} to NA or a finite value is to encourage wrapping.
Settings for one entry may affect the width and wrapping of other entries,
because column widths and row heights for the table as a whole must
satisfy the constraints for all their entries. \item Text representing
\code{plotmath} expressions cannot be wrapped, so \code{maxwidth} should
be \code{Inf} or \code{<= -1} for such entries. }

\code{method=0} ignores \code{minwidth} and \code{maxwidth} when
determining row and column sizes.  However a warning will be raised if the
resulting sizes violate the constraints for any enabled entries.
}
\note{
Operation of this function is controlled by parameters in internal object
\code{internalOpt}, specifically those with prefix \code{rcsize_}.  Values
of option \code{rcsize_verbose} greater than 2 will cause a data frame of
calculated entry widths to be written to the global environment for each
iteration.

Among methods 2-5, 2 and 4 seem best because they do not alter the target
size in the numerator; method 5 encourages expanding row *and* column
sizes by \code{rcsize_rcmin} even when no wrapping is required in a given
direction.
}
\seealso{
\code{\link{entrySize_mm}}, called by this function to calculate the sizes
in \code{sizeInfo}.
}
\keyword{internal}
