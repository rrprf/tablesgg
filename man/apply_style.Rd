% Auto-generated documentation for function apply_style 
%  2021-06-02 11:12:18 
\name{apply_style}
\alias{apply_style}
\title{Apply a Style to Table Elements }
\description{
Apply a style to assign graphical properties to table elements.  This is
an internal function not intended to be called by package users.
}
\usage{
apply_style(x, style, replace, setEnabled, unstyled, base_style)
}
\arguments{
  \item{x}{A data frame containing table elements (entries, hvrules, or blocks).
  }
  \item{style}{A \code{styleObj} object, containing a style appropriate for the type of
elements in \code{x}.
  }
  \item{replace}{If FALSE only graphical properties not already present as columns in
\code{x} are added; existing columns are not changed.  If NA, any NA
values in existing graphical property columns are replaced with values
from \code{style} (along with any missing columns).  If TRUE then existing
columns will be entirely replaced.
  }
  \item{setEnabled}{If TRUE then column \code{enabled} is set to TRUE for elements whose
graphical properties are set (or would be set, if \code{replace} were
TRUE) entirely or in part by \code{style}; i.e., elements that match a
style row.  \code{enabled} is not changed for unmatched elements.  If
FALSE, \code{enabled} is not changed for any elements.
  }
  \item{unstyled}{Character string indicating how to handle elements for which \code{style}
does not provide graphical parameters (i.e., elements that do not match
any style row).  One of "pass", "disable", "base", or "error".
  }
  \item{base_style}{A \code{styleObj} object that will be applied to unstyled elements when
\code{unstyled="base"}.  This style should be guaranteed to match every
element; if it does not an error will be raised.
  }
}
\value{
A data frame.  It has one row per table element and the same columns as
\code{x}, plus additional or updated columns containing the graphical
properties to use for each element, plus a column \code{style_row} (the
number of the last row in \code{style} that the entry matched, or NA if it
matched none).
}
\details{
Table elements include entries, blocks, and hvrules.  If \code{x} contains
table entries, the \code{element_type} for \code{style} should be "entry".
If \code{x} contains table blocks, \code{element_type} may be either
"block" or "hvrule".  In the latter case a set of hvrules will be
generated, one for each of the four sides of each block.  Then
\code{style} will be used to assign graphical properties to those hvrules.

The graphical properties available for an element, and their types, are
defined in function \code{grSpecs} and documented in \code{?elements}.

See \code{styleObj} for information about how a style is specified, and
how it is applied to elements.  Note that individual style specifications
(rows) in \code{style} are applied to \code{x} in order.  If an element
matches more than one \code{style} row, the last matching row will
override the earlier ones.

NA values after evaluating \code{condition} expressions in a style:

\code{styleObj} objects contain contain character fields that are
evaluated as expressions with respect to columns of \code{x}.  The fields
are called \code{condition} for entry and block styles, and
\code{block_condition} and \code{adjacent_condition} for hvrule styles.
Each expression must evaluate to a logical scalar or vector with one value
per row of \code{x}.  If an expression evaluates to NA for a row of
\code{x} it is treated as FALSE for that row--the reason to not throw an
error is to make it easier to manually add blocks or hvrules by position
only.  (Note that this is different from the case where the condition
expression itself, before evaluation, is NA.  The latter is treated as
evaluating to TRUE for all rows of \code{x}.)
}
\seealso{
\code{\link{styleObj}}; \code{\link{grSpecs}}, \code{\link{apply_scale}}
}
\keyword{internal}
