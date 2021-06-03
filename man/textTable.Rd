% Auto-generated documentation for function textTable 
%  2021-06-02 11:12:19 
\name{textTable}
\alias{textTable}
\title{Create a Structure Representing a 2D Table }
\description{
Create a structure representing the content and organization of a 2D
table: table body, row and column headers, and annotation.  All table
cells are formatted as character strings.  This is an S3 generic.
}
\usage{
textTable(x, ...)
}
\arguments{
  \item{x}{Object to be formatted as a 2D table.
  }
  \item{...}{Additional arguments passed to specific methods.
  }
}
\value{
An object with S3 class \code{textTable}.  This is a list with components:
  \item{body}{Character matrix containing the body of the table.
  }
  \item{rowhead}{Character matrix with the same number of rows as the table body,
containing row headers for the table.  Row headers are displayed as a set
of columns to the left of the table body.  May be empty (0 columns).
  }
  \item{rowheadLabels}{Character matrix with as many columns as \code{rowhead} and at most one
row, specifying labels for the \code{rowhead} columns.  May be empty (0
rows).
  }
  \item{colhead}{Character matrix with the same number of columns as the table body,
containing column headers for the table.  Column headers are displayed as
a set of rows above the table body.  If \code{rowheadLabels} are present,
\code{colhead} must have at least one row, but otherwise it may be empty
(0 rows).
  }
  \item{title, subtitle, foot}{Character vectors providing annotation for the table.  May be empty
(length 0).
  }
  \item{partdim}{Numeric matrix with one row per table part (i.e., the components listed
above), and columns:
  \describe{
    \item{nr, nc:}{Number of rows, columns in the part (\code{nc} equal to NA for annotation
parts).
    }
    \item{arow1, arow2, acol1, acol2:}{First and last rows, first and last columns occupied by the part within
the table's augmented row-column grid.  \code{arow*} should be NA if
\code{nr} is 0, \code{acol*} should be NA if \code{nc} is 0.
    }
  }
  }
  \item{rowhier, colhier}{Lists describing the hierarchical structure of row and column headers,
respectively.  Each list has one component per header layer (column of
\code{rowhead}, row of \code{colhead}), in order from outermost layer to
innermost.  In turn, each of these components is a data frame with one row
per node in the hierarchy at that layer.
  }
Components \code{body}, \code{rowhead}, and \code{colhead} will each have
an attribute \code{type}.  For \code{body} this will be a character matrix
with the same dimensions, containing an arbitrary string describing the
type of value represented in each cell (e.g., "numeric"), or NA.  For
\code{rowhead} and \code{colhead}, it will be a character vector with
length equal to the number of layers of headers (i.e., number of columns
in \code{rowhead}, number of rows in \code{colhead}), again containing a
string describing the type of values in each layer, or NA.

Components \code{body}, \code{rowhead}, \code{rowheadLabels},
\code{colhead}, \code{title}, \code{subtitle}, and \code{foot} will each
have an attribute \code{justification}.  It will be a character matrix or
vector of the same size and shape as the component.  Values "l", "c", "r"
specify left, centered, and right horizontal justification of text,
respectively, for the corresponding table entry.  Value NA means that the
type of justification is not specified--it will be set by the entry style
used when plotting the table.
}
\details{
\code{textTable} objects are the fundamental structure used to represent
table _content_ and _organization_ in the \code{tablesgg} package.

Components \code{body}, \code{rowhead}, \code{rowheadLabels},
\code{colhead}, \code{title}, \code{subtitle}, and \code{foot} correspond
to the table _parts_ with those names.  Empty parts should be of type
character: either a 0-length vector or a matrix with one or both
dimensions equal to 0, depending on the component.

Character strings representing table content may be prefixed with either
"MATH_" or "MKDN_".  The former indicates the string is to be interpreted
as a \code{plotmath} expression, the latter that the string contains
markdown or HTML tags.

Components \code{partdim}, \code{rowhier}, and \code{colhier} are
automatically derived from the other components whenever a
\code{textTable} is created or updated.

See Appendix A of the package vignette for more information about writing
\code{textTable} methods.
}
\seealso{
Specific methods for creating textTable's from other objects.
}
