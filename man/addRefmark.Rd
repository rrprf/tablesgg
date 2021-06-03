% Auto-generated documentation for function addRefmark 
%  2021-06-02 11:12:18 
\name{addRefmark}
\alias{addRefmark}
\title{Add a Reference Mark to Entries in a Table }
\description{
Add a reference mark (a symbol placed before or after entry text to
indicate cross-references; e.g. for footnotes) to entries in a table.
}
\usage{
addRefmark(x, mark, before=character(0), after=character(0), 
    parts=NULL, raise, ...)
}
\arguments{
  \item{x}{A \code{textTable} or \code{pltdTable} object.
  }
  \item{mark}{Character string containing the reference mark.
  }
  \item{before, after}{Character strings containing regular expressions (see \code{?regex}) that
will be matched against the _text_ of table entries (using \code{grepl}).
One or both of \code{before} and \code{after} may be specified.
  }
  \item{parts}{Optional character vector listing table parts.  If specified, only entries
in those parts will be matched against \code{before} and \code{after}.
The default is to use all table parts.
  }
  \item{raise}{Logical scalar.  If TRUE, the reference mark will be displayed as a
superscript, using \code{plotmath}.  The default is TRUE except for
asterisk marks, since that character is already raised relative to other
characters.
  }
  \item{...}{Additional arguments passed to \code{grepl} when matching \code{before}
and \code{after} to entry text.
  }
}
\value{
An object like \code{x}.  The text of table cells/entries selected by
\code{before} and \code{after} will be modified to include the reference
mark, and if \code{raise} is TRUE, those cells/entries will be flagged to
indicate that they should be treated as \code{plotmath} expressions.
}
\details{
Reference marks are placed at the beginning or end of an entry's text.  If
\code{raise} is TRUE they will be displayed as superscripts.  This is
implemented by converting the text to make use of R's \code{plotmath}
facility to create the superscript.  A limitation of \code{plotmath} is
that it ignores newline characters within text.  Therefore raised
reference marks will not work with multi-line entries, and a warning will
be issued.

In addition to using numbers, letters, or asterisk as reference marks,
traditional symbols can be specified by their unicode values: dagger
("\\u2020"), double dagger ("\\u2021"), paragraph symbol ("\\u00B6"),
section symbol ("\\u00A7"), and double vertical bars ("\\u2016").
However, unicode symbols may not be available for all OS's or graphics
devices.

With this function the user identifies the entries to be marked by
searching the entry text itself, via regular expressions \code{before}
and/or \code{after}.  For plotted tables (\code{pltdTable} objects), an
alternative way to add reference marks is to use one of the \code{props<-}
functions to assign an \code{element_refmark} to it.  They allow selection
of entries using other descriptors.
}
\seealso{
\code{\link{element_refmark}}, \code{\link{props<-}}
}
\examples{
# Add reference marks to a 'textTable':
ttbl <- textTable(iris2_tab, foot="sd = standard deviation")
ttbl <- addRefmark(ttbl, mark="a", before="sd =", after="sd$")
plot(ttbl)

# Add reference marks to a 'pltdTable':
plt <- plot(textTable(iris2_tab, foot="sd = standard deviation"))
plt <- addRefmark(plt, mark="*", before="sd =", after="sd$")
plt

# To add a reference mark to just the *first* appearance of "sd", use 
# 'propsa<-' instead:
plt <- plot(textTable(iris2_tab, foot="sd = standard deviation"))
plt <- addRefmark(plt, mark="a", before="sd =")
propsa(plt, arows=arow(plt, hpath=c("setosa", "sd")), 
       acols=acol(plt, "rowhead")[2]) <- element_refmark("a", side="after")
plt
  }
