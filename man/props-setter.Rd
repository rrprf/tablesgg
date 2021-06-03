% Auto-generated documentation for function props<- 
%  2021-06-02 11:12:19 
\name{props<-}
\alias{props<-}
\title{Update Graphical Properties for Selected Table Elements }
\description{
Update the graphical properties for selected table entries, hvrules, or
blocks in a plotted table.
}
\usage{
props(x, id=NULL, regex=NULL, setEnabled=TRUE, mustMatch=TRUE, 
    ...) <- value
}
\arguments{
  \item{x}{A \code{pltdTable} object containing a plotted table.
  }
  \item{id}{Optional character vector of the ID's of the elements (or of table parts
containing the elements) to be modified.
  }
  \item{regex}{Optional character string containing a regular expression.  This will be
used to find table entries whose text matches the regular expression.
(Only valid when \code{value} is an \code{element_entry} or
\code{element_refmark} object.)
  }
  \item{setEnabled}{Logical scalar.  If TRUE then any element whose properties are updated by
this function will have its \code{enabled} value set to TRUE (and thus
will be displayed in a plot).  \code{enabled} will not be changed for
elements that are not updated.  If \code{setEnabled} is FALSE,
\code{enabled} is not changed for any elements.
  }
  \item{mustMatch}{Logical scalar.  If TRUE, any strings in argument \code{id} that do not
match an element or part ID in \code{x} will be treated as an error.
  }
  \item{...}{Additional arguments passed to \code{grepl} when \code{regex} is used to
select table entries.
  }
  \item{value}{An \code{element_entry}, \code{element_refmark}, \code{element_hvrule}, or
\code{element_block} object that contains the new values for graphical
properties.  See DETAILS.
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
the documentation of those functions for the available properties.  As an
example, \verb{element_entry(color="red", fontface=3, hpad=rel(0.8))}
specifies that all the updated entries will be displayed in red italics,
and padding on their left and right will be reduced to 80\% of the current
amount.  Any graphical properties not mentioned in the call that creates
the \code{element_*} object are left unchanged.

There are two special properties: \code{enabled} and \code{textspec} (the
latter only for entries).  \code{enabled} is a logical scalar.  If
\code{enabled} is set to FALSE the selected elements will not be
displayed, and disabled entries/hvrules will not be allocated any space in
the plotted table. \code{textspec} is a character string, one of "plain",
"plotmath", or "markdown".  If "plotmath", entry text will be treated as
an expression that allows display of mathematical symbols and notation,
including subscripts and superscripts (see \code{?plotmath}).  If
"markdown", entry text may contain markdown or HTML tags that affect the
appearance of the displayed text.  (This requires the \code{ggtext}
package, and only a subset of HTML tags is supported.  See the
\code{ggtext} package documentation for more information.)

\code{element_refmark} is used to add a reference mark to the selected
entries.  (A reference mark is a symbol placed before or after entry text
to indicate cross-references; e.g. for footnotes.)  It may also update any
of the graphical properties accepted by \code{element_entry}, except
\code{textspec} and \code{text}.

Arguments \code{id} and \code{regex} indicate which elements are to be
updated.  When \code{value} indicates that table entries are to be
modified, \code{id} may specify the ID's of individual entries or the ID's
of table parts or blocks.  In the latter case, all entries completely or
partially contained in the parts or blocks are selected.  Thus, for
example, specifying \code{id="table"} will update every entry in the
table, while \code{id="body"} will update only entries in the table body.
Argument \code{regex} is only valid for modifying table entries;
\code{grepl} is used to identify the entries whose text matches the
\code{regex} pattern.  If both \code{id} and \code{regex} are specified,
then only entries selected by both are modified.

When \code{value} indicates that hvrules are to be modified, \code{id}
should contain the ID's of individual hvrules and/or table blocks.  In the
latter case, any hvrule associated with a listed block (that is, the
hvrule was defined as running along one of the sides of the block) will be
updated.

When \code{value} indicates that blocks are to be modified, \code{id}
should contain the ID's of individual table blocks.

See \code{?elements} for the format of element ID strings.

This function overrides graphical properties in \code{x} that may have
been set by a style.  Therefore the value of \code{style_row} is set to NA
for any elements whose properties are updated by this function.
}
\seealso{
\code{\link{element_entry}}, \code{\link{element_refmark}},
\code{\link{element_hvrule}}, \code{\link{element_block}},
\code{\link{ids}}, \code{\link{propsa<-}}, \code{\link{propsd<-}}
}
\examples{
ttbl <- textTable(iris2_tab, title="The iris data", 
                  subtitle=c("Summary statistics by species", 
                             "A second subtitle line"), 
                  foot="sd = standard deviation")
plt <- plot(ttbl)
# Change properties of elements:
props(plt, id="body") <- element_entry(fontface=3, fill="gray85")
# This may include changing text:
props(plt, id="subtitle,2") <- element_entry(text="Properties changed by 'props<-'", 
                                             fill="gray85")
# Use property 'enabled' to control whether an element is displayed:
props(plt, id="rowhead_and_body_bottom") <- element_hvrule(enabled=FALSE)
plt

# Add reference marks to entries with abbreviation "sd":
plt <- plot(ttbl)
props(plt, regex="^sd$") <- element_refmark(mark="*", side="after")
props(plt, regex="^sd =") <- element_refmark(mark="*", side="before")
plt
# If both 'id' and 'regex' are specified only the intersection is modified:
plt <- plot(ttbl)
props(plt, regex="^sd$", id="rowblock/B/2/1") <- 
  element_refmark(mark="*", side="after")
props(plt, regex="^sd =") <- element_refmark(mark="*", side="before")
plt

  }
