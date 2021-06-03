% Auto-generated documentation for function element_refmark 
%  2021-06-02 11:12:18 
\name{element_refmark}
\alias{element_refmark}
\title{Specify a Reference Mark that can be Added to Table Entries }
\description{
Specify a reference mark (a symbol placed before or after entry text to
indicate cross-references; e.g. for footnotes) that can be added to
entries in a table.
}
\usage{
element_refmark(mark=NULL, side=NULL, raise, ..., inherit.blank=FALSE)
}
\arguments{
  \item{mark}{Character string containing the character(s) to be used as the reference
mark.
  }
  \item{side}{Character string indicating where the reference mark is to be placed:
"before" or "after" the entry text.
  }
  \item{raise}{Logical scalar.  If TRUE, the reference mark will be displayed as a
superscript, using \code{plotmath}.  The default is TRUE except for
asterisk marks, since that character is already raised relative to other
characters.
  }
  \item{...}{Additional arguments passed to \code{element_entry}.  These can be used to
set other graphical properties of table entries at the same time as
setting the reference mark.  However it is an error to set \code{text} or
\code{textspec} to anything other than NULL, since they are used
internally by this function.
  }
  \item{inherit.blank}{Ignored.
  }
}
\value{
An object of S3 classes \code{element_refmark} and \code{element}.  If any
arguments are specified in \code{...}, they are passed to
\code{element_entry} and the resulting object is attached as attribute
\code{extra}.
}
\details{
This function is modeled on the \code{element_*} functions used in
\code{ggplot2} to specify graphical properties in themes.  It is primarily
used to create the value on the right-hand side of an assignment involving
the \code{props<-} group of setter functions.
}
\seealso{
\code{\link{addRefmark}} for a different way to add reference marks, and
for more information about reference marks in general.
\code{\link{element_entry}}, \code{\link{props<-}},
\code{\link{propsa<-}}, \code{\link{propsd<-}}.
}
\examples{
plt <- plot(iris2_tab, title="Summary statistics for the iris data", 
            foot="sd = standard deviation")
props(plt, id="foot") <- element_refmark(mark="*", side="before")
# Add a reference mark to just the first appearance of 'sd' in the row header:
propsa(plt, arows=arow(plt, hpath=c("setosa", "sd")), acols=2) <- 
       element_refmark(mark="*", side="after")
plt
  }
