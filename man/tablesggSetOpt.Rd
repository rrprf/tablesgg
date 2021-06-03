% Auto-generated documentation for function tablesggSetOpt 
%  2021-06-02 11:12:19 
\name{tablesggSetOpt}
\alias{tablesggSetOpt}
\title{Set the Values of Package Options }
\description{
Set the values of package options.
}
\usage{
tablesggSetOpt(...)
}
\arguments{
  \item{...}{Arguments in \code{tag = value} form, or a list of tagged values.  The
tags must come from the names of package options described in DETAILS
below.  The named options will be set to the values provided.
  }
}
\value{
A list with the old values of the named options, invisibly.
}
\details{
The new option values persist until the end of the R session or until they
are changed by another call to this function or to
\code{tablesggOpt(reset=TRUE)}.

The options that may be set are:

\describe{ \item{entryStyle}{A \code{styleObj} object, with element type
\code{entry}.  This is the default style for table entries.}
\item{blockStyle}{A \code{styleObj} object, with element type
\code{block}.  This is the default style for blocks.} \item{hvruleStyle}{A
\code{styleObj} object, with element type \code{hvrule}.  This is the
default style for hvrules.} \item{plot.margin}{A numeric vector of length
4, containing the amount of empty space to add around the four sides of a
plotted table, in millimeters.  The order of sides is top, right, bottom,
left.} \item{allowMarkdown}{Logical scalar.  If TRUE then text for table
entries is allowed to contain markdown and HTML tags to control its
appearance.  TRUE is valid only if package \code{ggtext} is available.}
\item{allowWrap}{Logical scalar.  If TRUE then automatic wrapping of text
for table entries is allowed.  TRUE is valid only if packages
\code{ggtext} and \code{quadprog} are available.} }

Facilities to handle markdown and automatic wrapping of entry text are
provided by Claus Wilke's \code{ggtext} package
(https://CRAN.R-project.org/package=ggtext).  Therefore
\code{allowMarkdown} and \code{allowWrap} can be set to TRUE only when
that package has been installed.  Note that only a subset of HTML tags are
available.
}
\seealso{
\code{\link{tablesggOpt}} to get current values of options, or to reset
options to their "factory-fresh" setting.  \code{\link{styles_pkg}} for
the set of package-provided table styles.
}
\examples{
  oldopt <- tablesggOpt()
  tablesggOpt(reset=TRUE)
  plt1 <- plot(iris2_tab, title="Factory-fresh default styles")
  # Set new default style for table entries:
  tablesggSetOpt(entryStyle=styles_pkg$entryStyle_pkg_2)
  plt2 <- plot(iris2_tab, title="Changed default entry style")
  # Compare:
  print(plt1, vpy=0.75)
  print(plt2, vpy=0.25, newpage=FALSE)
  
  # Change the values of multiple options:
  tablesggSetOpt(list(hvruleStyle=styles_pkg$hvruleStyle_pkg_base, 
                      plot.margin=c(5, 5, 5, 5)))
  # ... plot some tables using the new defaults ...
  # Restore the old options:
  tablesggSetOpt(oldopt)
  identical(tablesggOpt(), oldopt)
  }
