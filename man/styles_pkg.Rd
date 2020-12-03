\name{styles_pkg}
\alias{styles_pkg}
\docType{data}
\title{Built-In Styles for Table Elements}
\description{
  A list that contains pre-defined styles for table entries, blocks, 
  and hvrules.  Styles are used to assign graphical properties to elements, 
  and thus control the appearance of a table when it is displayed.  In addition 
  to the package-provided styles in this list, users can modify or create 
  new styles to customize their tables.
}
\usage{styles_pkg}
\format{
  The list has the following components, each a \code{styleObj} object:
  \describe{
    \item{\code{entryStyle_pkg_base}, \code{blockStyle_pkg_base}, 
      \code{hvruleStyle_pkg_base}}{Minimal styles that assign 
      the same graphical properties to all elements.}
    \item{\code{entryStyle_pkg_1}, \code{blockStyle_pkg_1}, 
      \code{hvruleStyle_pkg_1}}{The default styles used by the package.}
    \item{\code{entryStyle_pkg_2}}{Similar to \code{entryStyle_pkg_1}, but 
      with \code{hpad} and \code{vpad} about 50 percent larger, to 
    give a more spacious layout of table entries.}
    \item{\code{entryStyle_pkg_null}, \code{blockStyle_pkg_null}, 
      \code{hvruleStyle_pkg_null}}{"Null" styles designed to not 
      match any element, and thus not assign graphical properties to any 
      element.  They are intended for internal use only.}
  }
}
\seealso{\code{\link{styleObj}}, \code{\link{tablesggOpt}}}
\examples{
  names(styles_pkg)  # built-in styles
  str(styles_pkg$entryStyle_pkg_1)
}
\keyword{datasets}
