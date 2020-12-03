\name{tablesgg-package}
\alias{tablesgg-package}
\alias{tablesgg}
\docType{package}
\title{Presentation-Quality Tables, Displayed Using \pkg{ggplot2}}
\description{
  Presentation-quality tables are displayed as plots on an R 
  graphics device.  Although there are other packages that format tables 
  for display, this package is unique in combining two features: (a) It is 
  aware of the logical structure of the table being presented, and makes 
  use of that for automatic layout and styling of the table.  This avoids 
  the need for most manual adjustments to achieve an attractive result. 
  (b) It displays tables using \pkg{ggplot2} graphics.  Therefore a table can 
  be presented anywhere a graph could be, with no more effort.  External 
  software such as LaTeX or HTML or their viewers is not required. 
  
  Methods are included to display data frames; tables created by R's 
  \code{ftable}, \code{table}, and \code{xtabs} functions; and tables 
  created by the \pkg{tables} and \pkg{xtable} packages.  Methods can be 
  added to display other table-like objects. 
  
  Other package features:
  \itemize{
    \item A full set of tools is provided to control the appearance 
of tables, including titles, footnotes and reference marks, horizontal 
and vertical rules, and spacing of rows and columns.  Many properties can be 
set automatically by specifying \emph{styles}.  Default styles are  
included, and the user can define custom styles.  
    \item There are tools for low-level manipulation of the appearance of 
individual table elements if desired.
    \item All sizes and dimensions in displayed tables are specified in physical 
units (points for font size, millimeters for everything else).  Therefore a 
plotted table has a well-defined physical size, independent of the size of 
the graphics device on which it is displayed.  The user can easily increase 
or decrease the displayed size by a scale factor, maintaining the relative 
proportions of table elements.
    \item Since the plotted tables are ordinary \code{ggplot} objects, 
the facilities of \pkg{ggplot2} and its various extension packages are 
available to modify or manipulate the table.  For example, the table can be 
inserted as an image within another plot.
  }
  
  A vignette is included that illustrates usage and options available in 
  the package.
}
