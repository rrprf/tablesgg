# Presentation-Quality Tables, Displayed Using `ggplot2`

`tablesgg` is an R package that displays presentation-quality tables as 
plots on an R graphics device.  Although there are other packages that format 
tables for display, this package is unique in combining two features: 

* It is aware of the logical structure of the table being presented, and makes 
use of that for automatic layout and styling of the table.  This avoids 
the need for most manual adjustments to achieve an attractive result. 

* It displays tables using `ggplot2` graphics.  Therefore a table can 
be presented anywhere a graph could be, with no more effort.  External 
software such as LaTeX or HTML or their viewers is not required. 

Methods are included to display data frames; tables created by R's 
ftable(), table(), and xtabs() functions; and tables 
created by the `tables` and `xtable` packages.  Methods can be 
added to display other table-like objects. 

Other package features:

* A full set of tools is provided to control the appearance 
of tables, including titles, footnotes and reference marks, horizontal 
and vertical rules, and spacing of rows and columns.  Many properties can be 
set automatically by specifying _styles_.  Default styles are 
included, and the user can define custom styles.

* There are tools for low-level manipulation of the appearance of 
individual table elements if desired.

* All sizes and dimensions in displayed tables are specified in physical 
units (points for font size, millimeters for everything else).  Therefore a 
plotted table has a well-defined physical size, independent of the size of 
the graphics device on which it is displayed.  The user can easily increase 
or decrease the displayed size by a scale factor, maintaining the relative 
proportions of table elements.

* Since the plotted tables are ordinary `ggplot` objects, 
the facilities of `ggplot2` and its various extension packages are 
available to modify or manipulate the table.  For example, the table can be 
inserted as an image within another plot.

A vignette is included that illustrates usage and options available in 
the package.
