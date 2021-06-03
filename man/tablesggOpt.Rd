% Auto-generated documentation for function tablesggOpt 
%  2021-06-02 11:12:19 
\name{tablesggOpt}
\alias{tablesggOpt}
\title{Get or Reset Package Options }
\description{
Get the values of package options, or reset all options to their
"factory-fresh" defaults.
}
\usage{
tablesggOpt(x=NULL, reset=FALSE)
}
\arguments{
  \item{x}{Character string with the name of a single package option, or NULL.
  }
  \item{reset}{Logical scalar.  If TRUE, all options will be reset to their initial,
"factory-fresh" values.
  }
}
\value{
If \code{x} is the name of a single package option, the value of that
option.  Otherwise, a named list with the current values of all package
options.  In both cases the result is after resetting if \code{reset} is
TRUE.

The result is invisible if \code{reset} is TRUE.

The available options are documented in \code{?tablesggSetOpt}.
}
\details{
The user can change option values using the \code{tablesggSetOpt}
function.  The new values will stay in effect for the rest of the R
session or until they are changed again by the user.
}
\seealso{
\code{\link{tablesggSetOpt}}, \code{\link{styles_pkg}}
}
\examples{
  # See names of available options:
  names(tablesggOpt())
  # The current value of option 'plot.margin':
  tablesggOpt("plot.margin")
  }
