% Auto-generated documentation for function textTable.table 
%  2020-11-29 11:21:55 
\name{textTable.table}
\alias{textTable.table}
\title{Create a \code{texttable} from a \code{table} or \code{xtabs} Object }
\description{
Create a \code{textTable} object representing a flattened multiway
contingency table.
}
\usage{
\method{textTable}{table}(x, colheadLabels=c("layers", "none", "paste"), sep=": ", 
    title=character(0), subtitle=character(0), foot=character(0), ...)
}
\arguments{
  \item{x}{A \code{table} object, as produced by R's \code{table} or \code{xtabs}
functions, representing a multiway contingency table.
  }
  \item{colheadLabels}{Character scalar; how to display names of column header variables.  "none"
means to not display them.  "layers" (the default) means to display them
as additional column header layers (so each header variable occupies two
rows instead of one).  "paste" means to paste the variable name in front
of each of its values, separated by \code{sep}.
  }
  \item{sep}{Character scalar; string that separates a variable name from its values
when \code{colheadLabels} is "paste".
  }
  \item{title, subtitle, foot}{Optional character vectors providing annotation for the table.  May be
empty (i.e., \code{character(0)}, the default).
  }
  \item{...}{Additional arguments passed to \code{ftable}, to convert \code{x} to an
\code{ftable} object.
  }
}
\value{
An object with S3 class \code{textTable}.  See the documentation for the
generic for details about its structure.
}
\details{
This function simply converts \code{x} to an \code{ftable} (flattened
multiway contingency table), then applies the corresponding
\code{textTable} method to that object.

It also works for \code{xtabs} objects since they inherit from
\code{table}.
}
\seealso{
\code{ftable}, \code{xtabs}
}
\examples{
# UCBAdmissions is a contingency table in array form ('table' object).
data(UCBAdmissions, package="datasets")
ttbl <- textTable(UCBAdmissions)
plot(ttbl, title=c("Plotting a 'table' object:", "UCB Admissions data"))

# Method also works for 'xtabs' since they inherit from 'table' (example 
# from '?xtabs'):
data(warpbreaks, package="datasets")
warpbreaks$replicate <- rep_len(1:9, 54)
xt <- xtabs(breaks ~ wool + tension + replicate, data = warpbreaks)
ttbl <- textTable(xt, title="Plotting an 'xtabs' object (warpbreaks data)")
plot(ttbl)
  }
